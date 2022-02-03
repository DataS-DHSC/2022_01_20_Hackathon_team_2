required_packages <- c("sf", "tidyverse", "SpatialPosition", "terra", "leaflet","janitor","terra", "rgdal")

required_packages <- setdiff(required_packages, rownames(installed.packages())) 

message(paste0(c("Installing:", required_packages), collapse= "\n *"))
invisible(lapply(required_packages, function(x) suppressMessages(install.packages(x))))


library(sf)
library(tidyverse)
library(SpatialPosition)
library(tmap)
library(leaflet)
library(janitor)
library(terra)


############################################## OS api set up -------------
# if you don't have an API key from the OS ignore this part 
# you will have to modify the base map code below to default to Open Street map
Sys.setenv(OS_PROJECT_API_KEY = )

user <- Sys.getenv('USERNAME')

#User needs to have the "material to share with participants" folder as a subfolder of OneDrive Documents
file_path <- paste0("C:/Users/",user,"/OneDrive - Department of Health and Social Care/Documents/material to share with participants/data/")
                          


############################################## Load data -----------------
# carehome locations
carehomes <- readr::read_csv(paste0(file_path,"carehome_point_locations.csv")) %>%
  dplyr::select(-lat_long_coordinates) %>% 
  sf::st_as_sf(wkt = "bng_coordinates", crs = 27700)


# carehomes metadata
carehomes_metadata <- readr::read_csv(paste0(file_path,"carehome-meta-data.csv")) %>% 
  # clean up names using janitor
  janitor::clean_names() %>% 
  #subset columns that are used in analysis
  dplyr::select(location_id,
                care_homes_beds,
                location_local_authority,
                location_latest_overall_rating,
                service_type_care_home_service_with_nursing,
                service_type_care_home_service_without_nursing,
                service_type_domiciliary_care_service,
                service_user_band_dementia,
                service_user_band_learning_disabilities_or_autistic_spectrum_disorder,
                service_user_band_older_people)


# Local Authority Districts from ONS geography portal
lad <- sf::st_read("https://opendata.arcgis.com/datasets/ac4ad96a586b4e4bab306dd59eb09401_0.geojson") %>%
  #convert to BNG - not sure if this is needed but it makes sense to convert to a CRS that uses metres
  sf::st_transform(27700) %>% 
  dplyr::select(LAD21CD, LAD21NM)



############################################## Set up base mapping in leaflet -----------------
os_attribution <- "Contains OS data © Crown copyright [and database right] 2022"
os_api <- paste0("https://api.os.uk/maps/raster/v1/zxy/Light_3857/{z}/{x}/{y}.png?key=",Sys.getenv("OS_PROJECT_API_KEY"))


base_map <- leaflet::leaflet(options = leafletOptions(zoomControl = TRUE)) %>%  
  leaflet:: addTiles(urlTemplate = os_api,
                     attribution = os_attribution,
                     group = "Ordnance Survey") %>%
  #uncomment line below and comment out addTiles above if no API key is provided
  #leaflet::addTiles() %>% 
  leaflet::addProviderTiles("Esri.WorldImagery",
                            group = "Satellite") %>% 
  leaflet::addLayersControl(baseGroups = c("Base map", "Satellite"),
                            options = layersControlOptions(collapsed = FALSE)) 


############################################## Define study area  -----------------
# define Merseyside and create a boundary file for it
merseyside <- c("Sefton",
                "Wirral",
                "Liverpool",
                "St. Helens",
                "Knowsley")

# single polygon for merseyside
merseyside_polygon <- lad %>% 
  dplyr::filter(LAD21NM %in% merseyside) %>% 
  dplyr::summarise()


############################################## Clean up and enrich carehome data -----------------
carehomes <- carehomes %>% 
  dplyr::left_join(carehomes_metadata, 
                   by = c("cqclocationid" = "location_id")) %>% 
  # subset to Merseyside only
  dplyr::filter(location_local_authority %in% merseyside) %>% 
  # subset carehomes that cater to users with learning disabilities or autistic spectrum disorder
  dplyr::filter(service_user_band_learning_disabilities_or_autistic_spectrum_disorder == "Y") %>% 
  # calculate an attractivenss score which combines the capacity of each care home with its rating
  # this will give smaller but better scoring carehomes a greater pull when it comes to defining their service area.
  dplyr::mutate(
    # numeric value for each rating 
    Rating_Score = dplyr::case_when(
      location_latest_overall_rating == "Outstanding" ~ 1.4,
      location_latest_overall_rating == "Good" ~ 1,
      location_latest_overall_rating == "Requires improvement" ~ 0.6,
      location_latest_overall_rating == "Inadequate" ~ 0.1),
    # percent rank for number of beds
    Beds_Score = dplyr::percent_rank(care_homes_beds )
  )


# attractiveness 
carehomes <- carehomes %>% 
  mutate(Attractiveness = Rating_Score * Beds_Score)

carehomes <- carehomes %>% 
  mutate(location_latest_overall_rating=factor(location_latest_overall_rating, 
                                               levels = c('Outstanding','Good','Requires improvement', 'Inadequate'), ordered=TRUE))


############################################## Plot carehome data -----------------
base_map %>% 
  leaflet::addPolygons(data = sf::st_transform(merseyside_polygon, 4326), 
                       fillColor = "#80cdc1", 
                       color = "#E3773E",
                       fillOpacity = 0.5,
                       opacity = 1,
                       weight = 1) %>% 
  leaflet::addCircleMarkers(data = sf::st_transform(carehomes, 4326),
                            fillColor = "#d01c8b",
                            color = "white",
                            fillOpacity = 1,
                            weight = 0.5,
                            radius = 3)

# colour for rating 
factpal <- colorFactor(topo.colors(5), unique(carehomes$location_latest_overall_rating))

# Map of care homes sized by Number of beds and coloured by CQC rating
base_map %>% 
  addPolygons(data = st_transform(merseyside_polygon, 4326), 
              fillColor = "#80cdc1", 
              color = "#E3773E",
              fillOpacity = 0.5,
              opacity = 1,
              weight = 1,
              highlight = highlightOptions(
                weight = 4,
                color = "white",
                fillOpacity = 0.5,
                bringToFront = FALSE,
                sendToBack = TRUE)) %>% 
  addCircleMarkers(data = st_transform(carehomes, 4326),
                   fillColor = ~factpal(carehomes$location_latest_overall_rating),
                   #color = 
                   fillOpacity = 1,
                   weight = 0.5,
                   radius = 1.2*sqrt(carehomes$care_homes_beds),
                   popup = ~paste("<h3>Care homes</h3>",
                                  # "<strong>Lab Name:</strong>",lab_name,"<br>",
                                  "<strong>ID:</strong>",cqclocationid,"<br>",
                                  "<strong>N beds:</strong>",care_homes_beds,"<br>",
                                  "<strong>Rating:</strong>",location_latest_overall_rating,"<br>")) %>%
  addLegend(pal = factpal,
            values = carehomes$location_latest_overall_rating, 
            opacity = 0.7, title = NULL,
            position = "bottomright")




######## huff gravity model -------------

huff_750_2 <- huff(knownpts = carehomes,
                   varname = "Attractiveness",
                   typefct = "exponential", 
                   span = 750,
                   beta = 2,
                   resolution = 100, 
                   mask = merseyside_polygon, 
                   returnclass = "sf")


huff_1500_8 <- huff(knownpts = carehomes,
                    varname = "Attractiveness",
                    typefct = "exponential", 
                    span = 1500,
                    beta = 8,
                    resolution = 100, 
                    mask = merseyside_polygon, 
                    returnclass = "sf")


huff_750_1 <- huff(knownpts = carehomes,
                   varname = "Attractiveness",
                   typefct = "exponential", 
                   span = 750,
                   beta = 1,
                   resolution = 100, 
                   mask = merseyside_polygon, 
                   returnclass = "sf")


huff_1500_2 <- huff(knownpts = carehomes,
                    varname = "Attractiveness",
                    typefct = "exponential", 
                    span = 1500,
                    beta = 2,
                    resolution = 100, 
                    mask = merseyside_polygon, 
                    returnclass = "sf")


# Create a raster surface
huff_750_2_raster <- rasterHuff(x = huff_750_2, mask = merseyside_polygon)
huff_1500_8_raster <- rasterHuff(x = huff_1500_8, mask = merseyside_polygon)
huff_750_1_raster <- rasterHuff(x = huff_750_1, mask = merseyside_polygon)
huff_1500_2_raster <- rasterHuff(x = huff_1500_2, mask = merseyside_polygon)



##################### explore different settings -------------------------
tmap_mode("plot")
tmap_arrange(tm_shape(huff_750_2_raster) + tm_raster(title = "span = 750, beta = 2"), 
             tm_shape(huff_1500_8_raster) + tm_raster(title = "span = 1500, beta = 8"),
             tm_shape(huff_750_1_raster) + tm_raster(title = "span = 750, beta = 1"),
             tm_shape(huff_1500_2_raster) + tm_raster(title = "span = 1500, beta = 2")) 

# reclassify the huff_750_2_raster raster to a binary one 
reclass_mt <- matrix(c(0,0.1, NA,
                       0.1, 100, 1),
                     ncol = 3,
                     byrow = TRUE)

huff_750_2_raster_binary <- terra::classify(terra::rast(huff_750_2_raster), reclass_mt)

huff_750_2_polygon_binary <- as.polygons(huff_750_2_raster_binary) %>% 
  st_as_sf() %>% 
  filter(layer == 1) %>% 
  st_cast("POLYGON") %>% 
  mutate(market = letters[row_number()])


### Description from https://cran.r-project.org/web/packages/SpatialPosition/vignettes/SpatialPosition.html
#The Huff model is a relative version of the Reilly model: for a given point i and a given attraction center j, 
#the attraction force of j is divided by the sum of all the possible attraction centers that affect i. 
#The result my be understood as the probability to choose j among the set of possible destinations. 
#Computed on a regular fine grid of points i the model produces a raster representing probable catchment areas.

####### visualise "markets"

pal <- colorNumeric("viridis", values(huff_750_2_raster),
                    na.color = "transparent")
base_map %>% 
  addCircleMarkers(data = st_transform(carehomes, 4326),
                   fillColor = ~factpal(carehomes$location_latest_overall_rating),
                   #color = 
                   fillOpacity = 1,
                   weight = 0.5,
                   radius = 10*carehomes$Attractiveness,
                   popup = ~paste("<h3>Care homes</h3>",
                                  # "<strong>Lab Name:</strong>",lab_name,"<br>",
                                  "<strong>ID:</strong>",cqclocationid,"<br>",
                                  "<strong>N beds:</strong>",care_homes_beds,"<br>",
                                  "<strong>Rating:</strong>",location_latest_overall_rating,"<br>",
                                  "<strong>Attractiveness:</strong>",Attractiveness)) %>%
  addLegend(pal = factpal,
            values = carehomes$location_latest_overall_rating, 
            opacity = 0.7, title = NULL,
            position = "bottomright") %>% 
  addRasterImage(huff_750_2_raster, colors = pal, opacity = 0.5) %>% 
  addPolygons(data = st_transform(huff_750_2_polygon_binary, 4326),
              fillColor = NA, 
              color = "black",
              fillOpacity = 0,
              opacity = 1,
              weight = 3,
              label = paste("Market: ",huff_750_2_polygon_binary$market)
  ) %>% 
  addLegend(pal = pal, values = raster::values(huff_750_2_raster),
            title = "Probabilistic Catchment Areas \nof care homes (learning difficulties)")
