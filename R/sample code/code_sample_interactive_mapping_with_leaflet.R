library(dplyr)

# all LSOA codes with population centroids (in easting and northings)
lsoa_centroids <- readr::read_csv("data/Lower_Layer_Super_Output_Areas_(December_2011)_Population_Weighted_Centroids.csv")

# keep English LSOAs only
Eng_lsoa_centroids <- lsoa_centroids %>% dplyr::filter(stringr::str_starts(lsoa11cd, "E"))

# transform CRS: From BNG (Easting / Northings) to WGS84 (lat/lng)

Eng_lsoa_centroids <- Eng_lsoa_centroids %>%
  # define coordinates as british national grid (eastings/northings)
  sf::st_as_sf(coords = c("X", "Y"), crs = 27700) %>%
  # transform to WGS84 (World Geodetic System) long /lat (add as 'geometry' list column)
  sf::st_transform(4326) 

# extract lat/lng columns
Eng_lsoa_centroids <- Eng_lsoa_centroids %>% 
  dplyr::mutate(locationlong = sf::st_coordinates(.)[,1],
                locationlat = sf::st_coordinates(.)[,2])

# Create a leaflet map with default map tile using addTiles()

m <- Eng_lsoa_centroids %>% 
  # plot only Liverpool LSOA centroids
  dplyr::filter(stringr::str_starts(lsoa11nm, "Liverpool")) %>% 
  leaflet::leaflet() %>%
  leaflet::addTiles() %>% 
  leaflet::addMarkers(
    lng = ~locationlong,
    lat = ~locationlat,
    popup = ~paste0("<b> lsoa11nm : ", lsoa11nm  , "</b>", "<br/>", 
                    "lsoa11cd: ", lsoa11cd , "<br/>", 
                    "coords: ", geometry),
    label = ~lsoa11nm)
m


# Use OS background mapping (by passing a OS url and api key object into addTiles())
#Define url templates for your favourite tile styles
source('R/os_hub_api_key_for_hackday20Jan2022.R') # script is just 1 line: Sys.setenv(OS_PROJECT_API_KEY = 'XXX')
OSRoad_3857 <- paste0("https://api.os.uk/maps/raster/v1/zxy/Road_3857/{z}/{x}/{y}.png?key=",Sys.getenv("OS_PROJECT_API_KEY"))


m_OS <- Eng_lsoa_centroids %>% 
  # plot only Liverpool LSOA centroids
  dplyr::filter(stringr::str_starts(lsoa11nm, "Liverpool")) %>% 
  leaflet::leaflet() %>%
  leaflet::addTiles(OSRoad_3857) %>% 
  leaflet::addMarkers(
    lng = ~locationlong,
    lat = ~locationlat,
    popup = ~paste0("<b> lsoa11nm : ", lsoa11nm  , "</b>", "<br/>", 
                    "lsoa11cd: ", lsoa11cd , "<br/>", 
                    "coords: ", geometry),
    label = ~lsoa11nm)

m_OS
