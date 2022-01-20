library(magrittr)
path <- "C:/Users/llaeber/Department of Health and Social Care/NW025 - 2021-02 DriveTime analysis carehomes/04 hackathon/material to share with participants/data/"


# read in location points -------------------------------------------------

carehome_points <- readr::read_csv(paste0(path, "carehome_point_locations.csv"))

# use either of the coordinate columns as geometry
# LNG/LAT
carehome_points_long_lat <- sf::st_as_sf(carehome_points, wkt = "lat_long_coordinates") %>% 
  dplyr::select(-bng_coordinates)
# BNG
carehome_points_bng <- sf::st_as_sf(carehome_points, wkt = "bng_coordinates")%>% 
  dplyr::select(-lat_long_coordinates)


# read in catchments ------------------------------------------------------

carehome_catchments <- readr::read_csv(paste0(path, "carehome_catchments.csv"))
head(carehome_catchments)

# LNG/LAT
carehome_catchments_long_lat <- sf::st_as_sf(carehome_catchments, wkt = "lat_long_coordinates") %>% 
  dplyr::select(-bng_coordinates)
# BNG
carehome_catchments_bng <- sf::st_as_sf(carehome_catchments, wkt = "bng_coordinates")%>% 
  dplyr::select(-lat_long_coordinates)


# combine points and catchments -------------------------------------------

carehome_points_and_catchments_long_lat <- carehome_catchments_long_lat %>% 
  dplyr::left_join(carehome_points_long_lat %>% 
                     dplyr::mutate(locationlong = sf::st_coordinates(.)[,1],
                                   locationlat = sf::st_coordinates(.)[,2]) %>% 
                     sf::st_drop_geometry(),
                   by = "cqclocationid")
  
carehome_points_and_catchments_long_lat

# mapping trial -----------------------------------------------------------

# Leaflet
map_points <- head(carehome_points_long_lat) %>% 
  leaflet::leaflet() %>%
  leaflet::addTiles() %>% # or add Ordnance Survey Tile style into brackets
  leaflet::addMarkers(label = ~cqclocationid)

map_points


map_catchments <- head(carehome_catchments_long_lat) %>% 
  leaflet::leaflet() %>%
  leaflet::addTiles() %>% 
  leaflet::addPolygons()

map_catchments

map_catchments_and_points <- head(carehome_points_and_catchments_long_lat) %>% 
  leaflet::leaflet() %>%
  leaflet::addTiles() %>% 
  leaflet::addPolygons() %>% 
  leaflet::addMarkers(
    lng = ~locationlong,
    lat = ~locationlat,
    label = ~cqclocationid,
    popup = ~paste0("<b> CQC Location ID : ", cqclocationid  , "</b>", "<br/>", 
                    "Lon: ", locationlong , "<br/>", 
                    "Lat: ", locationlat))

map_catchments_and_points


  
  

