# OS background mapping API
# sample code from: https://github.com/howaskew/OSapiR
source('R/os_hub_api_key_for_hackday20Jan2022.R') # script is just 1 line: Sys.setenv(OS_PROJECT_API_KEY = 'XXX')


# 1. Basic background map ----------------------------------------------------

# Basic example: Adding backdrops or base map tiles to leaflet maps
library(leaflet)

m <- leaflet() %>%
  addTiles(paste0("https://api.os.uk/maps/raster/v1/zxy/Light_3857/{z}/{x}/{y}.png?key=",Sys.getenv("OS_PROJECT_API_KEY"))) %>% 
  addMarkers(lng=-1.470770, lat=50.938039, popup="Explorer House, home of Ordnance Survey")
m  # Print the map


# 2. Different styles of background maps ----------------------------------

# Varying map tile styles
#Define url templates for the different tile styles
OSLight_3857 <- paste0("https://api.os.uk/maps/raster/v1/zxy/Light_3857/{z}/{x}/{y}.png?key=",Sys.getenv("OS_PROJECT_API_KEY"))
OSRoad_3857 <- paste0("https://api.os.uk/maps/raster/v1/zxy/Road_3857/{z}/{x}/{y}.png?key=",Sys.getenv("OS_PROJECT_API_KEY"))
OSOutdoor_3857 <- paste0("https://api.os.uk/maps/raster/v1/zxy/Outdoor_3857/{z}/{x}/{y}.png?key=",Sys.getenv("OS_PROJECT_API_KEY"))

#Pass these templates to the addTiles() function
m <- leaflet() %>%
  addTiles(OSLight_3857) %>% 
  addMarkers(lng=-1.470770, lat=50.938039, popup="Explorer House, home of Ordnance Survey")
m  # Print the map

m <- leaflet() %>%
  addTiles(OSRoad_3857) %>% 
  addMarkers(lng=-1.470770, lat=50.938039, popup="Explorer House, home of Ordnance Survey")
m  # Print the map

m <- leaflet() %>%
  addTiles(OSOutdoor_3857) %>% 
  addMarkers(lng=-1.470770, lat=50.938039, popup="Explorer House, home of Ordnance Survey")
m  # Print the map


# 3. Changing the map projection ------------------------------------------
# Leaflet expects all point, line, and shape data to be specified in latitude and longitude using WGS 84 (a.k.a. EPSG:4326).
# By default, when displaying this data it projects everything to EPSG:3857 and expects that any map tiles are also displayed in EPSG:3857.
# However, you can use custom projections via the integrated Proj4Leaflet plugin.

#Define a Proj4Leaflet coordinate reference system (crs) instance configured for British National Grid (EPSG:27700) and the resolutions of our base map
crs <- leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:27700",
                  proj4def = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs',
                  resolutions = c(896.0, 448.0, 224.0, 112.0, 56.0, 28.0, 14.0, 7.0, 3.5, 1.75 ),
                  origin = c(-238375.0, 1376256.0)
)

#Define url templates for the different tile styles in EPSG:27700 
OSRoad_27700 <- paste0("https://api.os.uk/maps/raster/v1/zxy/Road_27700/{z}/{x}/{y}.png?key=",Sys.getenv("OS_PROJECT_API_KEY"))
OSLight_27700 <- paste0("https://api.os.uk/maps/raster/v1/zxy/Light_27700/{z}/{x}/{y}.png?key=",Sys.getenv("OS_PROJECT_API_KEY"))
OSOutdoor_27700 <- paste0("https://api.os.uk/maps/raster/v1/zxy/Outdoor_27700/{z}/{x}/{y}.png?key=",Sys.getenv("OS_PROJECT_API_KEY"))
OSLeisure_27700 <- paste0("https://api.os.uk/maps/raster/v1/zxy/Leisure_27700/{z}/{x}/{y}.png?key=",Sys.getenv("OS_PROJECT_API_KEY"))

#Pass these templates to the addTiles() function, along with the new crs
m <- leaflet(options=leafletOptions(crs=crs)) %>%
  addTiles(OSRoad_27700) %>% 
  addMarkers(lng=-1.470770, lat=50.938039, popup="Explorer House, home of Ordnance Survey")
m  # Print the map

m <- leaflet(options=leafletOptions(crs=crs)) %>%
  addTiles(OSOutdoor_27700) %>% 
  addMarkers(lng=-1.470770, lat=50.938039, popup="Explorer House, home of Ordnance Survey")
m  # Print the map

m <- leaflet(options=leafletOptions(crs=crs)) %>%
  addTiles(OSLight_27700) %>% 
  addMarkers(lng=-1.470770, lat=50.938039, popup="Explorer House, home of Ordnance Survey")
m  # Print the map



