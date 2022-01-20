# 1. Load packages ----------------------------------------------------------------

library(sf)
library(dplyr)

# 2. Read lsoa data in -------------------------------------------------------

# all LSOA codes with population centroids (in easting and northings)
lsoa_centroids <- readr::read_csv("data/Lower_Layer_Super_Output_Areas_(December_2011)_Population_Weighted_Centroids.csv") %>% 
  # keep English LSOAs only
  dplyr::filter(stringr::str_starts(lsoa11cd, "E"))

# 3. Tranform CRS from BNG easting and northings to Longitude and  --------

lsoa_centroids <- lsoa_centroids %>%
  # define coordinates as british national grid (eastings/northings)
  sf::st_as_sf(coords = c("X", "Y"), crs = 27700) %>%
  # transform to WGS84 (World Geodetic System) long /lat (add as 'geometry' list column)
  sf::st_transform(4326) 


# 4. Optional: Extract long & lat as individual columns -------------------

# extract lat/lng as separate columns from geometry column
lsoa_centroids <- lsoa_centroids %>% 
  dplyr::mutate(locationlong = sf::st_coordinates(.)[,1],
                locationlat = sf::st_coordinates(.)[,2])