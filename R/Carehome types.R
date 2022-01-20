library(tidyverse)

# Read meta data
carehomeMeta <- read.csv2("Input/carehome-meta-data.csv",header = TRUE,sep = ",")

# Read location data
carehomeLocation <- read.csv2("Input/carehome_point_locations.csv",header = TRUE,sep = ",")


# Select only care homes, create new variable to identify non-nursing homes (over_65 includes all nursing and non-nursing)
# Merge meta data with location data
carehomeMerged <- carehomeMeta %>%
  filter(Care.home. == "Y") %>%
  mutate(non_nurs_over_65 = ifelse(over_65 == "Y" & nurs_over_65 == "", "Y", "")) %>%
  mutate(non_nurs_under_65 = ifelse(under_65 == "Y" & nurs_under_65 == "", "Y", "")) %>%
  select(ï..Location.ID, Location.Name, non_nurs_over_65, non_nurs_under_65,
          nurs_over_65, nurs_under_65, ldis_over_65, ldis_under_65, Service.user.band...Dementia) %>%
  left_join(., carehomeLocation, by = c("ï..Location.ID" = "cqclocationid"))

# Summary of care homes by types identified (different markets)
carehomeMerged %>% 
  group_by(non_nurs_over_65, non_nurs_under_65, over_65, under_65,ldis_over_65, ldis_under_65, nurs_over_65, nurs_under_65, Service.user.band...Dementia) %>%
  count()





## Over 65 = over_65
## Under 65 = under_65
## Nursing under 65 = nurs_under_65
## Nursing over 65 = nurs_over_65
## Over 65 Learning Dis # If do both under and over 65 (counted as over) = ldis_over_65
## Under 65 Learning Dis = ldis_under_65
## Dementia = Service.user.band...Dementia



