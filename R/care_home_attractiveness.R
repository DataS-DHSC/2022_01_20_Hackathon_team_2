### Hackathon - 20/01/22
### Challenge 2: Identifying Local Care Markets

if (!require("DBI")) install.packages("DBI")
library(DBI)
if (!require("odbc")) install.packages("odbc")
library(odbc)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("reshape"))install.packages("reshape")
library(reshape2)
if (!require("zoo")) install.packages("zoo")
library(zoo)
library(lubridate)
library(scales)
library(ggplot2)
if (!require("ggpubr")) install.packages("ggpubr")
library(ggpubr)
if (!require("gridExtra")) install.packages("gridExtra")
library(gridExtra)
if (!require("svglite")) install.packages("svglite")
library(svglite)
if (!require("writexl")) install.packages("writexl")
library(writexl)
if (!require("readxl")) install.packages("readxl")
library(readxl)
if (!require("slider")) install.packages("slider")
library(slider)

# create graph theme
# note: 'sans' in R refers to the font 'Arial' in Windows
theme_dft <- ggplot2::theme(axis.text = element_text(family = "sans", size = 10, colour = "black"), # axis text
                            axis.title.x = element_text(family = "sans", size = 14, colour = "black", # x axis title
                                                        margin = margin(t = 10)),
                            axis.title.y = element_text(family = "sans", size = 14, colour = "black", # y axis title
                                                        margin = margin(t = 10)),
                            plot.title = element_text(size = 16, family = "sans", hjust = 0.5),
                            plot.subtitle = element_text(size = 14, colour = "black", hjust = -0.05,
                                                         margin = margin(t = 10)),           
                            legend.key = element_blank(), # make the legend background blank
                            legend.position = "bottom", # legend at the bottom
                            legend.direction = "horizontal", # legend horizontal
                            legend.title = element_blank(), # remove legend title
                            legend.text = element_text(size = 10, family = "sans"),
                            axis.ticks = element_blank(), # remove tick marks
                            panel.background = element_blank(), # remove background
                            panel.grid.major.y = element_line(color = "grey80"),
                            axis.line.x = element_line(size = 0.3, colour = "grey"),
                            axis.line.y = element_line(size = 0.3, colour = "grey"))

# Set graph colours
dhsc_colours <- c( "#00ad93","#E57200","#616265", "#101820", "#34b6e4", "#512698")

dhsc_green <- dhsc_colours[1]
dhsc_orange <- dhsc_colours[2]
dhsc_grey <- dhsc_colours[3]
dhsc_black <- dhsc_colours[4]
dhsc_blue <- dhsc_colours[5]
dhsc_purple <- dhsc_colours[6]

# Creating username
setwd("~/") #sets working directory to default (which will contain username)
user <- stringr::str_sub(getwd(),
                         str_locate_all(getwd(), pattern = "/") %>% .[[1]] %>% .[c(2),1] + 1,
                         str_locate_all(getwd(), pattern = "/") %>% .[[1]] %>% .[c(3),1] - 1) #returns username

# set input_path
input_path<- paste0("C:/Users/",user,"/OneDrive - Department of Health and Social Care/Documents/Coding/material to share with participants")
setwd(input_path)

# read in data --------------------------------------------------------------------------
# carehome point locations
CH_point_locations <- read.csv(paste0(input_path,"/data/carehome_point_locations.csv"))

# care home metadata
carehome_metadata <- read.csv(paste0(input_path,"/data/carehome-meta-data.csv"))

# output tables
merseyside_results_table_10km <- read.csv(paste0(input_path,"/data/output tables/merseyside_results_table_10km.csv"))

aggregate_results_national <- read.csv(paste0(input_path,"/data/output tables/aggregate_results_national.csv"))

agg_results_merseyside_10km_25km <- read.csv(paste0(input_path,"/data/output tables/agg_results_merseyside_10km_25km.csv"))

# LSOA_pwc_points
LSOA_pwc_points <- read.csv(paste0(input_path,"/data/LSOA_pwc_points/Lower_Layer_Super_Output_Areas_(December_2011)_Population_Weighted_Centroids.csv"))


# Separate merseyside from care home metadata
merseyside_CH_metadata <- carehome_metadata %>% 
  select(ï..Location.ID:Location.Local.Authority) %>% 
  filter(Location.Local.Authority %in% c("Liverpool",
                                         "Wirral",
                                         "Sefton",
                                         "Knowsley",
                                         "St. Helens")) %>% 
  filter(Care.home. == "Y",
         !is.na(Location.Latest.Overall.Rating),
         Location.Latest.Overall.Rating != "")

# convert ratings to numbers
merseyside_CH_metadata <- merseyside_CH_metadata %>% 
  mutate(Rating.Score = case_when(
    Location.Latest.Overall.Rating == "Outstanding" ~ 1.4,
    Location.Latest.Overall.Rating == "Good" ~ 1,
    Location.Latest.Overall.Rating == "Requires improvement" ~ 0.6,
    Location.Latest.Overall.Rating == "Inadequate" ~ 0.1)
  )

# number of care home beds 
#range(merseyside_CH_metadata$Care.homes.beds)

# percentile for care home beds
merseyside_CH_metadata <- merseyside_CH_metadata %>% 
  mutate(Beds.Score = percent_rank(Care.homes.beds))

# attractiveness 
merseyside_CH_metadata <- merseyside_CH_metadata %>% 
  mutate(Attractiveness = Rating.Score * Beds.Score)

# order for histogram
order<- c('Outstanding','Good','Requires improvement', 'Inadequate')

merseyside_CH_metadata<-merseyside_CH_metadata%>%mutate(Location.Latest.Overall.Rating=factor(Location.Latest.Overall.Rating, 
                                                                                              levels=order, ordered=TRUE))

# histogram
merseyside_CH_metadata%>%ggplot(aes(Attractiveness))+
  geom_histogram()+
  facet_wrap(~Location.Latest.Overall.Rating, ncol=1)+
  scale_y_continuous(name = 'Number of care homes')+
  labs(title=paste0("Number of care homes by attractiveness"))+
  theme_dft


merseyside_CH_metadata%>%ggplot(aes(Care.homes.beds,Attractiveness, colour=Location.Latest.Overall.Rating))+
  geom_point()+
  labs(title=paste0("Number of care homes beds by attractiveness"), x="Care home beds")+
  theme_dft






