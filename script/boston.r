# Author: Erik Strand
# Version: 2021-04-07

library(tidyverse)
library(sf)
library(ggmap)
library(lubridate)

boston_ih <- read_csv("../data/final project/Boston_IHsites.csv")

boston_zip <- 
  read_sf("../data/final project/boston_zip") %>% 
  transmute(zip = as.integer(ZIP5))

#<=============================================================================>

boston_ih_points <- 
  boston_ih %>% 
  filter(
    `Public/ Private` == "Private" | `Public/ Private` == "Public/Private"
  ) %>% 
  transmute(
    zip = as.integer(`Zip Code`), 
    tot_units = RentUnits, 
    aff_units = `Income- Restricted Rental`, 
    perc_aff = 13,
    ami = 70
  ) %>% 
  group_by(zip, perc_aff, ami) %>% 
  summarize_at(vars(tot_units:aff_units), ~ sum(.))

boston_ih_zip <- 
  boston_zip %>% 
  left_join(boston_ih_points, by = "zip") %>% 
  mutate(aff_units = ifelse(is.na(aff_units), 0, aff_units))

write_sf(boston_ih_zip, "../data/final project/CLEAN_DATA/boston/boston.shp")
