# Author: Erik Strand
# Version: 2021-04-07

library(tidyverse)
library(sf)
library(ggmap)
library(lubridate)

dc_ih <- 
  read_sf("../data/final project/DC_IHsites")

dc_zip <- 
  read_sf("../data/final project/dc_zip") %>% 
  transmute(zip = ZCTA5CE10)

#<=============================================================================>

dc_ih_points <-  
  dc_ih %>% 
  filter(
    str_detect(AGENCY_CAL, "DHCD")
  ) %>% 
  transmute(
    address = ADDRESS, 
    aff_units = TOTAL_AFFO,
    perc_aff = 10, 
    ami = 80
  ) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = st_crs(dc_zip)) %>% 
  st_join(dc_zip) %>% 
  group_by(zip, perc_aff, ami) %>% 
  summarize(aff_units = sum(aff_units, na.rm = TRUE)) %>% 
  drop_na(zip)

dc_ih_zip <- 
  dc_zip %>% 
  left_join(dc_ih_points, by = "zip") %>% 
  mutate(
    aff_units = ifelse(is.na(aff_units), 0, aff_units),
    zip = as.integer(zip)
  )

st_write(dc_ih_zip, "../data/final project/CLEAN_DATA/dc/dc.shp")
