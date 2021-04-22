# Author: Erik Strand
# Version: 2021-04-07

library(tidyverse)
library(sf)
library(ggmap)
library(lubridate)

sf_ih <- 
  read_csv("../data/final project/SF_IHsites.csv")

sf_zip <- 
  read_sf("../data/final project/bay_zip") %>% 
  filter(
    po_name == "SAN FRANCISCO",
    zip != "94128" #SFO airport
  ) %>% 
  transmute(zip = as.integer(zip))

#<=============================================================================>

sf_ih_points <- 
  sf_ih %>% 
  drop_na(Latitude) %>% 
  st_as_sf(coords = c("Longitude", "Latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = st_crs(sf_zip)) %>% 
  mutate(
    date = mdy(`Actual/Estimated Completion Date`), 
    year = year(date)
  ) %>% 
  # filter(year >= 2015) %>% 
  transmute(
    zip = `Zip Code`,
    address = 
      paste0(`Street Number`, " ", `Street Name`, " ", `Street Type`, ", San Francisco, CA, ", zip), 
    tot_units = `Project Units`, 
    aff_units = `Affordable Units`, 
    onsite_aff = `On-Site Affordable Units`, 
    year = year, 
    perc_aff = ifelse(tot_units > 24, 18, 12),
    ami = ifelse(tot_units > 24, 72, 55)
  ) %>% 
  st_drop_geometry() %>% 
  group_by(zip, perc_aff, ami) %>% 
  summarize_at(vars(c(tot_units:onsite_aff)), ~ sum(., na.rm = TRUE))

sf_ih_zip <- 
  sf_zip %>% 
  left_join(sf_ih_points, by = "zip") %>% 
  mutate(
    aff_units = ifelse(is.na(aff_units), 0, aff_units),
    zip = as.integer(zip)
  )

st_write(sf_ih_zip, "../data/final project/CLEAN_DATA/sf/sf.shp")
