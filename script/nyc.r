# Author: Erik Strand
# Version: 2021-03-13

library(tidyverse)
library(sf)
library(ggmap)
library(lubridate)

api <- "AIzaSyBG4AWOB3g0oh4-DJmylPpUM4dQ6URBf5M"
register_google(key = api)

nyc_ih_raw <- read_csv("../data/final project/HPD Inclusionary Housing Sites.csv")
nyc_all_housing <- 
  read_csv("../data/final project/Housing_New_York_Units_by_Building.csv")
nyc_zip <- read_sf("../data/zipcode") %>% select(zip = MODZCTA)

#<=============================================================================>

nyc_ih_points <- 
  nyc_ih_raw %>% 
  left_join(nyc_all_housing, by = c("Project_ID" = "Project ID")) %>% 
  drop_na("Project Name") %>% 
  transmute(
    id = `Project_ID`, 
    street = Address,
    address = paste0(Address, ", ", Borough.x, ", NY, ", Postcode), 
    borough = `Borough.x`,
    date = mdy(Transfer_Date), 
    year = ifelse(is.na(date), 2021, year(date)),
    zipcode = Postcode, 
    x = Longitude, 
    y = Latitude, 
    eli = `Extremely Low Income Units`, 
    vli = `Very Low Income Units`, 
    li = `Low Income Units`, 
    mod = `Moderate Income Units`, 
    mid = `Middle Income Units`, 
    other = `Other Income Units`, 
    aff_units = `All Counted Units`, 
    perc_aff = ifelse(`Program_Type` == "VIH", 30, 25), 
    ami = ifelse(`Program_Type` == "VIH", 80, 60)
  ) %>% 
  filter(year > 2014)

test <- 
  nyc_ih_points %>% 
  filter(is.na(x)) %>% 
  mutate(
    x = geocode(address, output = "latlon", source = "google")[1], 
    y = geocode(address, output = "latlon", source = "google")[2]
  ) %>% 
  unique()

test <- 
  sjlabelled::remove_all_labels(test) %>% 
  rename(
    x = lon, 
    y = lat
  )

nyc_ih_points_zip <- 
  nyc_ih_points %>% 
  bind_rows(test) %>% 
  unique() %>% 
  drop_na(x) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = st_crs(nyc_zip)) %>% 
  st_join(nyc_zip) %>% 
  group_by(zip, perc_aff, ami) %>% 
  summarize_at(vars(c(eli:aff_units)), ~ sum(., na.rm = TRUE)) %>% 
  st_drop_geometry()

nyc_ih_zip <- 
  nyc_zip %>% 
  left_join(nyc_ih_points_zip, by = "zip") %>% 
  mutate(
    aff_units = ifelse(is.na(aff_units), 0, aff_units),
    zip = as.integer(zip)
  )

st_write(nyc_ih_zip, "../data/final project/CLEAN_DATA/nyc/nyc.shp")
