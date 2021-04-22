# Author: Erik Strand
# Version: 2021-03-13

library(tidyverse)
library(sf)
library(readxl)

chi_zip <- 
  read_sf("../data/final project/chicago_zip") %>% 
  select(zip = ZIP)

chi_ih_points_raw <- 
  read_excel("../data/final project/Chicago_IHsites_points.xlsx")

#<=============================================================================>

chi_ih_points <- 
  chi_ih_points_raw %>% 
  transmute(
    address = `Project Location`,
    on_site_units = `On-Site Units`, 
    off_site_units = `Off-Site Units`, 
    aff_units = on_site_units + off_site_units,
    x = `Latitude (generated)`,
    y = `Longitude (generated)`
  ) %>% 
  na_if(0) %>% 
  drop_na(aff_units) %>% 
  st_as_sf(coords = c("y", "x")) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = st_crs(chi_zip)) %>% 
  st_join(chi_zip) %>% 
  group_by(zip) %>% 
  summarize_at(vars(c(on_site_units:aff_units)), ~ sum(., na.rm = TRUE))

chi_ih_points_2015 <- 
  chi_ih_points_raw %>% 
  filter(`ARO Version` == "2015 ARO") %>% 
  transmute(
    address = `Project Location`,
    on_site_units = `On-Site Units`, 
    off_site_units = `Off-Site Units`, 
    aff_units = on_site_units + off_site_units,
    x = `Latitude (generated)`,
    y = `Longitude (generated)` 
  ) %>% 
  na_if(0) %>% 
  drop_na(aff_units) %>% 
  st_as_sf(coords = c("y", "x")) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = st_crs(chi_zip)) %>% 
  st_join(chi_zip) %>% 
  group_by(zip) %>% 
  summarize_at(vars(c(on_site_units:aff_units)), ~ sum(., na.rm = TRUE))


chi_ih_zip <- 
  chi_zip %>% 
  left_join(chi_ih_points %>% st_drop_geometry(), by = "zip") %>% 
  mutate(
    zip = as.integer(zip),
    perc_aff = 10, 
    ami = 60, 
    aff_units = ifelse(is.na(aff_units), 0, aff_units)
  )

chi_ih_zip_2015 <- 
  chi_zip %>% 
  left_join(chi_ih_points_2015 %>% st_drop_geometry(), by = "zip") %>% 
  mutate(
    zip = as.integer(zip),
    perc_aff = 10, 
    ami = 60, 
    aff_units = ifelse(is.na(aff_units), 0, aff_units)
  )

st_write(chi_ih_zip, "../data/final project/CLEAN_DATA/chicago/chicago.shp")
st_write(chi_ih_zip_2015, "../data/final project/CLEAN_DATA/chicago/chicago_2015.shp")

