# Author: Erik Strand
# Version: 2021-03-17

library(tidyverse)
library(sf)

raw_chi <- read_csv("../data/final project/housing_chi_data.csv")
raw_nyc <- read_csv("../data/final project/housing_nyc_data.csv")
raw_sf <- read_csv("../data/final project/housing_sf_data.csv")
raw_boston <- read_csv("../data/final project/housing_boston_data.csv")
raw_dc <- read_csv("../data/final project/housing_dc_data.csv")

vars = 
  c(
    S2504_C05_001E = "rental_all",
    S2504_C05_009E = "rental_2014plus"
  )

chi_zip <- 
  read_sf("../data/final project/chicago_zip") %>% 
  select(zip = ZIP) %>% 
  pull(zip)

nyc_zip <- 
  read_sf("../data/zipcode") %>% 
  select(zip = MODZCTA) %>% 
  pull(zip)

dc_zip <- 
  read_sf("../data/final project/dc_zip") %>% 
  transmute(zip = ZCTA5CE10)  %>% 
  pull(zip)

boston_zip <- 
  read_sf("../data/final project/boston_zip") %>% 
  transmute(zip = as.integer(ZIP5))  %>% 
  pull(zip)

sf_zip <- 
  read_sf("../data/final project/bay_zip") %>% 
  filter(
    po_name == "SAN FRANCISCO",
    zip != "94128" #SFO airport
  ) %>% 
  transmute(zip = as.integer(zip))  %>% 
  pull(zip)

#<=============================================================================>

# CHICAGO 

raw_chi %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars)) %>% 
  mutate(
    variable = vars[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% chi_zip) %>% 
  write_csv("../data/final project/CLEAN_DATA/chi_housing.csv")


# NEW YORK CITY

raw_nyc %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars)) %>% 
  mutate(
    variable = vars[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% nyc_zip) %>% 
  write_csv("../data/final project/CLEAN_DATA/nyc_housing.csv")


# SAN FRANCISCO 

raw_sf %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars)) %>% 
  mutate(
    variable = vars[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% sf_zip) %>% 
  write_csv("../data/final project/CLEAN_DATA/sf_housing.csv")


# BOSTON

raw_boston %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars)) %>% 
  mutate(
    variable = vars[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% boston_zip) %>% 
  write_csv("../data/final project/CLEAN_DATA/boston_housing.csv")


# WASHINGTON D.C. 

raw_dc %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars)) %>% 
  mutate(
    variable = vars[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% dc_zip) %>% 
  write_csv("../data/final project/CLEAN_DATA/dc_housing.csv")
