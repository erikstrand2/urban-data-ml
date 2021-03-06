# Author: Erik Strand
# Version: 2021-03-17

library(tidyverse)
library(sf)
library(readr)

raw_inc_nyc <- read_csv("../data/final project/income_nyc_data.csv")
raw_si_nyc <- read_csv("../data/final project/si_nyc_data.csv")

raw_inc_chi <- read_csv("../data/final project/income_chi_data.csv")
raw_si_chi <- read_csv("../data/final project/si_chi_data.csv")

raw_inc_sf <- read_csv("../data/final project/income_sf_data.csv")
raw_si_sf <- read_csv("../data/final project/si_sf_data.csv")

raw_inc_boston <- read_csv("../data/final project/income_boston_data.csv")
raw_si_boston <- read_csv("../data/final project/si_boston_data.csv")

raw_inc_dc <- read_csv("../data/final project/income_dc_data.csv")
raw_si_dc <- read_csv("../data/final project/si_dc_data.csv")

vars_inc <- 
  c(
    S1903_C03_015E = "med_hh_inc"
  )

vars_si <- 
  c(
    DP05_0001E = "pop",
    DP05_0018E = "med_age", 
    DP05_0019E = "youth_pop", 
    DP05_0024E = "elderly_pop", 
    DP05_0037E = "white_pop", 
    DP05_0038E = "black_pop", 
    DP05_0039E = "native_pop", 
    DP05_0044E = "asian_pop", 
    DP05_0052E = "pacisl_pop", 
    DP05_0057E = "other_race_pop", 
    DP05_0058E = "multiracial_pop", 
    DP05_0071E = "latinx_pop",
    DP05_0086E = "housing_total", 
    DP05_0087E = "citizen_pop"
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
  transmute(zip = ZCTA5CE10) %>% 
  pull(zip)

boston_zip <- 
  read_sf("../data/final project/boston_zip") %>% 
  transmute(zip = as.integer(ZIP5)) %>% 
  pull(zip)

sf_zip <- 
  read_sf("../data/final project/bay_zip") %>% 
  filter(
    po_name == "SAN FRANCISCO",
    zip != "94128" #SFO airport
  ) %>% 
  transmute(zip = as.integer(zip)) %>% 
  pull(zip)

#<=============================================================================>

# NEW YORK CITY

nyc_inc <- 
  raw_inc_nyc %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars_inc)) %>% 
  mutate(
    variable = vars_inc[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% nyc_zip) %>% 
  mutate_at(
    vars(med_hh_inc), 
    ~ parse_number(.)
  )

nyc_si <- 
  raw_si_nyc %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars_si)) %>% 
  mutate(
    variable = vars_si[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% nyc_zip) %>% 
  mutate_at(vars(pop:citizen_pop), as.integer) %>% 
  mutate(
    youth_perc = youth_pop / pop, 
    elderly_perc = elderly_pop / pop, 
    white_perc = white_pop / pop, 
    black_perc = black_pop / pop,
    native_perc = native_pop / pop,
    asian_perc = asian_pop / pop, 
    pacisl_perc = pacisl_pop / pop, 
    other_race_perc = other_race_pop / pop, 
    multiracial_perc = multiracial_pop / pop, 
    latinx_perc = latinx_pop / pop, 
    citizen_perc = citizen_pop / pop
  ) %>% 
  select(
    zip, 
    pop, 
    med_age,
    youth_pop, 
    youth_perc, 
    elderly_pop, 
    elderly_perc, 
    white_pop, 
    white_perc, 
    black_pop,
    black_perc, 
    asian_pop, 
    asian_perc, 
    pacisl_pop, 
    pacisl_perc, 
    other_race_pop, 
    other_race_perc, 
    multiracial_pop, 
    multiracial_perc, 
    latinx_pop, 
    latinx_perc, 
    citizen_pop, 
    citizen_perc, 
    housing_total
  )
  
nyc_inc %>% 
  left_join(nyc_si, by = "zip") %>% 
  write_csv("../data/final project/CLEAN_DATA/nyc_si.csv")


# CHICAGO
  
chi_inc <- 
  raw_inc_chi %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars_inc)) %>% 
  mutate(
    variable = vars_inc[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% chi_zip) %>% 
  mutate_at(
    vars(med_hh_inc), 
    ~ parse_number(.)
  )

chi_si <- 
  raw_si_chi %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars_si)) %>% 
  mutate(
    variable = vars_si[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% chi_zip) %>% 
  mutate_at(vars(pop:citizen_pop), as.integer) %>% 
  mutate(
    youth_perc = youth_pop / pop, 
    elderly_perc = elderly_pop / pop, 
    white_perc = white_pop / pop, 
    black_perc = black_pop / pop,
    native_perc = native_pop / pop,
    asian_perc = asian_pop / pop, 
    pacisl_perc = pacisl_pop / pop, 
    other_race_perc = other_race_pop / pop, 
    multiracial_perc = multiracial_pop / pop, 
    latinx_perc = latinx_pop / pop, 
    citizen_perc = citizen_pop / pop
  ) %>% 
  select(
    zip, 
    pop, 
    med_age,
    youth_pop, 
    youth_perc, 
    elderly_pop, 
    elderly_perc, 
    white_pop, 
    white_perc, 
    black_pop,
    black_perc, 
    asian_pop, 
    asian_perc, 
    pacisl_pop, 
    pacisl_perc, 
    other_race_pop, 
    other_race_perc, 
    multiracial_pop, 
    multiracial_perc, 
    latinx_pop, 
    latinx_perc, 
    citizen_pop, 
    citizen_perc, 
    housing_total
  )

chi_inc %>% 
  left_join(chi_si, by = "zip") %>% 
  write_csv("../data/final project/CLEAN_DATA/chi_si.csv")
  

# SAN FRANCISCO 

sf_inc <- 
  raw_inc_sf %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars_inc)) %>% 
  mutate(
    variable = vars_inc[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% sf_zip) %>% 
  mutate_at(
    vars(med_hh_inc), 
    ~ parse_number(.)
  )

sf_si <- 
  raw_si_sf %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars_si)) %>% 
  mutate(
    variable = vars_si[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% sf_zip) %>% 
  mutate_at(vars(pop:citizen_pop), as.integer) %>% 
  mutate(
    youth_perc = youth_pop / pop, 
    elderly_perc = elderly_pop / pop, 
    white_perc = white_pop / pop, 
    black_perc = black_pop / pop,
    native_perc = native_pop / pop,
    asian_perc = asian_pop / pop, 
    pacisl_perc = pacisl_pop / pop, 
    other_race_perc = other_race_pop / pop, 
    multiracial_perc = multiracial_pop / pop, 
    latinx_perc = latinx_pop / pop, 
    citizen_perc = citizen_pop / pop
  ) %>% 
  select(
    zip, 
    pop, 
    med_age,
    youth_pop, 
    youth_perc, 
    elderly_pop, 
    elderly_perc, 
    white_pop, 
    white_perc, 
    black_pop,
    black_perc, 
    asian_pop, 
    asian_perc, 
    pacisl_pop, 
    pacisl_perc, 
    other_race_pop, 
    other_race_perc, 
    multiracial_pop, 
    multiracial_perc, 
    latinx_pop, 
    latinx_perc, 
    citizen_pop, 
    citizen_perc, 
    housing_total
  )

sf_inc %>% 
  left_join(sf_si, by = "zip") %>% 
  write_csv("../data/final project/CLEAN_DATA/sf_si.csv")


# BOSTON

boston_inc <- 
  raw_inc_boston %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars_inc)) %>% 
  mutate(
    variable = vars_inc[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% boston_zip) %>% 
  mutate_at(
    vars(med_hh_inc), 
    ~ parse_number(.)
  )

boston_si <- 
  raw_si_boston %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars_si)) %>% 
  mutate(
    variable = vars_si[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% boston_zip) %>% 
  mutate_at(vars(pop:citizen_pop), as.integer) %>% 
  mutate(
    youth_perc = youth_pop / pop, 
    elderly_perc = elderly_pop / pop, 
    white_perc = white_pop / pop, 
    black_perc = black_pop / pop,
    native_perc = native_pop / pop,
    asian_perc = asian_pop / pop, 
    pacisl_perc = pacisl_pop / pop, 
    other_race_perc = other_race_pop / pop, 
    multiracial_perc = multiracial_pop / pop, 
    latinx_perc = latinx_pop / pop, 
    citizen_perc = citizen_pop / pop
  ) %>% 
  select(
    zip, 
    pop, 
    med_age,
    youth_pop, 
    youth_perc, 
    elderly_pop, 
    elderly_perc, 
    white_pop, 
    white_perc, 
    black_pop,
    black_perc, 
    asian_pop, 
    asian_perc, 
    pacisl_pop, 
    pacisl_perc, 
    other_race_pop, 
    other_race_perc, 
    multiracial_pop, 
    multiracial_perc, 
    latinx_pop, 
    latinx_perc, 
    citizen_pop, 
    citizen_perc, 
    housing_total
  )

boston_inc %>% 
  left_join(boston_si, by = "zip") %>% 
  write_csv("../data/final project/CLEAN_DATA/boston_si.csv")


# WASHINGTON D.C. 

dc_inc <- 
  raw_inc_dc %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars_inc)) %>% 
  mutate(
    variable = vars_inc[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% dc_zip) %>% 
  mutate_at(
    vars(med_hh_inc), 
    ~ parse_number(.)
  )

dc_si <- 
  raw_si_dc %>% 
  filter(GEO_ID != "id") %>% 
  select(-NAME) %>% 
  pivot_longer(
    -GEO_ID, 
    names_to = "variable", 
    values_to = "estimate"
  ) %>% 
  filter(variable %in% names(vars_si)) %>% 
  mutate(
    variable = vars_si[variable], 
    zip = substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID)) %>% as.integer()
  ) %>% 
  select(-GEO_ID) %>% 
  pivot_wider(
    names_from = variable, 
    values_from = estimate
  ) %>% 
  filter(zip %in% dc_zip) %>% 
  mutate_at(vars(pop:citizen_pop), as.integer) %>% 
  mutate(
    youth_perc = youth_pop / pop, 
    elderly_perc = elderly_pop / pop, 
    white_perc = white_pop / pop, 
    black_perc = black_pop / pop,
    native_perc = native_pop / pop,
    asian_perc = asian_pop / pop, 
    pacisl_perc = pacisl_pop / pop, 
    other_race_perc = other_race_pop / pop, 
    multiracial_perc = multiracial_pop / pop, 
    latinx_perc = latinx_pop / pop, 
    citizen_perc = citizen_pop / pop
  ) %>% 
  select(
    zip, 
    pop, 
    med_age,
    youth_pop, 
    youth_perc, 
    elderly_pop, 
    elderly_perc, 
    white_pop, 
    white_perc, 
    black_pop,
    black_perc, 
    asian_pop, 
    asian_perc, 
    pacisl_pop, 
    pacisl_perc, 
    other_race_pop, 
    other_race_perc, 
    multiracial_pop, 
    multiracial_perc, 
    latinx_pop, 
    latinx_perc, 
    citizen_pop, 
    citizen_perc, 
    housing_total
  )

dc_inc %>% 
  left_join(dc_si, by = "zip") %>% 
  write_csv("../data/final project/CLEAN_DATA/dc_si.csv")
  
  
