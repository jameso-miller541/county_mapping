#James Miller

#Map census tracts at high risk for lead poisoning, based on concentration of properties built prior to 1978 and lead poisoning incidence



library(tidyverse)
library(leaflet)
library(sf)

#read in data

#childhood lead exposure data
Childhood_Lead_Exposure_testyear_ebll_census_data <- read_csv("data/Childhood_Lead_Exposure_testyear_ebll_census_data.csv")

#census tract shapefiles (Minnesota)
census_tracts <- read_sf("~/RCamp/county_mapping/data/mn_shps/tl_2025_27_tract.shp")

#year property built estimates (St. Louis County)
year_built <- read_csv("data/year_built_by_tract/ACSDT5Y2024.B25034-Data.csv")


