#James Miller

#Map census tracts at high risk for lead poisoning, based on concentration of properties built prior to 1978 and lead poisoning incidence
library(tidyverse)
library(leaflet)
library(scales)
library(sf)

#read in data

#childhood lead exposure data
Childhood_Lead_Exposure_testyear_ebll_census_data <- read_csv("data/Childhood_Lead_Exposure_testyear_ebll_census_data.csv")

#census tract shapefiles (Minnesota)
census_tracts <- read_sf("data/mn_shps/tl_2025_27_tract.shp")

#year property built estimates (St. Louis County)
year_built <- read_csv("data/year_built_by_tract/ACSDT5Y2024.B25034-Data.csv")

#slc boundary shapefile
slc_boundary <- read_sf("data/st_louis_boundary/County_Boundary%2C_Saint_Louis_County%2C_MN.shp")

#tidy and clean data
#
#census tracts
census_tracts$GEOID <- as.double(census_tracts$GEOID)#change GEOID to double

lead_exposure_tracts <- left_join(census_tracts, lead_exposure, by = "GEOID") #join census tracts to lead exposure

#lead exposure
slc_lead_exposure_tracts <- mutate(lead_exposure_tracts, 
                        Count = case_when(Count == "4-Jan" ~ "1-4", .default = Count)) %>% #read suppressed counts correctly
  filter(COUNTYFP == 137) %>%  #filter to only tracts in St. Louis County (county FIPs code 137)
  select(STATEFP,
         COUNTYFP,
         TRACTCE,
         GEOID,
         GEOIDFQ,
         geometry,
         Topic,
         Year,
         Geo_Name,
         Category,
         Measure,
         Value,
         Count,
         Population,
         Note,
         Label) #select relevant attributes

#year built tidying and calculating percent of homes built before 1970
year_built_cleaned <- year_built %>% 
  rename(GEOIDFQ = GEO_ID,
         estimate_total = B25034_001E,
         estimate_MOE = B25034_001M,
         estimate_2020_or_later = B25034_002E,
         MOE_2020_or_later = B25034_002M,
         estimate_2010_to_2019 = B25034_003E,
         MOE_2010_to_2019 = B25034_003M,
         estimate_2000_to_2009 = B25034_004E,
         MOE_2000_to_2009 = B25034_004M,
         estimate_1990_to_1999 = B25034_005E,
         MOE_1990_to_1999 = B25034_005M,
         estimate_1980_to_1989 = B25034_006E,
         MOE_1980_to_1989 = B25034_006M,
         estimate_1970_to_1979 = B25034_007E,
         MOE_1970_to_1979 = B25034_007M,
         estimate_1960_to_1969 = B25034_008E,
         MOE_1960_to_1969 = B25034_008M,
         estimate_1950_to_1959 = B25034_009E,
         MOE_1950_to_1959 = B25034_009M,
         estimate_1940_to_1949 = B25034_010E,
         MOE_1940_to_1949 = B25034_010M,
         estimate_1939_or_earlier = B25034_011E,
         MOE_1939_or_earlier = B25034_011M) %>% #renanme columns
  filter(NAME != "Geographic Area Name", NAME != "St. Louis County, Minnesota") %>% #remove header rows
  mutate(TRACTCE = str_sub(GEOIDFQ, -6)) %>% #create new column with tract ID isolated
  relocate(NAME, TRACTCE) %>% #move name to front
  mutate(across(4:26, as.numeric)) %>%  #change estimate and MOE columns to double
  select(NAME,
         GEOIDFQ,
         TRACTCE,
         estimate_total, 
         estimate_2020_or_later, 
         estimate_2010_to_2019, 
         estimate_2000_to_2009, 
         estimate_1990_to_1999,
         estimate_1980_to_1989,
         estimate_1970_to_1979,
         estimate_1960_to_1969,
         estimate_1950_to_1959,
         estimate_1940_to_1949,
         estimate_1939_or_earlier) %>% #select only estimates (removes MOEs)
  mutate(percent_built_before_1970 = label_percent(accuracy = 0.01)
         (rowSums(pick(estimate_1960_to_1969:estimate_1939_or_earlier), na.rm = T)/ 
                               estimate_total)) #calculate percentage of homes built before 1970

#select identifiers and percent built before 1970
year_built_percent <- select(year_built_cleaned, 
                             GEOIDFQ,
                             TRACTCE,
                             percent_built_before_1970)

#join percent built prior to 1970 to slc exposure tracts
slc_lead_exposure_tracts <- left_join(slc_lead_exposure_tracts, year_built_percent, 
                                      by = "TRACTCE").

#read exposure tracts as sf
slc_lead_exposure_tracts <- st_transform(slc_lead_exposure_tracts, crs = 4269)

#mapping
#
#interactive map of high lead exposure tracts
leaflet(slc_lead_exposure_tracts) %>% 
  addTiles()+
  addPolygons(data = slc_lead_exposure_tracts)

plot(slc_lead_exposure_tracts[1])




