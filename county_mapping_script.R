#James Miller

#Map census tracts at high risk for lead poisoning, based on concentration of properties built prior to 1978 and lead poisoning incidence
library(tidyverse)
library(leaflet)
library(scales)
library(sf)
library(cowplot)
library(ggspatial)

#read in data
#
#childhood lead exposure data
Childhood_Lead_Exposure_testyear_ebll_census_data <- read_csv("data/Childhood_Lead_Exposure_testyear_ebll_census_data.csv")

#census tract shapefiles (Minnesota)
census_tracts <- read_sf("data/mn_shps/tl_2025_27_tract.shp")

#year property built estimates (St. Louis County)
year_built <- read_csv("data/year_built_by_tract/ACSDT5Y2024.B25034-Data.csv")

#slc boundary shapefile
slc_boundary <- read_sf("data/st_louis_boundary/County_Boundary%2C_Saint_Louis_County%2C_MN.shp")

#MN cities bounary
cities <- read_sf("data/dlh_boundary/CITIES1.shp")

#tidy and clean data
#
#census tracts
census_tracts$GEOID <- as.double(census_tracts$GEOID)#change GEOID to double

lead_exposure_tracts <- left_join(census_tracts, Childhood_Lead_Exposure_testyear_ebll_census_data, by = "GEOID") #join census tracts to lead exposure

#cites
#
cities_4269 <- st_transform(cities, crs = 4269)# project to 4269
#dlh bounding box
dlh <- filter(cities, FEATURE_NA == "Duluth") #filter to only Duluth

dlh_4269_bb <- st_as_sfc(st_bbox(dlh)) #get dlh bounding box (for inset)

#other cities in SLC
slc_cities <- filter(cities, FEATURE_NA == "Duluth"|
                       FEATURE_NA == "Hibbing"|
                       FEATURE_NA == "Virginia"|
                       FEATURE_NA == "Ely"|
                       FEATURE_NA == "Chisholm")
#find cetnter point
slc_cities_point <- st_centroid(slc_cities)

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
                                      by = "TRACTCE")

#add column for combined risk assessment with percentage of housing and lead poisoning incidence
slc_lead_exposure_tracts <- mutate(slc_lead_exposure_tracts, 
                                   risk_label = case_when(
                                     Label == "Significantly higher (1-2 times)" | Label == "Significantly higher (3+ times)" &
                                       percent_built_before_1970 >= 50 ~ ">50% buildings older than 1970 AND > EBLL incidence",
                                     percent_built_before_1970 >= 50 ~ ">50% buildings older than 1970",
                                     Label == "Significantly higher (1-2 times)" | Label == "Significantly higher (3+ times)" ~ "> EBLL incidence",
                                     T ~ "<50% buildings older than 1970"))

#read exposure tracts as sf
slc_lead_exposure_tracts <- st_transform(slc_lead_exposure_tracts, crs = 4269)

#read boundary as sf
slc_boundary_sf <- st_transform(slc_boundary, crs = 4269)

#Geospatial operations
slc_lead_exposure_tracts_clip <- st_intersection(slc_lead_exposure_tracts, slc_boundary_sf)#clip slc_lead_exposure_tracts to slc boundary (removes tracts in Lake Superior)

dlh_exposure_tracts <- st_intersection(slc_lead_exposure_tracts_clip, dlh_4269_bb) #clip tracts in duluth

#mapping
#
#plot county wide census tracts
ggm_slc <-
  ggplot(slc_lead_exposure_tracts_clip)+
  geom_sf(aes(fill = risk_label), 
          alpha = 1, 
          show.legend = F)+
  geom_sf(data = dlh_4269_bb, 
          fill = NA,
          color = "black",
          size = .7)+
  geom_sf(data = slc_cities_point)+
  geom_text(data = slc_cities_point, 
            aes(label = FEATURE_NA,
                geometry = geometry),
            stat = "sf_coordinates",
            size = 3.5,
            nudge_y = 0,
            nudge_x = .12)+
  theme(legend.position = "right", 
        legend.title.position = "top", 
        legend.direction = "vertical")+
  theme_void()+
  scale_fill_discrete(palette = c("gray", "orange", "coral"), 
                      name = "")+
  annotation_scale(location = "bl",
                   pad_x = unit(0.3, "cm"),
                   unit_category = "imperial", 
                   style = "ticks")+
  annotation_north_arrow(location = "tr",
                         pad_y = unit(.7, "cm"),
                         which_north = "grid",
                         style = north_arrow_orienteering(text_size = 5), 
                         height = unit(.7, "cm"), 
                         width = unit(.4, "cm"))
ggm_slc
?geom_text

#plot Duluth tracts
ggm_dlh <- 
  ggplot(dlh_exposure_tracts)+
  geom_sf(aes(fill = risk_label),
          show.legend = T)+
  geom_sf(data = dlh_4269_bb, 
          fill = NA,
          color = "black",
          size = 1.2)+
   coord_sf(xlim = st_bbox(dlh_4269_bb) [c(1, 3)],
             ylim = st_bbox(dlh_4269_bb)[c(2, 4)])+
  theme_void()+
  scale_fill_discrete(palette = c("gray", "orange", "coral"),
                      name = "")+
  theme(legend.position = c(0.53,1.2), 
        legend.direction = "vertical")

ggm_dlh
  
#inset map
ggdraw()+
  draw_plot(ggm_slc, 
            x = -.35)+
  draw_plot(ggm_dlh,
            x = 0.2, 
            y = 0.25,
            width = .45,
            height = .45)+
  draw_label("Estimated Housing Age and and EBLL Incidence in St. Louis County, MN",
             fontface = "bold",
             x = 0,
             hjust = -0.1,
             vjust = -22,
             size = 14)+
  theme(plot.margin = margin(0,0,0,0))

#leaflet mapping
#
#create palette for level of risk
pal1 <- colorFactor(c("gray", 
                      "orange",
                      "yellow",
                      "coral"),
                    domain = slc_lead_exposure_tracts$risk_label)




leaflet(slc_lead_exposure_tracts_clip) %>% 
  addTiles() %>% 
  addPolygons(fillColor = ~pal1(risk_label), 
              label = ~risk_label, 
              opacity = .5,
              color = "black",
              weight = 2,
              fillOpacity = .5) %>% 
  addLegend(values = ~risk_label,
            pal = pal1, 
            title = "Risk Label")


