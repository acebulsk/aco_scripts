library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

setwd("E:/Alex_Working/ACO/Vancouver/")

dat <- read_csv("data/raw/2021_ACO_Summaries_MV.csv")

# get coords

coords <- dat %>% 
  group_by(STA = Plot_ID) %>% 
  summarise(
    utm_n = mean(Northing, na.rm = T),
    utm_e = mean(Easting, na.rm = T),
    height = mean(`Elev - Ellipsoid`, na.rm = T)
  ) %>% 
  mutate(utm_z = "utm10")

# write_csv(coords, "coords_trip3.csv")  
  
coord_geo <- read.csv("data/raw/coords_geo.csv") %>%
  select(Plot_ID = station, lon, lat)

coord_utm <- read.csv("data/raw/coords_utm.csv")%>%
  select(Plot_ID = station, easting = utm_e, northing = utm_n, elev_ellipsoidal = height)

coord_final <- left_join(coord_utm, coord_geo)

# raw
raw <- read.csv("data/raw/2021_ACO_Summaries_Trip1-3_raw.csv") %>% 
  select(-Northing, -Easting, -Elev...Ellipsoid, -Elevation...Geodetic) %>% 
  pivot_longer(Centre:W2.5, names_to = "cardinal_direction", values_to = "depth") %>% 
  mutate(Study_Area = "Metro Vancouver") %>% 
  left_join(coord_final) %>% 
  filter(is.na(depth) == F) %>% 
  mutate(multi_core = NA,
         core = NA,
         plug = NA, 
         mass = NA,
         cardinal_direction = case_when(cardinal_direction == "Centre" ~ cardinal_direction,
                   TRUE ~ substr(cardinal_direction, 1,1))) %>% 
  select(ACO_Flight,
         plot_datetime,
         Study_Area,
         Plot_ID,
         easting:lat, 
         Plot_type,
         cardinal_direction,
         multi_core,
         depth,
         core,
         plug,
         mass, 
         SWE,
         density)

write_csv(raw, "data/deliverables/metrovan_trip1-3_raw_ac.csv")  


# sum

sum <- dat %>% 
  select(-Easting, -Northing, -`Elev - Ellipsoid`, -Latitude, -Longitude) %>% 
  left_join(coord_final) %>%
  select(ACO_Flight:Plot_ID,
         easting:lat,
         Plot_type:count)

write_csv(sum, "data/deliverables/metrovan_trip1-3_plot_averages.csv")  






