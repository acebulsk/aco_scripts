library(dplyr)
library(readr)
library(readxl)

trip1 <- read_xlsx("ACO_SWE__Tsitika_2021-04-19.xlsx", sheet = 2) %>%
  mutate(
    ACO_Flight_Number = 1,
    Study_Area = "Tsitika",
          plot_type = NA,
    multi_core = NA
                   ) %>%
  select(
    ACO_Flight_Number,
    plot_datetime = date,
         Plot_ID,
         Study_Area,
         plot_type,
         easting = Easting_m,
         northing = Northing_m,
         elev_ellipsoidal = Elevation_m,
         lon = long,
         lat,
         plot_type,
         cardinal_direction = Point_Observation.Cardinal_Direction,
         multi_core,
         depth = Point_Observation.Depth__cm_,
         core = Point_Observation.Core_Length__cm_,
         plug = Point_Observation.Plug__cm_,
         mass = Point_Observation.Mass_Final__g_)

trip2 <- read_xlsx("ACO_SWE__Tsitika_2021-04-19.xlsx", sheet = 4) %>%
  mutate(
    ACO_Flight_Number = 2,
    Study_Area = "Tsitika",
         plot_type = NA,
    multi_core = NA

  ) %>%
  select(
    ACO_Flight_Number,
    plot_datetime = `date/time`,
    Plot_ID,
    Study_Area,
    plot_type,
    easting = Easting_m,
    northing = Northing_m,
    elev_ellipsoidal = Elevation_m,
    lon = long,
    lat,
    plot_type,
    cardinal_direction = Cardinal_Direction,
    multi_core,
    depth,
    core,
    plug,
    mass)

all <- rbind(trip1, trip2) %>%
  group_by(Plot_ID) %>%
  mutate(
    SWE = mass / (3.14159265359 * (2.1 * 2.1)),
    density = SWE / (depth - plug)
  ) %>%
  select(-easting, -northing, -elev_ellipsoidal, -lat, -lon)

coords <- rbind(trip1, trip2) %>%
  group_by(STA = Plot_ID) %>%
  summarise(
    utm_n = mean(northing, na.rm = T),
    utm_e = mean(easting, na.rm = T),
    height = mean(elev_ellipsoidal, na.rm = T)
    ) %>%
  mutate(utm_z = "utm9") %>%
  filter(is.na(utm_n) == F)

# send to TRX
# write_csv(coords, "2021_tsitika_plot_coords_raw.csv")


coord_geo <- read.csv("2021_tsitika_plot_coords_ITRF_geo.csv") %>%
  select(Plot_ID = station, lon, lat)

coord_utm <- read.csv("2021_tsitika_plot_coords_ITRF_utm.csv")%>%
  select(Plot_ID = station, easting = utm_e, northing = utm_n, elev_ellipsoidal = height)

coord_final <- left_join(coord_utm, coord_geo)

# raw with coords

raw <- left_join(coord_final, all, by = "Plot_ID") %>%
  select(ACO_Flight_Number:Study_Area, Plot_ID:lat, plot_type:density) %>%
  arrange(ACO_Flight_Number, desc())

sum <- raw %>%
  group_by(Plot_ID, ACO_Flight_Number) %>%
  summarise(
    ACO_Flight_Number = mean(ACO_Flight_Number),
    plot_datetime = first(strftime(plot_datetime, format = '%Y-%m-%d %R')),
    Study_Area = first(Study_Area),
    easting = mean(easting, na.rm = T),
    northing = mean(northing, na.rm = T),
    elev_ellipsoidal = mean(elev_ellipsoidal, na.rm = T),
    lon = mean(lon, na.rm = T),
    lat = mean(lat, na.rm = T),
    plot_type = first(plot_type),
    mean_depth = mean(depth),
    med_depth = median(depth),
    max_depth = max(depth),
    min_depth = min(depth),
    sd_depth = sd(depth),
    SWE = mean(SWE, na.rm = T),
    density = mean(density, na.rm = T),
    count = n()) %>%
  select(ACO_Flight_Number:Study_Area, Plot_ID, easting:count) %>%
  arrange(ACO_Flight_Number, desc())


# write out

write.csv(raw, "summary/2021_tsitika_trip1and2_raw.csv", row.names = F)
write.csv(sum, "summary/2021_tsitika_trip1and2_summary.csv", row.names = F)

