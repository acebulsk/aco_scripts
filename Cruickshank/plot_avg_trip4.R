# a script to summarise snow samples.
# takes device magic forms created by the weather station form and the aco ground truth form.

# get dependencies
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)

# working directory
setwd("E:/Alex_Working/ACO/aco_scripts/Cruickshank/4May_13-15")

##### get rov coords #####

coord_geo <- read.csv("../Scratch/cru_plot_coords_ppk_final_ITRF2014.csv") %>%
  select(Plot_ID = station, lon, lat)

coord_utm <- read.csv("../Scratch/cru_plot_coords_ppk_final_ITRF2014_UTM10.csv") %>%
  select(Plot_ID = station, easting = utm_e, northing = utm_n, elev_ellipsoidal = height)

coord_final <- left_join(coord_utm, coord_geo)

# write.csv(coord_final, "cru_plot_coords_ITRF2014.csv", row.names = F)

##### ACO Device Magic Form #####

# get raw device magic tbl (most recent version)
raw <- read.csv("Raw/ACO Ground Truth Plot - ACO Snow Survey Plot (use this one) - v1.88(1).csv") %>%
  mutate(
    # control for extensions added
    depth_final = case_when(
      Point_Observation.Sample_Type == "Density" ~ Point_Observation.Depth__cm_,
      Point_Observation.Sample_Type == "Depth" ~ Point_Observation.Depth_Final__cm_
    ))

fltr <- raw %>%
  select(Survey_Start_Time,
         Study_Area,
         Plot_ID,
         plot_type = Point_Observation.Type_of_Plot,
         cardinal_direction = Point_Observation.Cardinal_Direction,
         multi_core = Point_Observation.Multi_Part_Core_,
         depth = depth_final,
         core = Point_Observation.Core_Length__cm_,
         plug = Point_Observation.Plug__cm_,
         mass = Point_Observation.Mass_Final__g_) %>%
  mutate(plug = replace_na(plug, 0)) %>%
  filter(Study_Area == "Cruickshank", is.na(depth) == F) %>%
  slice(293:n()) # only take trip 4 survey

# check number of plots done during trip 4, should be 14 (12 minus two extra plots aka cam 3 and sr50)
length(unique(fltr$Plot_ID))

# fix cores done in multiple tries
multi <- fltr %>%
  filter(multi_core == "yes")

multi_calc <- multi %>%
  group_by(Plot_ID, cardinal_direction) %>%
  summarise(
    depth = max(depth, na.rm = T),
    core = sum(core, na.rm = T),
    plug = sum(plug, na.rm = T),
    mass = sum(mass, na.rm = T)) %>%
  mutate(
    depth = depth - plug,
    SWE = mass / (3.14159265359 * (2.1 * 2.1)),
    density = SWE / (depth)
  )

multi_final <- multi %>%
  select(Survey_Start_Time:multi_core) %>%
  distinct() %>%
  left_join(multi_calc, by = c("Plot_ID", "cardinal_direction"))

# fltr table and bind multi cores
all_final <- fltr %>%
  filter(multi_core == "no" | multi_core == "") %>% # get non multi cores and probes
  mutate(
    depth = depth - plug,
    SWE = mass / (3.14159265359 * (2.1 * 2.1)),
    density = SWE / (depth)
  ) %>%
  rbind(multi_final) %>%
  left_join(coord_final, by = "Plot_ID") %>%
  select(Survey_Start_Time:Plot_ID, easting:lat, plot_type:density)

write.csv(all_final, "Deliverables/cru_trip4_plots_raw.csv", row.names = F)


# daily totals for depth
dly_depth_totals <- all_final %>%
  mutate(day = substr(Survey_Start_Time, 3,4)) %>%
  group_by(day) %>%
  summarise(
    total_depth_metre = sum(depth)/100
  )

# daily density totals
dly_density_totals <- all_final %>%
  filter(sample_type == "Density") %>%
  mutate(day = substr(Survey_Start_Time, 3,4)) %>%
  group_by(day) %>%
  summarise(
    total_density_metre = sum(depth)/100
  )

# plot averages
plt_avg <- all_final %>%
  group_by(Plot_ID) %>%
  summarise(
    plot_datetime = first(Survey_Start_Time),
    Study_Area = first(Study_Area),
    plot_type = first(plot_type),
    mean_depth = mean(depth),
    med_depth = median(depth),
    max_depth = max(depth),
    min_depth = min(depth),
    sd_depth = sd(depth),
    SWE = mean(SWE, na.rm = T),
    density = mean(density, na.rm = T),
    count = n()) %>%
  left_join(coord_final, by = "Plot_ID") %>%
  select(plot_datetime, Study_Area, Plot_ID, easting:lat, plot_type:count)

# write out

# write_csv(all_dat, "April_5-9_trip2/3 Published/2021_cru_trip2_allsnowdata_ppk.csv")
write.csv(plt_avg, "Deliverables/cru_trip4_plot_averages.csv", row.names = F)

write_csv(dly_depth_totals, "Deliverables/cru_trip4_depth_total2.csv")

write_csv(dly_density_totals, "Deliverables/cru_trip4_density_total2.csv")






