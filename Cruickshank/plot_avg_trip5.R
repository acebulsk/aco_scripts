# a script to summarise snow samples.
# takes device magic forms created by the weather station form and the aco ground truth form.

# get dependencies
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)

# working directory
setwd("E:/Alex_Working/ACO/aco_scripts/Cruickshank/5June_16-18")

##### get rov coords #####

coord_geo <- read.csv("../Scratch/cru_plot_coords_ppk_final_ITRF2014.csv") %>%
  select(Plot_ID = station, lon, lat)

coord_utm <- read.csv("../Scratch/cru_plot_coords_ppk_final_ITRF2014_UTM10.csv") %>%
  select(Plot_ID = station, easting = utm_e, northing = utm_n, elev_ellipsoidal = height)

coord_final <- left_join(coord_utm, coord_geo)

# write.csv(coord_final, "cru_plot_coords_ITRF2014.csv", row.names = F)

##### ACO Device Magic Form #####

# get raw device magic tbl (most recent version)
raw <- read.csv("../../ACO Ground Truth Plot - TRIP 5.csv") %>%
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
         dist_from_centre = Point_Observation.Distance_From_Centre__m_,
         multi_core = Point_Observation.Multi_Part_Core_,
         depth = depth_final,
         core = Point_Observation.Core_Length__cm_,
         plug = Point_Observation.Plug__cm_,
         mass = Point_Observation.Mass_Final__g_) %>%
  mutate(plug = replace_na(plug, 0),
         Survey_Start_Time = ymd_hms(Survey_Start_Time),
         cardinal_direction = case_when( # there is one missing card arm direction - which is North
           cardinal_direction == "" ~ "N",
           TRUE ~ cardinal_direction)) %>%
  filter(Study_Area == "Cruickshank",
         is.na(depth) == F,
         Survey_Start_Time > "2021-06-01 11:58:00") # only take trip 5 survey

# check number of plots done during trip 5, should be 10 missing E7S and CAM2_NG_4
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
  group_by(Plot_ID, Survey_Start_Time, Study_Area, plot_type, cardinal_direction, dist_from_centre) %>%
  summarise( # average multi cores done on small snow patches just need to average across each one
    multi_core = first(multi_core),
    depth = mean(depth),
    core = mean(core),
    plug = mean(plug),
    mass = mean(mass),
    count = n()
  ) %>%
  mutate(
    depth = depth - plug,
    SWE = mass / (3.14159265359 * (2.1 * 2.1)),
    density = SWE / (depth)
  ) %>%
  rbind(multi_final) %>%
  left_join(coord_final, by = "Plot_ID") %>%
  select(Survey_Start_Time:Plot_ID, easting:lat, plot_type:density)

length(unique(all_final$Plot_ID))


# fill incomplete plots

plot_template_dir <-  c("N", "N", "N", "N",
                        "NE", "NE", "NE", "NE",
                        "E", "E", "E", "E",
                        "SE", "SE", "SE", "SE",
                        "S", "S", "S", "S",
                        "SW", "SW", "SW", "SW",
                        "W", "W", "W", "W",
                        "NW", "NW", "NW", "NW")
seq <- c(2.5, 5, 7.5, 10)
rep <- rep(seq, 8)
centre <- c("Centre", "")

plot_template <- data.frame(cardinal_direction = plot_template_dir, dist_from_centre = rep) %>%
  rbind(centre)

fill <- all_final %>%
  group_by(Plot_ID) %>%
  group_modify(~ right_join(.x, plot_template, by = c("cardinal_direction", "dist_from_centre"))) %>%
  fill(Plot_ID:plot_type) %>%
  mutate(depth = replace_na(depth, 0),
         plot_type = "Cardinal 10 m") %>%
  select(Survey_Start_Time:Plot_ID, easting:lat, plot_type:density, -dist_from_centre, -count)

count <- fill %>% group_by(Plot_ID) %>% tally() # 33 each for 8 * 4 + 1

write.csv(fill, "cru_trip5_plots_raw.csv", row.names = F)

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
plt_avg <- fill %>%
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
  select(plot_datetime, Study_Area, Plot_ID, easting:lat, plot_type:count) %>%
  mutate(plot_type = "Cardinal 10 m")

# write out

# write_csv(all_dat, "April_5-9_trip2/3 Published/2021_cru_trip2_allsnowdata_ppk.csv")
write.csv(plt_avg, "Deliverables/cru_trip5_plot_averages.csv", row.names = F)

write_csv(dly_depth_totals, "Deliverables/cru_trip4_depth_total2.csv")

write_csv(dly_density_totals, "Deliverables/cru_trip4_density_total2.csv")






