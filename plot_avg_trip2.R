# a script to summarise snow samples.
# takes device magic forms created by the weather station form and the aco ground truth form.

# get dependencies
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)

setwd("E:/Alex_Working/ACO/Cruickshank")

##### get rov coords #####

# # trip 1
# coord1 <- read_csv("March 08-12th/3 Published/cru_trip1_sample_locations_ppk.csv") %>%
#   select(-alias, -Plot_ID, -date, Plot_ID = ID) %>%
#   filter(!Plot_ID %in% c("Refpointwxb", "CRU_baseref_20210311"))
#
# # calculate average for cameras
# coord_plot_average <- read_csv("March 08-12th/3 Published/cru_trip1_sample_locations_ppk.csv") %>%
#   select(-alias, -date, -ID) %>%
#   filter(Plot_ID != "na") %>%
#   group_by(Plot_ID) %>%  # do average for camera stations
#   summarise(northing = mean(northing),
#             easting = mean(easting),
#             elev = mean(elev),
#             solution = first(solution),
#             aspect = first(aspect),
#             cover = first(cover),
#             station_type = first(station_type))

# trip 2
# coord2 <- read_csv("April_5-9_trip2/1 Raw/Data Collector/20210405_20210410_points.txt",
#                    col_names = c("Plot_ID", "northing", "easting", "elev", "lat", "lon", "elevation")) %>%
#   filter(!Plot_ID %in% c("CRU_wx","CRU_wx2","CRU_wx3")) %>% #rm ref coords
#   select(-lat, -lon, -elevation)
#
# solution <- c("SBAS", "SBAS", "fixed","DGPS", "fixed" ,"fixed" , "fixed", "float", "fixed")
# aspect <- c( "north","south","east","east","east","south","south","south","south")
# cover <- c("alpine", "alpine",  "open", "forest", "forest",  "forest", "forest", "forest", "open")
# station_type <-  c( "cardinal_10", "cardinal_10",  "cardinal_10", "camera", "cardinal_10", "cardinal_10", "cardinal_10", "cardinal_10", "cardinal_10")
#
# coord2_meta <- cbind(coord2, solution, aspect, cover, station_type)
#
# CRU_E6N <- coord1 %>%
#   filter(Plot_ID == "CRU_CAM2_NG_4") %>%  #used camera stake # 4 as centre plot
#   mutate(Plot_ID = "CRU_E6N_C2STK4")
#
# # combine trip 1 and 2 coords
# coord_all <- rbind(coord_plot_average, coord2_meta, CRU_E6N)
#
# coord_trx <- coord_all %>%
#   select(STA = Plot_ID, utm_n = northing, utm_e = easting, height = elev) %>%
#   mutate(utm_z = "utm10")

# write.csv(coord_all, "cru_plot_coords_ppk_final.csv")
# write.csv(coord_trx, "cru_plot_coords_ppk_final_totrx.csv", rownames = F)

coord_geo <- read.csv("Scratch/cru_plot_coords_ppk_final_ITRF2014.csv") %>%
  select(Plot_ID = station, lon, lat)

coord_utm <- read.csv("Scratch/cru_plot_coords_ppk_final_ITRF2014_UTM10.csv")%>%
  select(Plot_ID = station, easting = utm_e, northing = utm_n, elev_ellipsoidal = height)

coord_final <- left_join(coord_utm, coord_geo)

# write.csv(coord_final, "cru_plot_coords_ITRF2014.csv", row.names = F)

##### get weather station form ####

# weather station form that has wx snow course
wx_df <- read_csv("2April_5-9_trip2/1 Raw/Weather Station Visit Form - Weather Station Visit - v1.14_raw.csv") %>%
  select(Job_Start_Time, Vancouver_Island_Stations, starts_with("Snow_Course")) %>%
  filter(Vancouver_Island_Stations == "Upper Cruickshank")

# deal with multi cores from weather station survey
snow_course <- wx_df %>%
  mutate(core_number = substring(Snow_Course.Add_Snow_Core.Snow_Core__, 1,1)) %>%
  group_by(core_number) %>%
  summarise(
    depth = max(Snow_Course.Add_Snow_Core.Depth__cm_, na.rm = T),
    core = sum(Snow_Course.Add_Snow_Core.Core__cm_, na.rm = T),
    plug = max(Snow_Course.Add_Snow_Core.Plug__cm_, na.rm = T),
    mass = sum(Snow_Course.Add_Snow_Core.Mass_Final__g_, na.rm = T),
    rating = mean(Snow_Course.Add_Snow_Core.Core_Rating, na.rm = T)
  ) %>%
  mutate(SWE = mass / (3.142 * (2.1 * 2.1)),
         density = SWE / (depth - plug)
         )

snow_course_final <- snow_course %>%
  filter(core_number != "S") %>%
  mutate(Survey_Start_Time = "4/5/2021 5:30",
         Study_Area = "Cruickshank",
         Plot_ID = "CRU_snowsurvey",
         plot_type = "wx_snow_survey",
         cardinal_direction = NA,
         sample_type = "Density",
         multi_core = "yes") %>%
  select(Survey_Start_Time:multi_core, depth:density, -rating)

snow_scale <- snow_course %>%
  filter(core_number == "S") %>%
  mutate(Survey_Start_Time = "4/5/2021 6:00",
         Study_Area = "Cruickshank",
         Plot_ID = "CRU_snowpillow",
         plot_type = "wx_snow_survey",
         cardinal_direction = NA,
         sample_type = "Density",
         multi_core = "yes") %>%
  select(Survey_Start_Time:multi_core, depth:density, -rating)

sr50 <- wx_df %>%
  slice(1) %>%
  mutate(Survey_Start_Time = "4/5/2021 6:00",
         Study_Area = "Cruickshank",
         Plot_ID = "CRU_WX_SR50",
         plot_type = "wx_snow_survey",
         cardinal_direction = NA,
         sample_type = "Depth",
         multi_core = NA) %>%
  select(Survey_Start_Time:multi_core, depth = Snow_Course.SR50_Depth_) %>%
  mutate(core = NA,
         plug = NA,
         mass = NA,
         SWE = NA,
         density = NA)


# # multi core to single
# snow_course_avg <- snow_course %>%
#   summarise(
#     mean_depth = mean(depth),
#     med_depth = median(depth),
#     max_depth = max(depth),
#     min_depth = min(depth),
#     sd_depth = sd(depth),
#     SWE = mean(SWE),
#     density = mean(density),
#     count = n()
#   )
#
#
# snow_course_meta <- data.frame("Plot_ID" = "CRU_snowsurvey",
#                                     "plot_datetime" = "2021-03-09 9:59:00",
#                                     "plot_type" = "snowsurvey") %>%
#   cbind(snow_course_avg)
#
# snow_depth_snsr <- wx_df$Snow_Course_Job.SR50_Depth_[1]
#
# snow_scale_density <- wx_df %>%
#   slice(8) %>%
#   select(core_number = Snow_Course_Job.Add_Snow_Core.Snow_Core__,
#          depth = Snow_Course_Job.Add_Snow_Core.Depth__cm_,
#          core = Snow_Course_Job.Add_Snow_Core.Core__cm_,
#          plug = Snow_Course_Job.Add_Snow_Core.Plug__cm_,
#          mass = Snow_Course_Job.Add_Snow_Core.Mass_Final__g_,
#          rating = Snow_Course_Job.Add_Snow_Core.Core_Rating) %>%
#   mutate(
#     mean_depth = depth,
#     med_depth = NA,
#     max_depth = NA,
#     min_depth = NA,
#     sd_depth = NA,
#     SWE = mass / (3.142 * (2.1 * 2.1)),
#     density = SWE / (depth - plug),
#     count = 1) %>%
#   select(mean_depth:count)
#
# # final wx snow survey list
# snow_scale_meta <- data.frame("Plot_ID" = "CRU_snowpillow",
#                                "plot_datetime" = "2021-03-09 9:59:00",
#                                "plot_type" = "snowscale") %>%
#   cbind(snow_scale_density)

##### ACO Device Magic Form #####

# get raw device magic tbl (most recent version)
raw <- read_csv("2April_5-9_trip2/1 Raw/ACO Ground Truth Plot - ACO Snow Survey Plot (use this one) - v1.72_raw.csv") %>%
  select(Survey_Start_Time,
         Study_Area,
         Plot_ID,
         plot_type = Point_Observation.Type_of_Plot,
         cardinal_direction = Point_Observation.Cardinal_Direction,
         sample_type = Point_Observation.Sample_Type,
         multi_core = Point_Observation.Multi_Part_Core_,
         depth = Point_Observation.Depth__cm_,
         core = Point_Observation.Core_Length__cm_,
         plug = Point_Observation.Plug__cm_,
         mass = Point_Observation.Mass_Final__g_) %>%
  filter(Study_Area == "Cruickshank", is.na(depth) == F)

# get older dm from version that rob used on his phone
raw_old <- read_csv("2April_5-9_trip2/1 Raw/ACO Ground Truth Plot - ACO Snow Survey Plot (use this one) - v1.61.csv") %>%
  select(Survey_Start_Time,
         Study_Area,
         Plot_ID,
         plot_type = Point_Observation.Type_of_Plot,
         cardinal_direction = Point_Observation.Cardinal_Direction,
         sample_type = Point_Observation.Sample_Type,
         multi_core = Point_Observation.Multi_Part_Core_,
         depth = Point_Observation.Depth__cm_,
         core = Point_Observation.Core_Length__cm_,
         plug = Point_Observation.Plug__cm_,
         mass = Point_Observation.Mass_Final__g_) %>%
  filter(Study_Area == "Cruickshank", is.na(depth) == F) %>%
  slice(95:130) # grab all

raw <- rbind(raw, raw_old) %>%
  mutate(plug = replace_na(plug, 0))


# fix cores done in multiple tries
multi <- raw %>%
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
fltr <- raw %>%
  filter(multi_core == "no" | is.na(multi_core) == T) %>%
  mutate(
    depth = depth - plug,
    SWE = mass / (3.14159265359 * (2.1 * 2.1)),
    density = SWE / (depth )
  ) %>%
  rbind(multi_final)

# bind wx station and make df with all snow samples taken
all_samples <- fltr %>%
  rbind(snow_course_final, snow_scale, sr50) %>%
  left_join(coord_final, by = "Plot_ID") %>%
  select(Survey_Start_Time:Plot_ID, easting:lat, plot_type:density) %>%
  mutate(Survey_Start_Time = mdy_hm(Survey_Start_Time))

write.csv(all_samples, "2April_5-9_trip2/3 Published/cru_trip2_plots_raw_plugs_subtracted.csv", row.names = F)


# daily totals for depth
dly_depth_totals <- all_samples %>%
  mutate(day = substr(Survey_Start_Time, 3,3)) %>%
  group_by(day) %>%
  summarise(
    total_depth_metre = sum(depth)/100
  )

# daily density totals
dly_density_totals <- all_samples %>%
  filter(sample_type == "Density") %>%
  mutate(day = substr(Survey_Start_Time, 3,3)) %>%
  group_by(day) %>%
  summarise(
    total_density_metre = sum(depth)/100
  )

# plot averages
plt_avg <- all_samples %>%
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
write.csv(plt_avg, "2April_5-9_trip2/3 Published/cru_trip2_plot_averages_plugs_subtracted.csv", row.names = F)

write_csv(dly_depth_totals, "2April_5-9_trip2/3 Published/cru_trip2_depth_total.csv")

write_csv(dly_density_totals, "2April_5-9_trip2/3 Published/cru_trip2_density_total.csv")

# graphs

all_wide <- pivot_longer(all_dat, cols = mean_depth:density,  names_to = "group")

ggplot(all_wide, aes(x = Plot_ID, y = value, colour = group)) +
  geom_point()

ggplot(all_dat, aes(x = cover, y = mean_depth, colour = station_type)) +
  geom_point()




