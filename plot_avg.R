# a script to summarise snow samples.
# takes device magic forms created by the weather station form and the aco ground truth form.

# get dependencies
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

##### get rov coords #####

coord <- read_csv("March 08-12th/3 Published/cru_trip1_sample_locations_ppk.csv") %>%
  group_by(Plot_ID) %>%  # do average for camera stations
  summarise(northing = mean(northing),
            easting = mean(easting),
            elev = mean(elev),
            aspect = first(aspect),
            cover = first(cover),
            station_type = first(station_type))

##### get weather station form ####

# weather station form that has wx snow course
wx_df <- read_csv("March 08-12th/1 Raw/Mobile Forms/dm_weatherstationvisit_form.csv") %>%
  select(Job_Start_Time, Vancouver_Island_Stations, starts_with("Snow_Course")) %>%
  filter(Vancouver_Island_Stations == "Upper Cruickshank") %>%
  mutate(Snow_Course_Job.Add_Snow_Core.Depth__cm_ = as.numeric(Snow_Course_Job.Add_Snow_Core.Depth__cm_))

# deal with multi cores from weather station survey
snow_course <- wx_df %>%
  slice(1:7) %>%
  mutate(core_number = substring(Snow_Course_Job.Add_Snow_Core.Snow_Core__, 1,1)) %>%
  group_by(core_number) %>%
  summarise(
    depth = max(Snow_Course_Job.Add_Snow_Core.Depth__cm_, na.rm = T),
    core = sum(Snow_Course_Job.Add_Snow_Core.Core__cm_),
    plug = sum(Snow_Course_Job.Add_Snow_Core.Plug__cm_),
    mass = sum(Snow_Course_Job.Add_Snow_Core.Mass_Final__g_),
    rating = mean(Snow_Course_Job.Add_Snow_Core.Core_Rating)
  ) %>%
  mutate(SWE = mass / (3.142 * (2.1 * 2.1)),
         density = SWE / (depth - plug)
         )

# multi core to single
snow_course_avg <- snow_course %>%
  summarise(
    mean_depth = mean(depth),
    med_depth = median(depth),
    max_depth = max(depth),
    min_depth = min(depth),
    sd_depth = sd(depth),
    SWE = mean(SWE),
    density = mean(density),
    count = n()
  )


snow_course_meta <- data.frame("Plot_ID" = "CRU_snowsurvey",
                                    "plot_datetime" = "2021-03-09 9:59:00",
                                    "plot_type" = "snowsurvey") %>%
  cbind(snow_course_avg)

snow_depth_snsr <- wx_df$Snow_Course_Job.SR50_Depth_[1]

snow_scale_density <- wx_df %>%
  slice(8) %>%
  select(core_number = Snow_Course_Job.Add_Snow_Core.Snow_Core__,
         depth = Snow_Course_Job.Add_Snow_Core.Depth__cm_,
         core = Snow_Course_Job.Add_Snow_Core.Core__cm_,
         plug = Snow_Course_Job.Add_Snow_Core.Plug__cm_,
         mass = Snow_Course_Job.Add_Snow_Core.Mass_Final__g_,
         rating = Snow_Course_Job.Add_Snow_Core.Core_Rating) %>%
  mutate(
    mean_depth = depth,
    med_depth = NA,
    max_depth = NA,
    min_depth = NA,
    sd_depth = NA,
    SWE = mass / (3.142 * (2.1 * 2.1)),
    density = SWE / (depth - plug),
    count = 1) %>%
  select(mean_depth:count)

# final wx snow survey list
snow_scale_meta <- data.frame("Plot_ID" = "CRU_snowpillow",
                               "plot_datetime" = "2021-03-09 9:59:00",
                               "plot_type" = "snowscale") %>%
  cbind(snow_scale_density)

##### ACO Device Magic Form #####

# get raw device magic tbl
raw <- read_csv("March 08-12th/1 Raw/Mobile Forms/2021_03_ACO_form_data.csv") %>%
  select(Survey_Start_Time,
         Study_Area,
         Plot_ID,
         plot_type = Point_Observation.Type_of_Plot,
         cardinal_direction = Point_Observation.Cardinal_Direction,
         multi_core = Point_Observation.Multi_Part_Core_,
         depth = Point_Observation.Depth__cm_,
         core = Point_Observation.Core_Length__cm_,
         plug = Point_Observation.Plug__cm_,
         mass = Point_Observation.Mass_Final__g_) %>%
  filter(Study_Area == "Cruickshank", is.na(depth) == F)

# fix cores done in multiple tries
multi <- raw %>%
  filter(multi_core == "yes")

multi_calc <- multi %>%
  group_by(Plot_ID) %>%
  summarise(
    depth = max(depth, na.rm = T),
    core = sum(core, na.rm = T),
    plug = sum(plug, na.rm = T),
    mass = sum(mass, na.rm = T)) %>%
  mutate(
    SWE = mass / (3.142 * (2.1 * 2.1)),
    density = SWE / (depth - plug)
  )

multi_final <- multi %>%
  select(Survey_Start_Time:multi_core) %>%
  distinct() %>%
  left_join(multi_calc, by = "Plot_ID")

# fltr table and get avg depths for each plot
fltr <- raw %>%
  mutate(
    depth = case_when(
    Plot_ID == "CRU_wx_cam_stks" ~ depth - 40, # made error in the field need to sub 40 cm for wx cam stakes.
    TRUE ~ depth # else replace with original depth
  )) %>%
  filter(multi_core == "no" | is.na(multi_core) == T) %>%
  mutate(
    SWE = mass / (3.142 * (2.1 * 2.1)),
    density = SWE / (depth - plug)
  ) %>%
  rbind(multi_final) %>%
  group_by(Plot_ID) %>%
  summarise(
    plot_datetime = first(Survey_Start_Time),
    plot_type = first(plot_type),
    mean_depth = mean(depth),
    med_depth = median(depth),
    max_depth = max(depth),
    min_depth = min(depth),
    sd_depth = sd(depth),
    SWE = mean(SWE, na.rm = T),
    density = mean(density, na.rm = T),
    count = n())


# join all snow data together and add coords
all_dat <- rbind(snow_scale_meta,
                 snow_course_meta,
                 fltr,
                 c("CRU_wx_SR50","2021-03-09 9:59:00","SR50", snow_depth_snsr,NA,NA,NA,NA,NA,NA,NA)) %>%
  left_join(coord, by = "Plot_ID") %>%
  mutate(across(mean_depth:density, as.numeric))

# write out

write_csv(all_dat, "March 08-12th/3 Published/2021_cru_trip1_snowdata_ppk.csv")

# graphs

all_wide <- pivot_longer(all_dat, cols = mean_depth:density,  names_to = "group")

ggplot(all_wide, aes(x = Plot_ID, y = value, colour = group)) +
  geom_point()

ggplot(all_dat, aes(x = cover, y = mean_depth, colour = station_type)) +
  geom_point()




