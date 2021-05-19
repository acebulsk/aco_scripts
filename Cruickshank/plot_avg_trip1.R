# a script to summarise snow samples.
# takes device magic forms created by the weather station form and the aco ground truth form.

# get dependencies
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

##### get rov coords #####
# get coords converted to itrf 2014 from trx nad84 csrs <- itrf2014
coord_geo <- read.csv("Scratch/cru_plot_coords_ppk_final_ITRF2014.csv") %>%
  select(Plot_ID = station, lon, lat)

coord_utm <- read.csv("Scratch/cru_plot_coords_ppk_final_ITRF2014_UTM10.csv")%>%
  select(Plot_ID = station, easting = utm_e, northing = utm_n, elev_ellipsoidal = height)

coord_final <- left_join(coord_utm, coord_geo)


##### get weather station form ####

# weather station form that has wx snow course
wx_df <- read_csv("1March 08-12th/1 Raw/Mobile Forms/dm_weatherstationvisit_form.csv") %>%
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
  mutate(
         depth = depth - plug,
         SWE = mass / (3.14159265359 * (2.1 * 2.1)),
         density = SWE / (depth)
         )

snow_course_final <- snow_course %>%
  mutate(
    Survey_Start_Time = "3/9/2021 9:59",
    Study_Area = "Cruickshank",
    Plot_ID = "CRU_snowsurvey",
    plot_type = "wx_snow_survey",
    cardinal_direction = NA,
    sample_type = "Density",
    multi_core = "yes") %>%
  select(Survey_Start_Time:multi_core, depth:density, -rating)

snow_scale_density <- wx_df %>%
  slice(8) %>%
  select(core_number = Snow_Course_Job.Add_Snow_Core.Snow_Core__,
         depth = Snow_Course_Job.Add_Snow_Core.Depth__cm_,
         core = Snow_Course_Job.Add_Snow_Core.Core__cm_,
         plug = Snow_Course_Job.Add_Snow_Core.Plug__cm_,
         mass = Snow_Course_Job.Add_Snow_Core.Mass_Final__g_,
         rating = Snow_Course_Job.Add_Snow_Core.Core_Rating) %>%
  mutate(
    depth = depth - plug,
    SWE = mass / (3.14159265359 * (2.1 * 2.1)),
    density = SWE / (depth)
  )

snow_scale_final <- snow_scale_density %>%
  mutate(
    Survey_Start_Time = "3/9/2021 2:59",
    Study_Area = "Cruickshank",
    Plot_ID = "CRU_snowpillow",
    plot_type = "wx_snow_survey",
    cardinal_direction = NA,
    sample_type = "Density",
    multi_core = "yes") %>%
  select(Survey_Start_Time:multi_core, depth:density, -rating)

sr50 <- wx_df %>%
  slice(1) %>%
  mutate(Survey_Start_Time = "3/9/2021 2:59",
         Study_Area = "Cruickshank",
         Plot_ID = "CRU_WX_SR50",
         plot_type = "wx_snow_survey",
         cardinal_direction = NA,
         sample_type = "Depth",
         multi_core = NA) %>%
  select(Survey_Start_Time:multi_core, depth = Snow_Course_Job.SR50_Depth_) %>%
  mutate(core = NA,
         plug = NA,
         mass = NA,
         SWE = NA,
         density = NA)


##### ACO Device Magic Form #####

# get raw device magic tbl
raw <- read_csv("1March 08-12th/1 Raw/Mobile Forms/2021_03_ACO_form_data.csv") %>%
  select(Survey_Start_Time,
         Study_Area,
         Plot_ID,
         plot_type = Point_Observation.Type_of_Plot,
         sample_type = Point_Observation.Sample_Type,
         cardinal_direction = Point_Observation.Cardinal_Direction,
         multi_core = Point_Observation.Multi_Part_Core_,
         depth = Point_Observation.Depth__cm_,
         core = Point_Observation.Core_Length__cm_,
         plug = Point_Observation.Plug__cm_,
         mass = Point_Observation.Mass_Final__g_) %>%
  filter(Study_Area == "Cruickshank", is.na(depth) == F) %>%
  mutate(
    plug = replace_na(plug, 0),
    depth = case_when(
      Plot_ID == "CRU_N3O" & sample_type == "Depth"  ~ ((depth  + 10)), # using 5 extensions thought they were 40 cm in field but are 42 cm. So shouldve been adding 210 cm and these measurements were with an addition of 200 cm
      Plot_ID == "CRU_N4S" & sample_type == "Depth" ~ ((depth  + 10)), # using 5 extensions thought they were 40 cm in field but are 42 cm. So shouldve been adding 210 cm and these measurements were with an addition of 200 cm
      Plot_ID == "CRU_wx_cam_stks" ~ ((depth  + 10) - 42), # made error in the field need to sub 40 cm for wx cam stakes. # using 5 extensions thought they were 40 cm in field but are 42 cm. So shouldve been adding 210 cm and these measurements were with an addition of 200 cm
      TRUE ~ depth # else replace with original depth
    ),
    Plot_ID = case_when(
      Plot_ID == "CRU_wx_cam_stks" ~ "CRU_WX_CAM",
      Plot_ID == "CRU-CAM1-OG" ~ "CRU_CAM1",
      Plot_ID == "CRU-CAM2-NG" ~ "CRU_CAM2",
      Plot_ID == "CRU-CAM3-OG" ~ "CRU_CAM3",
      TRUE ~ Plot_ID
    ))

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

# fltr table and get avg depths for each plot
fltr <- raw %>%
  filter(multi_core == "no" | is.na(multi_core) == T) %>%
  mutate(
    depth = depth - plug,
    SWE = mass / (3.14159265359 * (2.1 * 2.1)),
    density = SWE / (depth )
  ) %>%
  rbind(multi_final)

all_samples <- fltr %>%
  rbind(snow_scale_final,
        snow_course_final,
        sr50) %>%
  left_join(coord_final, by = "Plot_ID") %>%
  select(Survey_Start_Time:Plot_ID, easting:lat, plot_type:cardinal_direction, multi_core:density)

write.csv(all_samples, "1March 08-12th/3 Published/cru_trip1_plots_raw_plugs_subtracted.csv", row.names = F)

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
write.csv(plt_avg, "1March 08-12th/3 Published/cru_trip1_plot_averages_plugs_subtracted.csv", row.names = F)
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


# join all snow data together and add coords
all_dat <- rbind(snow_scale_meta,
                 snow_course_meta,
                 fltr,
                 c("CRU_WX_SR50","2021-03-09 9:59:00","SR50", snow_depth_snsr,NA,NA,NA,NA,NA,NA,NA)) %>%
  left_join(coords, by = "Plot_ID") %>%
  mutate(across(mean_depth:density, as.numeric),
         Study_Area = "Cruickshank") %>%
  select(plot_datetime, Study_Area, Plot_ID, easting:lat, plot_type:count)


# write out

write_csv(all_dat, "March 08-12th/3 Published/!2021_cru_trip1_snowdata_ppk_ITRF2014.csv")

# graphs

all_wide <- pivot_longer(all_dat, cols = mean_depth:density,  names_to = "group")

ggplot(all_wide, aes(x = Plot_ID, y = value, colour = group)) +
  geom_point()

ggplot(all_dat, aes(x = cover, y = mean_depth, colour = station_type)) +
  geom_point()




