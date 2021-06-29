library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(lubridate)

# send to TRX
# write_csv(coords, "2021_tsitika_plot_coords_raw.csv")


coord_geo <- read.csv("coords/2021_tsitika_plot_coords_ITRF_geo.csv") %>%
  select(Plot_ID = station, lon, lat)

coord_utm <- read.csv("coords/2021_tsitika_plot_coords_ITRF_utm.csv")%>%
  select(Plot_ID = station, easting = utm_e, northing = utm_n, elev_ellipsoidal = height)

coord_final <- left_join(coord_utm, coord_geo)

##### get weather station form ####

# weather station form that has wx snow course
wx_df <- read.csv("raw/Weather Station Visit Form - Weather Station Visit - v1.14(3).csv") %>%
select(Project_Name, Job_Start_Time, Russell_Creek_Substation, starts_with("Snow_Course")) %>%
  filter(Project_Name == "Russell Creek") %>%
  mutate(Job_Start_Time = ymd_hms(Job_Start_Time),
         core_number_numeric =  gsub("[^0-9.-]", "",Snow_Course.Add_Snow_Core.Snow_Core__),
         Snow_Course.Tare_Weight__g_ = replace_na(Snow_Course.Tare_Weight__g_, 0),
         Snow_Course.Add_Snow_Core.Mass_Final__g_ = Snow_Course.Add_Snow_Core.Mass___Tube__g_ - Snow_Course.Tare_Weight__g_) %>%
  filter(Job_Start_Time > "2021-05-13 0:00:00")

# deal with multi cores from weather station survey
snow_course <- wx_df %>%
  group_by(Russell_Creek_Substation, core_number_numeric) %>%
  summarise(
    Survey_Start_Time = first(Job_Start_Time),
    depth = max(Snow_Course.Add_Snow_Core.Depth__cm_, na.rm = T),
    core = sum(Snow_Course.Add_Snow_Core.Core__cm_, na.rm = T),
    plug = max(Snow_Course.Add_Snow_Core.Plug__cm_, na.rm = T),
    mass = sum(Snow_Course.Add_Snow_Core.Mass_Final__g_, na.rm = T),
    rating = mean(Snow_Course.Add_Snow_Core.Core_Rating, na.rm = T)
  ) %>%
  mutate(
    depth = depth - plug,
    SWE = mass / (3.14159265359 * (2.1 * 2.1)),
    density = if_else(is.na(SWE / (depth)), 0, SWE / (depth))
  ) %>%
  filter(depth != "NaN")

snow_course_final <- snow_course %>%
  mutate(Study_Area = "Russell Creek",
         Plot_ID = Russell_Creek_Substation,
         plot_type = "wx_snow_survey",
         cardinal_direction = NA,
         multi_core = "no") %>%
  ungroup() %>%
  select(Survey_Start_Time, Study_Area, Plot_ID, plot_type, cardinal_direction, multi_core, depth:density, -rating)


sr50 <- wx_df %>%
  group_by(Russell_Creek_Substation) %>%
  summarise(
    Survey_Start_Time = first(Job_Start_Time),
    depth = mean(Snow_Course.SR50_Depth_)) %>%
    filter(is.na(depth) == F) %>%
    mutate(
         Study_Area = "Russell Creek",
         Plot_ID = paste0("SR50_", Russell_Creek_Substation),
         plot_type = "wx_snow_survey",
         cardinal_direction = NA,
         multi_core = NA,
         core = NA,
         plug = NA,
         mass = NA,
         SWE = NA,
         density = NA) %>%
  select(Survey_Start_Time, Study_Area, Plot_ID, plot_type, cardinal_direction, multi_core, depth:density)


wx_final <- rbind(snow_course_final, sr50) %>%
  left_join(coord_final, by = "Plot_ID") %>%
  select(Survey_Start_Time:Plot_ID, easting:lat, plot_type:density)


##### ACO Device Magic Form #####

raw <- read.csv("raw/ACO Ground Truth Plot - ACO Snow Survey Plot (use this one) - v1.88.csv")

dat <- raw %>%
  mutate(
    depth_final = case_when(
      Point_Observation.Sample_Type == "Density" ~ Point_Observation.Depth__cm_,
      Point_Observation.Sample_Type == "Depth" ~ Point_Observation.Depth_Final__cm_
    )
  ) %>%
  select(
    Survey_Start_Time,
    Study_Area,
    Plot_ID,
    plot_type = Point_Observation.Type_of_Plot,
    cardinal_direction = Point_Observation.Cardinal_Direction,
    multi_core = Point_Observation.Multi_Part_Core_,
    depth = depth_final,
    core = Point_Observation.Core_Length__cm_,
    plug = Point_Observation.Plug__cm_,
    mass = Point_Observation.Mass_Final__g_) %>%
  mutate(plug = replace_na(plug, 0),
         Survey_Start_Time = ymd_hms(Survey_Start_Time)) %>%
  filter(Study_Area == "Russell Creek",
         is.na(depth) == F,
         Survey_Start_Time > "2021-05-12 00:00:00")

# fix cores done in multiple tries
multi <- dat %>%
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
all_final <- dat %>%
  filter(multi_core == "no" | is.na(multi_core) == T) %>% # get non multi cores and probes
  mutate(
    depth = depth - plug,
    SWE = mass / (3.14159265359 * (2.1 * 2.1)),
    density = SWE / (depth)
  ) %>%
  rbind(multi_final) %>%
  left_join(coord_final, by = "Plot_ID") %>%
  select(Survey_Start_Time:Plot_ID, easting:lat, plot_type:density) %>%
  rbind(wx_final)



# summary

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

write.csv(all_final, "summary/2021_tsitika_trip4_raw.csv", row.names = F)
write.csv(plt_avg, "summary/2021_tsitika_trip4_summary.csv", row.names = F)

