library(dplyr)

raw <- read.csv("E:/Alex_Working/ACO/Cruickshank/March 08-12th/3 Published/2021_cru_trip1_raw_data.csv", row.names = "X")

sum <- read.csv("E:/Alex_Working/ACO/Cruickshank/March 08-12th/3 Published/2021_cru_trip1_summary_ppk.csv")

# get coords converted to itrf 2014 from trx nad84 csrs <- itrf2014
coords <- read.csv("cru_plot_coords_ITRF2014.csv")

raw_coord <- left_join(raw, coords, by = "Plot_ID") %>%
  select(Survey_Start_Time:Plot_ID, easting:lat, plot_type:mass) %>%
  mutate(
    SWE = mass / (3.14159265359 * (2.1 * 2.1)),
    density = SWE / (depth - plug)
  )

sum_coord <- left_join(sum, coords, by = "Plot_ID") %>%
  mutate(count = count_depth + count_density,
         Study_Area = "Cruickshank") %>%
  select(plot_datetime, Study_Area, Plot_ID, easting = easting.y, northing = northing.y, elev_ellipsoidal:lat, plot_type:density, count)


write.csv(sum_coord, "E:/Alex_Working/ACO/Cruickshank/March 08-12th/3 Published/2021_cru_trip1_summary_itrf2014.csv")

# bind trip 1 and 2 raw obs

raw_trip2 <- read.csv("April_5-9_trip2/3 Published/cru_trip2_plots_raw.csv") %>% select(-sample_type)

all_raw <- rbind(raw_coord, raw_trip2)

write.csv(all_raw, "Deliverables/cru_point_observations_raw.csv")
