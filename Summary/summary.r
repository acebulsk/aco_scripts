# summary for all study areas

library(dplyr)
library(ggplot2)
library(tmap)
library(readr)
library(lubridate)

df <- read_csv("2021_aco_plot_summaries_20210504.csv") %>% 
  mutate(Survey_Start_Time = mdy_hm(Survey_Start_Time),
         day = day(Survey_Start_Time),
         SWE = case_when(
           Study_Area == "Metro Vancouver" ~ SWE / 10,
           TRUE ~ SWE
         )) 

daily <- df %>% 
  filter(Study_Area != "Metro Vancouver") %>% 
  group_by(ACO_Flight_Number, Study_Area, day) %>% 
  summarise(
    date = first(Survey_Start_Time),
    total_depth = sum(depth),
    total_swe = sum(SWE, na.rm = T)
  )

daily_van <- df %>% 
  group_by(Plot_ID)

write_csv(daily, "daily_depth_swe.csv")


ggplot(daily, aes(x = date, y = total_depth, colour = Study_Area)) +
  geom_point() +
  facet_grid(~ACO_Flight_Number, scales = "free")

ggsave("figs/daily_depth_totals.png", width = 11, height = 5)

daily %>% 
  filter(Study_Area != "Metro Vancouver") %>% # no daily res for metro 
  ggplot(aes(x = date, y = total_swe, colour = Study_Area)) +
  geom_point() +
  facet_grid(~ACO_Flight_Number, scales = "free")

ggsave("figs/daily_swe_totals.png", width = 11, height = 5)



flights <- df %>% 
  group_by(ACO_Flight_Number, Study_Area) %>% 
  summarise(
    date = first(Survey_Start_Time),
    total_depth = sum(depth),
    total_swe = sum(SWE, na.rm = T)
  )

ggplot(flights, aes(ACO_Flight_Number, total_depth, colour = Study_Area)) +
  geom_point()

ggsave("figs/trip_depth_totals.png", width = 8, height = 4)

flights %>% 
  filter(Study_Area != "Metro Vancouver") %>% # no daily res for metro 
  ggplot(aes(ACO_Flight_Number, total_swe, colour = Study_Area)) +
  geom_point()

ggsave("figs/trip_swe_totals.png", width = 8, height = 4)

paste("total depth in km:", sum(df$depth) / 100000)



