
# Libraries ------------------------------------------------
library(tidyverse)
library(janitor)
library(lubridate)
library(hms)
library(here)

# Data -----------------------------------------------------
flights <- read_csv("data/flights.csv")
airlines <- read_csv("data/airlines.csv")
planes <- read_csv("data/planes.csv")
weather <- read_csv("data/weather.csv")
additional_weather <- read_csv("data/export_weather.csv")


# Flight data cleaning -------------------------------------
flights_clean <- flights %>% 
  clean_names() %>% 
  # removing arrival data and other unnecessary variables
  select(-c(arr_time, sched_arr_time, 
            arr_delay, air_time, 
            minute)) %>% 
  # adding date variable
  mutate(date = make_datetime(year, month, day)) %>% 
  # mutating departure variables to datetime
  mutate(dep_time = 
  sprintf("%04d", dep_time),
dep_time = paste0(
  substr(dep_time, 1, 2), ":", 
  substr(dep_time, 3, 4)),
dep_time = as.POSIXct(dep_time,
                      format = "%H:%M",
                      tz = "US/Eastern"),
dep_time = as_hms(as.POSIXct(dep_time))
) %>%
  mutate(sched_dep_time = 
           sprintf("%04d", sched_dep_time),
         sched_dep_time = paste0(
           substr(sched_dep_time, 1, 2), ":", 
           substr(sched_dep_time, 3, 4)),
         sched_dep_time = as.POSIXct(sched_dep_time,
                               format = "%H:%M",
                               tz = "US/Eastern"),
         sched_dep_time = as_hms(as.POSIXct(sched_dep_time))
  ) %>% 
  # imputing NAs in dep_delay to be calculation of dep_time - sched_dep_time
  mutate(dep_delay = ifelse(is.na(dep_delay), 
                            dep_time - sched_dep_time, 
                            dep_delay)) %>% 
  # inserting status column
  mutate(status = case_when(
    is.na(dep_time) ~ "cancelled",
    dep_delay >= 15 ~ "delayed",
    dep_delay < 15 ~ "on time",
    TRUE ~ "unknown")) %>% 
  # mutating month value to words for data viz
  mutate(month = as.factor(month(date, 
                         label = TRUE))) %>% 
  # adding weekday column
  mutate(weekday = as.factor(wday(
    date, label = TRUE, abbr = TRUE)),
    .after = day)

# Weather cleaning data ------------------------------------
weather_clean <- weather %>% 
  clean_names() %>% 
  #removing columns with large number of NAs
  select(-c(year, month, day, hour,
            temp, dewp, humid, precip, pressure)) %>% 
  # dropping NA rows from weather data
  drop_na(visib) %>% 
  drop_na(wind_dir) %>% 
  drop_na(wind_speed) %>% 
  drop_na(wind_gust) %>% 
  #adding date variable for additional weather join
  mutate(date = as.Date(time_hour)) %>% 
  # joining additional weather data
  left_join(additional_weather, by = "date") %>% 
  # removing date column as not necessary post join
  select(-date)

# Planes cleaning data -------------------------------------
planes_clean <- planes %>% 
  clean_names() %>% 
  #removing unnecessary variables
  select(-c(year, seats, speed))

# Joining flight, weather & airline data-------------------
flight_weather <- flights_clean %>% 
  left_join(weather_clean, 
            by = c("origin", "time_hour")) %>% 
  left_join(airlines, by = "carrier") %>% 
  rename(carrier_name = name) %>% 
  # removing flights with missing wind_data
  drop_na(visib) %>% 
  drop_na(wind_dir) %>% 
  drop_na(wind_speed) %>% 
  drop_na(wind_gust) 

# Joining plane data ------------------------------------------------------
all_info <- flight_weather %>% 
  left_join(planes_clean, by = "tailnum") %>% 
  # imputing NA values
  mutate(tailnum = if_else(
  is.na(tailnum), "unknown", tailnum)) %>% 
  mutate(type = if_else(
    is.na(type), "unknown", type)) %>%
  mutate(manufacturer = if_else(
    is.na(manufacturer), "unknown", manufacturer)) %>%
  mutate(model = if_else(
    is.na(model), "unknown", model)) %>%
  mutate(engine = if_else(
    is.na(engine), "unknown", engine)) %>% 
  mutate(engines = if_else(
    is.na(engines), "unknown", as.character(engines)))


# Saving clean data scripts ------------------------------------------------

all_info %>% 
  write_csv(here("clean_data/all_info_clean.csv"))

all_info %>% 
  filter(origin == "EWR") %>% 
  write_csv(here("clean_data/newark_info.csv"))
