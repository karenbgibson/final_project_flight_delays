flights <- read_csv("data/flights.csv") %>% 
  clean_names() %>% 
  # removing arrival data and other unnecessary variables
  select(-c(arr_time, sched_arr_time, 
             arr_delay, air_time, time_hour)) %>% 
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
  # making variable names clearly pre-joining
  rename(carrier_code = carrier,
         origin_airport_code = origin,
         dest_airport_code = dest)

