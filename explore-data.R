library(tidyverse)
library(lubridate)

# transformations performed:
# 1. Add depart_hour and return_hour fields
load_mobi_dset <- function(f) {
  dt_format = col_datetime(format = "%Y-%m-%d %H:%M")
  tmp <- read_csv(
    f,
    trim_ws = TRUE,
    skip = 1,
    col_names = c(
      'depart_time',
      'return_time',
      'bike',
      'station_depart',
      'station_return',
      'membership',
      'trip_distance',
      'trip_seconds',
      'voltage_depart_mv',
      'voltage_return_mv',
      'temp_depart',
      'temp_return',
      'stopover_seconds',
      'n_stopovers'
    ),
    col_types = cols(depart_time = dt_format,
                     return_time = dt_format,
                     membership = 'f')
  )

  tmp <- tmp %>%
    mutate(depart_hour = hour(depart_time),
           return_hour = hour(return_time))

  # TODO remove nulls where appropriate

  tmp

}

df = load_mobi_dset("data/Mobi_System_Data_2021-04.csv")

# mean trip time, by membership type
df %>%
  filter(! is.na(membership)) %>%
  group_by(membership) %>%
  summarise(mean_trip_minutes = mean(trip_seconds) / 60) %>%
  arrange(desc(mean_trip_minutes)) %>%
  ggplot(aes(x=reorder(membership, mean_trip_minutes), mean_trip_minutes)) +
  geom_col() +
  coord_flip()

# mean stopovers by membership
# Look at "365 Day Founding Plus" !!!
df %>%
  filter(! is.na(membership)) %>%
  group_by(membership) %>%
  summarise(mean_stopovers = mean(n_stopovers)) %>%
  arrange(desc(mean_stopovers)) %>%
  ggplot(aes(x=reorder(membership, mean_stopovers), mean_stopovers)) +
  geom_col() +
  coord_flip()

# distribution of stopovers by membership
df %>%
  filter(! is.na(membership)) %>%
  filter(n_stopovers > 0) %>%
  ggplot(aes(x=n_stopovers)) +
  geom_density() +
  facet_wrap(~membership)

# mean trip time by departure hour
# busy time is the end of, or immediately after, the workday: 5-7pm
# morning use is WAY lower. Are people commuting one way?
df %>%
  ggplot(aes(depart_hour)) +
  geom_bar()

# mean trip time by return hour
# busy time is still the end of, or immediately after, the workday: 5-7pm
# Is this spike in trips a one-way commute FROM work TO home?
# Do people do this because they don't want to be sweaty at work?
df %>%
  ggplot(aes(return_hour)) +
  geom_bar()
