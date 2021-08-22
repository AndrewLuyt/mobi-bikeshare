library(tidyverse)
library(lubridate)

df <- read_csv("data/Mobi_System_Data_2021-04.csv", trim_ws = TRUE, skip = 1,
               col_names = c('depart_time','return_time','bike','station_depart',
                  'station_return','membership','trip_distance','trip_seconds',
                  'voltage_depart_mv','voltage_return_mv','temp_depart',
                  'temp_return','stopover_seconds','n_stopovers')) %>%
  mutate(depart_time = ymd_hm(depart_time),
         return_time = ymd_hm(return_time),
         trip_minutes =  trip_seconds / 60,
         stopover_minutes = stopover_seconds / 60,
         kph = trip_distance / (trip_minutes - stopover_minutes) / 1000 * 60) %>%
  filter(trip_minutes > 0, trip_minutes < 60*12) %>%
  extract(station_depart,
          into = c("id_depart", "station_depart"),
          regex = "([0-9]+) (.*)") %>%
  extract(station_return, into = c("id_return", "station_return"),
          regex = "([0-9]+) (.+)")

#' ## Add trip angles (start to end stations)
#' ### First correct station names - they're inconsistent between datasets
stations <- read_csv("data/scraped_stations.csv")

#' match IDs, show station names that don't match. There's only one though,
#' station 0084.
#'
df %>%
  select(id_depart, station_depart) %>%
  left_join(stations, by = c("id_depart" = "id")) %>%
  filter(station_depart != name) %>%
  # select(id_depart, station_depart, name) %>%
  distinct()

#' ### Correct names in `stations`
#' Use the intel gathered above to fix the names scraped from the web
stations <- stations %>%
  mutate(name = replace(name, which(name == "Beach & Broughton"), "Sunset Beach West"))

#' Check IDs in both tibbles for names that are **missing** in `stations`
missing1 <- setdiff(df$station_depart, stations$name) # in df, not in stations
missing2 <- setdiff(df$station_return, stations$name) # in df, not in stations
missing <- unique(c(missing1, missing2))

# These will be stations we won't have lat/lon coords for
no_coord_stations <- df %>%
  filter(station_depart %in% missing) %>%
  left_join(stations, by = c("id_depart" = "id")) %>%
  # select(id_depart, station_depart, name) %>%
  distinct() %>%
  select(id_depart)
dim(no_coord_stations) # about 2% of rides w/o geodata

#' ## Todo: join to get lat/lon, then compute angle of each ride,
#' then we can do some animations a la Divvy


angle_from_x_axis <- function(y,x) {
  angle <- atan2(y, x)
  if (y<0) {
    angle <- angle + 2 * pi
  }
  angle * 180 / pi
}


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
  ggplot(aes(trip_seconds/ 60)) +
  geom_histogram()

# mean trip time by return hour
# busy time is still the end of, or immediately after, the workday: 5-7pm
# Is this spike in trips a one-way commute FROM work TO home?
# Do people do this because they don't want to be sweaty at work?
df %>%
  ggplot(aes(return_hour)) +
  geom_bar()
