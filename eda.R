library(tidyverse)
library(lubridate)
library(sf)         # Vancouver map
library(gganimate)
library(viridis)

source("mobi_functions.R")

RIDES = "data/mobi_rides.Rdata"
if(file.exists(RIDES)) {
  load(RIDES)
  df <- as_tibble(df) %>%
    mutate(hour = hour(depart_time))
} else {
  stop("Please run clean_data.R to create the required data")
}

MAP = "data/vancouver_map.geojson"
if(file.exists(MAP)) {
  MAP <- st_read(MAP)
} else {
  stop("Please run clean_data.R to create the required data")
}

STANLEY_PARK = "data/parks-polygon-representation.geojson"
if(file.exists(STANLEY_PARK)) {
  STANLEY_PARK <- st_read(STANLEY_PARK)
  STANLEY_PARK <- STANLEY_PARK %>% filter(park_name == 'Stanley Park')
}

rm(RIDES)

# Helper function for vectors
angle_from_x_axis <- function(y,x) {
  angle <- atan2(y, x)
  if (y<0) {
    angle <- angle + 2 * pi
  }
  angle * 180 / pi
}

# Filter to the last year only ######################################
df <- df %>%
  filter(depart_time >= max(depart_time) - years(1))

# busiest stations by hour

busy_depart <- df %>% group_by(hour, station_depart) %>% count() %>% ungroup()
busy_return <- df %>% group_by(hour, station_return) %>% count() %>% ungroup()
busy <- full_join(busy_depart, busy_return, by=c("hour", "station_depart" = "station_return"))
busy %>% mutate(trips = n.x + n.y)
busy %>% group_by(hour) %>% slice_max(trips) %>% print(n = Inf) %>%
  ggplot(aes(hour, trips, label = station_depart)) +
  geom_line() + geom_text() +
  labs(title = "Busiest stations, by hour")

# busiest memberships by hour
busy <- df %>% group_by(hour, membership) %>% count() %>% ungroup()

# busiest memberships by hour, weekdays
df %>%
  # filter(wday(depart_time, label = T) %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) %>%
  filter(!wday(depart_time, label = T) %in% c("Sat", "Sun")) %>%
  group_by(hour, membership) %>%
  count() %>%
  ungroup() %>%
  group_by(hour) %>%
  slice_max(n) %>%
  ggplot(aes(hour, n, label=membership)) +
  geom_line() + geom_point(alpha=.5, col='red', cex=3) + geom_text() +
  labs(title = "Busiest membership type, by hour")
