library(data.table)
library(tidyverse)
library(lubridate)
library(geosphere)  # for distGeo()
library(sf)         # Vancouver map
library(gganimate)

SHAPEDATA <- "data/local-area-boundary.geojson"

df <-
  list.files(path = "./data",
             pattern = "^Mobi_System_Data.*.csv",
             # pattern = "*.csv",
             full.names = TRUE) %>%
  map_df(~fread(
    .,
    stringsAsFactors = TRUE,
    col.names = c('depart_time','return_time','bike','station_depart',
                  'station_return','membership','trip_distance','trip_seconds',
                  'voltage_depart_mv','voltage_return_mv','temp_depart',
                  'temp_return','stopover_seconds','n_stopovers'))) %>%
  mutate(trip_minutes =  trip_seconds / 60,
         stopover_minutes = stopover_seconds / 60,
         kph = trip_distance / (trip_minutes - stopover_minutes) / 1000 * 60) %>%
  filter(trip_minutes > 0, trip_minutes < 60*12) %>%
  extract(station_depart,
          into = c("id_depart", "station_depart"),
          regex = "([0-9]+) (.*)") %>%
  extract(station_return, into = c("id_return", "station_return"),
          regex = "([0-9]+) (.+)")

#' ## Add trip map angle (from start station to end station)
#' ### Correct station names - they're inconsistent between datasets
stations <- read_csv("data/scraped_stations.csv")

#' match IDs, show station names that don't match. There's only one though,
#' station 0084.
#'
# missing <-  df %>%
#   select(id_depart, station_depart) %>%
#   left_join(stations, by = c("id_depart" = "id")) %>%
#   filter(station_depart != name) %>%
#   # select(id_depart, station_depart, name) %>%
#   distinct() %>%
#   pull(var = name)

#' ### Correct names in `stations`
#' Use the intel gathered above to fix the names scraped from the web
# stations <- stations %>%
#   mutate(name = replace(name, which(name %in% missing), "Sunset Beach West"))

#' Check start/return IDs for station IDs that are **missing** in `stations`
# missing1 <- setdiff(df$id_depart, stations$id) # in df, not in stations
# missing2 <- setdiff(df$id_return, stations$id) #
# missing <- unique(c(missing1, missing2))

# These will be stations we won't have lat/lon coords for
# no_coord_stations_depart <- df %>%
#   filter(id_depart %in% missing) %>%
#   left_join(stations, by = c("id_depart" = "id")) %>%
#   # select(id_depart, station_depart, name) %>%
#   select(id = id_depart) %>%
#   distinct()

# no_coord_stations_return <- df %>%
  # filter(id_return %in% missing) %>%
  # left_join(stations, by = c("id_return" = "id")) %>%
  # # select(id_depart, station_depart, name) %>%
  # select(id = id_return) %>%
  # distinct()

# no_coord_stations <-
#   rbind(no_coord_stations_depart, no_coord_stations_return) %>%
#   distinct() %>%
#   pull(var = id)
#
# no_coord_rides <- df %>% filter(id_depart %in% no_coord_stations | id_return %in% no_coord_stations)


#' Join lat/lon of start and end stations to rides
df <- df %>%
  left_join(stations, by = c("id_depart" = "id")) %>%
  left_join(stations, by = c("id_return" = "id")) %>%
  select(depart_time, return_time, bike, id_depart, id_return, station_depart,
         station_return, lon_depart = longitude.x, lat_depart = latitude.x,
         lon_return = longitude.y, lat_return = latitude.y, membership,
         trip_distance, trip_minutes, n_stopovers, stopover_minutes, kph) %>%
  drop_na() # mostly this removes rows with missing lon/lat coordinates

#' Helper function
angle_from_x_axis <- function(y,x) {
  angle <- atan2(y, x)
  if (y<0) {
    angle <- angle + 2 * pi
  }
  angle * 180 / pi
}

#' Prepare to create vectors from their component forms. This lets us
#' aggregate trips (e.g. all rides from station 001 during one hour) and
#' easily obtain an overall vector by simply adding up the components.
#' We use distGeo to account for the curvature of the earth, though it
#' doesn't matter much at these small scales.
df <- df %>%
  mutate(delta_x = distGeo(cbind(lon_depart, lat_depart), cbind(lon_return, lat_depart)) / 1000,
         delta_y = distGeo(cbind(lon_depart, lat_depart), cbind(lon_depart, lat_return)) / 1000,
         delta_x =  # east positive, west negative
           if_else(lon_return > lon_depart, delta_x, (-1) * delta_x),
         delta_y =  # north positive, south negative
           if_else(lat_return > lat_depart, delta_y, (-1) * delta_y))

#' Change some names in the map data and remove a list column
van_map <- st_read(SHAPEDATA) %>%
  mutate(name = replace(name, which(name == "Arbutus-Ridge"), "Arbutus Ridge"),
         name = replace(name, which(name == "Kensington-Cedar Cottage"),
                        "Kensington")) %>%
  select(-geo_point_2d)

st_write(obj = van_map, dsn = "data/vancouver_map.geojson", delete_dsn = TRUE)
save(df, file = "data/mobi_rides.Rdata")
write_csv(df, file = "data/mobi_rides.csv")


TMP_DF <- df %>%
  drop_na() %>%
  filter(id_depart != id_return) %>%  # remove round trips
  group_by(hour = hour(depart_time), id_depart) %>%
  mutate(x = sum(delta_x),
         y = sum(delta_y),
         magnitude = sqrt(x^2 + y^2)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(angle = angle_from_x_axis(y,x)) %>%
  ungroup() %>%
  mutate(angle_group = cut_interval(angle, n = 4, labels = 1:4)) %>%  # bin angles into 4 quadrants for colouring?
  # filter(magnitude >= 50) %>% # higher numbers make the map less cluttered
  arrange(depart_time, desc(angle))


p <- TMP_DF %>%
  ggplot(aes(x = lon_depart,
             y = lat_depart,
             xend = lon_depart + sign(x) * ((log10(abs(x))/350)),
             yend = lat_depart + sign(y) * ((log10(abs(y))/350)),
             color = angle_group,
             group = id_depart)) +
  geom_sf(data = van_map, mapping = aes(), inherit.aes = FALSE) +
  geom_segment(arrow = arrow(length = unit(0.012, "npc")), size=0.6) +
  xlim(c(-123.2, -123.045)) +
  ylim(c(49.245, 49.315)) +
  transition_states(hour, transition_length = 3, state_length = 3) +
  ggtitle("Averaged Mobi traffic flow    {next_state}") +
  shadow_wake(wake_length = 0.01,  wrap = TRUE) +
  enter_fade() +
  exit_fade() +
  enter_grow() +
  exit_shrink()

FPS = 30; W = 960; H = 660; DTL = 1; NFRAMES = 1*24*12 # bins per hour * 24 hours * ()
animate(plot = p, fps = FPS, nframes = NFRAMES, detail=DTL, width=W, height=H)

#' ## TODO
#' color or facet by membership, etc
#' use a different type of map like the watercolor tiles so you can get
#' a cool look AND stanley park?

#' -----------------------------------------------

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
