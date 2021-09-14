WIP: Clean and process the Mobi bikeshare dataset
================
Andrew Luyt
2021-09-13

Download the data yourself as described in the README and run this
notebook, which will remove some nulls, transform & combine the data
into more convenient form, then save various `.Rdata`, `.geoJSON`, and
`.csv` files for use elsewhere.

``` r
library(data.table)
library(tidyverse)
library(lubridate)
library(geosphere)  # for distGeo()
library(sf)         # for plotting maps
library(gganimate)

SHAPEDATA <- "data/local-area-boundary.geojson"
```

## Read all the raw Mobi CSVs

``` r
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
  mutate(depart_time = ymd_hm(depart_time),
         return_time = ymd_hm(return_time),
         wday = lubridate::wday(depart_time, label = TRUE, abbr = TRUE),
         hour = lubridate::hour(depart_time),
         trip_minutes =  trip_seconds / 60,
         stopover_minutes = stopover_seconds / 60,
         kph = trip_distance / (trip_minutes - stopover_minutes) / 1000 * 60) %>%
  filter(trip_minutes > 0, trip_minutes < 60*12) %>%
  extract(station_depart, into = c("id_depart", "station_depart"),
          regex = "([0-9]+) (.*)") %>%
  extract(station_return, into = c("id_return", "station_return"),
          regex = "([0-9]+) (.+)")
```

    ## Warning: 1 failed to parse.

## Add map angle of trip

This is the angle (from the positive x-axis, pointing east) of the line
drawn on a map from the start of the trip to the end. The station data
(scraped from Mobi’s web site) includes the coordinates of bike
stations.

``` r
stations <- read_csv("data/scraped_stations.csv")

# Join lat/lon of start and end stations to rides
df <- df %>%
  left_join(stations, by = c("id_depart" = "id")) %>%
  left_join(stations, by = c("id_return" = "id")) %>%
  select(depart_time, return_time, bike, id_depart, id_return, station_depart,
         station_return, lon_depart = longitude.x, lat_depart = latitude.x,
         lon_return = longitude.y, lat_return = latitude.y, membership,
         trip_distance, trip_minutes, n_stopovers, stopover_minutes, kph) %>%
  drop_na() # mostly this removes rows with missing lon/lat coordinates
```

Create vector *components* for each ride. This lets us, during analysis,
aggregate trips (e.g. all rides from station *001* during one hour) and
easily obtain an overall vector by simply adding up the components. We
use distGeo to account for the curvature of the earth, though it doesn’t
matter much at these small scales.

``` r
df <- df %>%
  mutate(delta_x = distGeo(cbind(lon_depart, lat_depart),
                           cbind(lon_return, lat_depart)) / 1000,
         delta_y = distGeo(cbind(lon_depart, lat_depart),
                           cbind(lon_depart, lat_return)) / 1000,
         delta_x =  # east positive, west negative
           if_else(lon_return > lon_depart, delta_x, (-1) * delta_x),
         delta_y =  # north positive, south negative
           if_else(lat_return > lat_depart, delta_y, (-1) * delta_y))
```

Change some names in the map data and remove a list column

``` r
van_map <- st_read(SHAPEDATA) %>%
  mutate(name = replace(name, which(name == "Arbutus-Ridge"), "Arbutus Ridge"),
         name = replace(name, which(name == "Kensington-Cedar Cottage"),
                        "Kensington")) %>%
  select(-geo_point_2d)
```

## Export cleaned data

``` r
st_write(obj = van_map, dsn = "data/vancouver_map.geojson", delete_dsn = TRUE)
save(df, file = "data/mobi_rides.Rdata")
write_csv(df, file = "data/mobi_rides.csv")
```
