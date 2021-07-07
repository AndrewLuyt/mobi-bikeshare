library(tidyverse)

load_mobi_dset <- function(f) {
  dt_format = col_datetime(format = "%Y-%m-%d %H:%M")
  read_csv(
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
                     return_time = dt_format)
  )
}

df = load_mobi_dset("data/Mobi_System_Data_2021-04.csv")

