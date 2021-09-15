library(tidyverse)
library(lubridate)
library(sf)
library(gganimate)
library(viridis)

MAP.FILL = "gray20"

# for testing
ARRIVE_DF <- df %>%
  filter(month(depart_time) %in% 6:7,
         #membership %in% c("365 Day Founding Plus", "365 Plus"),
         id_depart != id_return) %>%  # remove round trips
  group_by(hour, id_return) %>%
  summarise(x = sum(lon_return - lon_depart),
            y = sum(lat_return - lat_depart),
            nrides = n(),
            lon_depart = first(lon_depart),
            lat_depart = first(lat_depart),
            lon_return = first(lon_return),
            lat_return = first(lat_return)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(angle = angle_from_x_axis(y,x)) %>%
  ungroup() %>%
  # bin angles into 4 quadrants for colouring
  mutate(angle_group = cut_interval(angle, n = 4, labels = 1:4)) %>%
  # filter(magnitude >= 50) %>% # higher numbers make the map less cluttered
  arrange(hour, desc(angle))

LEAVE_DF <- df %>%
  filter(month(depart_time) %in% 6:7,
         #membership %in% c("365 Day Founding Plus", "365 Plus"),
         id_depart != id_return) %>%  # remove round trips
  group_by(hour, id_depart) %>%
  summarise(x = sum(lon_return - lon_depart),
            y = sum(lat_return - lat_depart),
            nrides = n(),
            lon_depart = first(lon_depart),
            lat_depart = first(lat_depart),
            lon_return = first(lon_return),
            lat_return = first(lat_return)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(angle = angle_from_x_axis(y,x)) %>%
  ungroup() %>%
  # bin angles into 4 quadrants for colouring
  mutate(angle_group = cut_interval(angle, n = 4, labels = 1:4)) %>%
  # filter(magnitude >= 50) %>% # higher numbers make the map less cluttered
  arrange(depart_time, desc(angle))

# Returns gganimation of traffic arriving at a Mobi station
animated.map <- function(
  data,
  direction = "arriving",
  caption = "Andrew Luyt, 2021  |  Source: Mobi public data",
  transition_frames = 8,
  state_frames = 1,
  arrow.scale = 0.003
) {
  arrow.end <-  if (direction == "arriving") "first" else "last"

  # quo() and !! (unquote) work together to allow for selective use
  # of variable names in %>% pipes %>%
  # See vignette("programming")
  if (direction == "arriving"){
    group_station <- quo(id_return)
    xvar = quo(lon_return)
    yvar = quo(lat_return)
    xend_var = quo(xend_arriving)
    yend_var = quo(yend_arriving)
    title = "Traffic arriving at Mobi bike stations, June-July 2021     Hour: {next_state}"
    subtitle = "Arrows show the average direction of traffic into each station in the Mobi bike sharing system.\nDirection is averaged as a straight line from all start stations to end station.\nLonger arrows mean a stronger tendency for traffic to travel that direction."
  } else if (direction == "departing"){
    group_station <- quo(id_depart)
    xvar = quo(lon_depart)
    yvar = quo(lat_depart)
    xend_var = quo(xend_departing)
    yend_var = quo(yend_departing)
    title = "Traffic departing from Mobi bike stations, June-July 2021     Hour: {next_state}"
    subtitle = "Arrows show the average direction of traffic out of each station in the Mobi bike sharing system.\nDirection is averaged as a straight line from all start stations to end station.\nLonger arrows mean a stronger tendency for traffic to travel that direction."
  } else {
    stop("Direction must be 'arriving' or 'departing'")
  }

  # calculate vectors - slightly different for leaving vs arriving
  TMP_DF <- data %>%
    filter(month(depart_time) %in% 6:7,
           #membership %in% c("365 Day Founding Plus", "365 Plus"),
           id_depart != id_return) %>%  # remove round trips
    group_by(hour, !!group_station) %>%
    summarise(x = sum(lon_return - lon_depart),
              y = sum(lat_return - lat_depart),
              nrides = n(),
              lon_depart = first(lon_depart),
              lat_depart = first(lat_depart),
              lon_return = first(lon_return),
              lat_return = first(lat_return)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(angle = angle_from_x_axis(y,x),
           # calculate the endpoints of vectors for the two cases,
           # arriving at stations or departing from stations
           xend_arriving = lon_return - x * arrow.scale,
           yend_arriving = lat_return - y * arrow.scale,
           xend_departing = lon_depart + x * arrow.scale,
           yend_departing = lat_depart + y * arrow.scale) %>%
    ungroup() %>%
    # bin angles into 4 quadrants for colouring
    mutate(angle_group = cut_interval(angle, n = 4, labels = 1:4))

  p <- TMP_DF %>%
    ggplot(
      aes(x = !!xvar,
          y = !!yvar,
          # xend = if (direction == "arriving") lon_return - x * arrow.scale else lon_depart + x * arrow.scale,
          # yend = if (direction == "arriving") lat_return - y * arrow.scale else lat_depart + x * arrow.scale,
          xend = !!xend_var,
          yend = !!yend_var,
          color = angle_group,
          alpha = nrides,
          # size = nrides,
          group = !!group_station)) +
    geom_sf(data = MAP, mapping = aes(), inherit.aes = FALSE, fill = MAP.FILL) +
    geom_sf(data = STANLEY_PARK, mapping = aes(), inherit.aes = FALSE, fill = MAP.FILL) +
    geom_segment(size=0.7,
                 arrow = arrow(length = unit(0.01, "npc"), type = "closed", ends = arrow.end)) +
    scale_alpha_continuous(range = c(0.4, 1)) +
    # scale_size_continuous(range = c(0.4, 0.7)) +
    xlim(c(-123.19, -123.050)) +
    ylim(c(49.245, 49.315)) +
    theme_map_dark +
    annotate("text", x = -123.1684, y = 49.3117, label = "Vancouver", color = "black", cex = 18) +
    annotate("text", x = -123.169, y = 49.312, label = "Vancouver", color = "grey60", cex = 18) +
    labs(subtitle = subtitle,
         caption = caption) +
    transition_states(hour, transition_length = transition_frames, state_length = state_frames) +
    ggtitle(title) +
    shadow_wake(wake_length = 0.01,  wrap = TRUE) +
    enter_fade() +
    exit_fade()
}

# p <- animated.map(df, direction = "departing")
# p
