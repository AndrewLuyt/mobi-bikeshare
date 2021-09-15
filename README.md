Work in Progress: Case Study: Mobi Bikeshare in Vancouver
================
Andrew Luyt
07/07/2021

## Introduction

Mobi is a bikeshare in Vancouver, Canada. Launched in 2016, it is a
cooperative effort between the City of Vancouver and Shaw Go.

TBC…

## Goals of the Study

TBD, but some ideas:

-   How has usage changed since 2016?
    -   How has the network changed (where the stations are, \# of
        bikes..)
    -   Has usage gone up?
        -   is it correlated to the expansion of the station network?
-   Analyze usage patterns
    -   when is bike rush hour? where?
        -   visualize this on a map with scrubbable hour?
    -   are there spikes on certain days?
        -   a pattern, like on weekends?
        -   holidays or other special dates?
    -   how did COVID-19 affect MOBI usage?
        -   highlight dates when lockdowns were initiated/removed in BC
    -   visualize traffic hot spots
        -   departure
        -   arrival
            -   is there a difference, a favored direction, etc?
    -   how long are trips (time and distance)
        -   does it change by hour? day of the week?
        -   what were the longest trips?
-   Analyze memberships
    -   how do they change over time (2016 to now?)
    -   have membership **renewal** rates gone up?
-   What do the voltage columns mean?
    -   voltage\_return is **higher** than depart - why?
-   stopovers
    -   are people primarily commuting, or running errands with a
        stopover?
        -   what’s the distribution?
-   can you identify commutes, errands, and pleasure rides?
    -   commutes are probably from one station to a different one, no
        stopovers
    -   errands are probably from one station to the same station, with
        a stopover
    -   pleasure trips are probably like errands but with 0 stopovers
    -   do these categories form clear patterns in the data?
        -   how could you verify these observations if they are there?
            -   is there a usage survey that says how the mobi
                membership USES their bikes?
            -   are there patterns visible on the map?
            -   are there **time** or **place** (departure/arrival)
                patterns correlated with each usage group?
-   bicycle attrition?
    -   the `bike` column seems to record a unique bike ID - does it?
    -   how long do bikes last? do they disappear from the dataset?
        -   are all bikes used every day?
