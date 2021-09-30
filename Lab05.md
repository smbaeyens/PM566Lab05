Lab05
================
Sylvia Baeyens
9/24/2021

Loading Data Files & Merging

``` r
# Dowloading met data set
if (!file.exists("../met_all.gz")) {
  download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", "../met_all.gz", method="libcurl", timeout = 60)}
met <- data.table::fread("../met_all.gz")

# Download the data
stations <- data.table::fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Merging
met <- merge(
  x = met,
  y = stations,
  all.x = TRUE, all.y = FALSE,
  by.x = "USAFID", by.y = "USAF"
)
```

# 1. Representative Station for the US

First, we find mean values for variables of interest for each station.

``` r
stationAvg <- met[,.(
  temp = mean(temp, na.rm = TRUE),
  wind.sp = mean(wind.sp, na.rm = TRUE),
  atm.press = mean(atm.press, na.rm = TRUE)
), by = USAFID]
```

Identifying quantiles for variables of interest

``` r
stationAvg[,.(
  temp50 = quantile(temp, probs= .5, na.rm= TRUE),
  wind.sp = quantile(wind.sp, probs= .5, na.rm= TRUE),
  atm.press = quantile(atm.press, probs= .5, na.rm= TRUE)
)]
```

    ##      temp50  wind.sp atm.press
    ## 1: 23.68406 2.461838  1014.691
