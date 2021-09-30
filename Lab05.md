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
medians = stationAvg[,.(
  temp50 = quantile(temp, probs= .5, na.rm= TRUE),
  wind50 = quantile(wind.sp, probs= .5, na.rm= TRUE),
  atm50 = quantile(atm.press, probs= .5, na.rm= TRUE)
)]
```

Finding station closest to mean

``` r
stationAvg[, temp_dist := abs(temp- medians$temp50)]
medianTempStation = stationAvg[order(temp_dist)][1]

stationAvg[, wind_dist := abs(wind.sp- medians$wind50)]
medianWindStation = stationAvg[order(wind_dist)][1]

stationAvg[, atm_dist := abs(atm.press- medians$atm50)]
medianAtmStation = stationAvg[order(atm_dist)][1]
```

The station that is best representative of the median temperature of all
stations is 720458 The station that is best representative of the median
wind speed of all stations is 720929 The station that is best
representative of the median atmospheric pressure of all stations is
723200

# 2. Representative Station per State

First, we recover state variable via MERGE

``` r
stationStates = merge(
  x = stationAvg, y = stations,
  by.x = "USAFID", by.y = "USAF",
  all.x = TRUE, all.y = FALSE
  )
```

Now, computing medians per state & eucledian distance

``` r
stationStates[, temp50 := quantile(temp, probs = .5, na.rm = TRUE), by = STATE]
stationStates[, wind50 := quantile(wind.sp, probs = .5, na.rm = TRUE), by = STATE]
stationStates[, atm50 := quantile(atm.press, probs = .5, na.rm = TRUE), by = STATE]

stationStates[, eudist := sqrt(
  (temp - temp50)^2 + (wind.sp - wind50)^2
  )]
stationStates
```

    ##       USAFID     temp  wind.sp atm.press temp_dist   wind_dist  atm_dist CTRY
    ##    1: 690150 33.18763 3.483560  1010.379 9.5035752 1.021721847 4.3124708   US
    ##    2: 720110 31.22003 2.138348       NaN 7.5359677 0.323490383       NaN   US
    ##    3: 720113 23.29317 2.470298       NaN 0.3908894 0.008459788       NaN   US
    ##    4: 720120 27.01922 2.504692       NaN 3.3351568 0.042854372       NaN   US
    ##    5: 720137 21.88823 1.979335       NaN 1.7958292 0.482502805       NaN   US
    ##   ---                                                                        
    ## 1591: 726777 19.15492 4.673878  1014.299 4.5291393 2.212040085 0.3920955   US
    ## 1592: 726797 18.78980 2.858586  1014.902 4.8942607 0.396747923 0.2106085   US
    ## 1593: 726798 19.47014 4.445783  1014.072 4.2139153 1.983945197 0.6195467   US
    ## 1594: 726810 25.03549 3.039794  1011.730 1.3514356 0.577955642 2.9607085   US
    ## 1595: 726813 23.47809 2.435372  1012.315 0.2059716 0.026465595 2.3759601   US
    ##       STATE   temp50   wind50    atm50     eudist
    ##    1:    CA 22.66268 2.565445 1012.557 10.5649277
    ##    2:    TX 29.75188 3.413737 1012.460  1.9447578
    ##    3:    MI 20.51970 2.273423 1014.927  2.7804480
    ##    4:    SC 25.80545 1.696119 1015.281  1.4584280
    ##    5:    IL 22.43194 2.237622 1014.760  0.6019431
    ##   ---                                            
    ## 1591:    MT 19.15492 4.151737 1014.185  0.5221409
    ## 1592:    MT 19.15492 4.151737 1014.185  1.3437090
    ## 1593:    MT 19.15492 4.151737 1014.185  0.4310791
    ## 1594:    ID 20.56798 2.568944 1012.855  4.4922623
    ## 1595:    ID 20.56798 2.568944 1012.855  2.9131751

``` r
medianStateStation = stationStates[order(STATE, eudist)]
```

You can now go through medianStateStation data table to find the most
representative per state. For example, in CA, it is station 722970.

# 3. In the middle?
