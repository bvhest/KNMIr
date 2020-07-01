library(dplyr)
library(lubridate)
library(stringr)

# for stand-alone testing:
# library(tinytest)
# source("./R/getClimateDayDataApi.R")
# source("./R/utility_functions.R")
# load("./data/stations.RData")

# **********************************************************************************************************************
# check default behavior (all stations, start-date is start of this year, end-date is yesterday)
# note: resulting dataframe can take long to retrieve.
# **********************************************************************************************************************
dd <- get_daily_data()

station_count <- 47L
start_date <-
  lubridate::today() %>%
  lubridate::year(.) %>%
  paste0(., "0101") %>%
  as.numeric()
end_date <-
  (lubridate::today()-1) %>%
  as.character() %>%
  stringr::str_remove_all(., pattern = "-") %>%
  as.numeric()

# check default for parameters
expect_equal(length(unique(dd$STN)), station_count)
expect_equal(min(dd$YYYYMMDD), start_date)
expect_equal(max(dd$YYYYMMDD), end_date)
# check number of returned columns
expect_equal(ncol(dd), 41)


# **********************************************************************************************************************
# check non-default behavior: one station (for speed) and current year
# **********************************************************************************************************************
station_count <- 1L
from <-
  lubridate::today() %>%
  format(., "%Y")
start_date <-
  from %>%
  paste0(., "0101") %>%
  as.numeric()
dd <- get_daily_data(stationID = 260,
                     from = from)
expect_equal(length(unique(dd$STN)), station_count)
expect_equal(min(dd$YYYYMMDD), start_date)
expect_equal(max(dd$YYYYMMDD), end_date)


# **********************************************************************************************************************
# check non-default behavior: one station (for speed) and current year
# **********************************************************************************************************************
station_count <- 1L
from <-
  paste0(year(lubridate::today()), "01")
start_date <-
  from %>%
  paste0(., "01") %>%
  as.numeric()
dd <- get_daily_data(stationID = 260,
                     from = from)
expect_equal(length(unique(dd$STN)), station_count)
expect_equal(min(dd$YYYYMMDD), start_date)
expect_equal(max(dd$YYYYMMDD), end_date)


# **********************************************************************************************************************
# check non-default behavior: one station (for speed) and current month
# **********************************************************************************************************************
station_count <- 1L
to <-
  lubridate::today() %>%
  format(., "%Y%m")
to_date <-
  to %>%
  paste0(., "01") %>%
  lubridate::ymd() %>%
  lubridate::ceiling_date(unit = "month")-1
to_date <-
  to_date %>%
  as.character() %>%
  stringr::str_remove_all(pattern = "-") %>%
  as.numeric()
dd <- get_daily_data(stationID = 260,
                     from = from,
                     to = to)
expect_equal(length(unique(dd$STN)), station_count)
expect_equal(min(dd$YYYYMMDD), start_date)
expect_equal(max(dd$YYYYMMDD), end_date)


# **********************************************************************************************************************
# check deprecated functions
# **********************************************************************************************************************
dd_dep <- get_climate_day_data_api(stationID = 260,
                                   from = from,
                                   to = to)
expect_equal(dd, dd_dep)

dd_dep <- get_climate_data_api(stationID = 260,
                               from = from,
                               to = to)
expect_equal(dd, dd_dep)
