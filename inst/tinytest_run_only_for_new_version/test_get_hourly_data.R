library(dplyr)
library(lubridate)
library(stringr)


# check default behavior (all stations, start-date is start of this year, end-date is yesterday)
# note: resulting dataframe can take long to retrieve.
dd <- get_hourly_data()

station_count <- 1L
start_date <-
  (lubridate::today()-7) %>%
  paste0(., "01") %>%
  stringr::str_remove_all(pattern = "-") %>%
  as.numeric()
end_date <-
  (lubridate::today()-1) %>%
  as.character() %>%
  stringr::str_remove_all(pattern = "-") %>%
  paste0(., "24") %>%
  as.numeric()

# check default for parameters
expect_equal(length(unique(dd$STN)), station_count)
from <- paste0(min(dd$YYYYMMDD), stringr::str_pad(min(dd$HH), width = 2, side = "left", pad = "0")) %>% as.numeric()
expect_equal(from, start_date)
to <- paste0(max(dd$YYYYMMDD), stringr::str_pad(max(dd$HH), width = 2, side = "left", pad = "0")) %>% as.numeric()
expect_equal(to, end_date)
# check number of returned columns
expect_equal(ncol(dd), 25)


# check non-default behavior: one station (for speed) and current month
from <-
  lubridate::today() %>%
  format(., "%Y%m")
start_date <-
  from %>%
  paste0(., "0101") %>%
  as.numeric()
dd <- get_hourly_data(stationID = 350,
                     from = from)

expect_equal(length(unique(dd$STN)), station_count)
from.chk <- paste0(min(dd$YYYYMMDD), stringr::str_pad(min(dd$HH), width = 2, side = "left", pad = "0")) %>% as.numeric()
expect_equal(from.chk, start_date)
to.chk <- paste0(max(dd$YYYYMMDD), stringr::str_pad(max(dd$HH), width = 2, side = "left", pad = "0")) %>% as.numeric()
expect_equal(to.chk, end_date)

# check non-default behavior: one station (for speed) and current month
from <-
  (lubridate::today() - 1) %>%
    format(., "%Y%m%d")
start_date <-
  from %>%
  paste0(., "01") %>%
  as.numeric()
dd <- get_hourly_data(stationID = 350,
                      from = from)

expect_equal(length(unique(dd$STN)), station_count)
from.chk <- paste0(min(dd$YYYYMMDD), stringr::str_pad(min(dd$HH), width = 2, side = "left", pad = "0")) %>% as.numeric()
expect_equal(from.chk, start_date)
to.chk <- paste0(max(dd$YYYYMMDD), stringr::str_pad(max(dd$HH), width = 2, side = "left", pad = "0")) %>% as.numeric()
expect_equal(to.chk, end_date)


# check non-default behavior: one station (for speed) and current month
to <-
  (lubridate::today() - 1) %>%
  format(., "%Y%m%d")
end_date <-
  to %>%
  paste0(., "24") %>%
  as.numeric()
dd <- get_hourly_data(stationID = 350,
                     from = from,
                     to = to)
expect_equal(length(unique(dd$STN)), station_count)
from.chk <- paste0(min(dd$YYYYMMDD), stringr::str_pad(min(dd$HH), width = 2, side = "left", pad = "0")) %>% as.numeric()
expect_equal(from.chk, start_date)
to.chk <- paste0(max(dd$YYYYMMDD), stringr::str_pad(max(dd$HH), width = 2, side = "left", pad = "0")) %>% as.numeric()
expect_equal(to.chk, end_date)


# check deprecated functions
dd_dep <- get_climate_hour_data_api(stationID = 350,
                                    from = from,
                                    to = to)
expect_equal(dd, dd_dep)
