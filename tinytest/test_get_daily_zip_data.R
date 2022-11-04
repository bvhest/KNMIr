library(dplyr)
library(lubridate)
library(stringr)

# **********************************************************************************************************************
# check default behavior (all land-based stations, this year)
# **********************************************************************************************************************
dd <- get_daily_data_from_prepared_zip()

station_count <- 47L
start_date <-
  lubridate::today() %>%
  lubridate::year() %>%
  paste0(., "0101") %>%
  as.integer()
end_date <-
  (lubridate::today()-1) %>% # sea-based data lags the land-based data: adjust end date.
  as.character() %>%
  stringr::str_remove_all(pattern = "-") %>%
  as.integer()

# check default for parameters
expect_equal(length(unique(dd$STN)), station_count)
expect_equal(min(dd$YYYYMMDD), start_date)
expect_equal(max(dd$YYYYMMDD), end_date)
# check number of returned columns
expect_equal(ncol(dd), 41)


# **********************************************************************************************************************
# check non-default behavior: all sea-based stations, this year.
# **********************************************************************************************************************
station_count <- 13L

dd <- get_daily_data_from_prepared_zip(station_type = "sea")

expect_equal(length(unique(dd$STN)), station_count)
expect_equal(min(dd$YYYYMMDD), start_date)
# check number of returned columns
expect_equal(ncol(dd), 31)


# **********************************************************************************************************************
# check non-default behavior: all land- and sea-based stations, this year.
# **********************************************************************************************************************
station_count <- 60L

dd <- get_daily_data_from_prepared_zip(station_type = "both")

expect_equal(length(unique(dd$STN)), station_count)
expect_equal(min(dd$YYYYMMDD), start_date)
# check number of returned columns
expect_equal(ncol(dd), 31)


# **********************************************************************************************************************
# check non-default behavior: one land-based station and current year
# **********************************************************************************************************************
station_count <- 1L
from <-
  lubridate::today() %>%
  format(., "%Y")
start_date <-
  from %>%
  paste0(., "0101") %>%
  as.numeric()
to <-
  lubridate::today() %>%
  format(., "%Y%m")

dd <- get_daily_data_from_prepared_zip(stationID = 260,
                                       from = from,
                                       to = to)

expect_equal(length(unique(dd$STN)), station_count)
expect_equal(min(dd$YYYYMMDD), start_date)

# **********************************************************************************************************************
# check non-default behavior: one sea-based station and current year
# **********************************************************************************************************************

dd <- get_daily_data_from_prepared_zip(stationID = 201,
                                       from = from,
                                       to = to,
                                       station_type = "sea")

expect_equal(length(unique(dd$STN)), station_count)
expect_equal(min(dd$YYYYMMDD), start_date)


# **********************************************************************************************************************
# check parameters
# **********************************************************************************************************************
expect_error(get_daily_data_from_prepared_zip(from = 2020, to = 2019),
             pattern = "The values for 'from' and 'to' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM' or 'YYYYMMDD'.")
expect_error(get_daily_data_from_prepared_zip(from = "2020", to = "2019"),
             pattern = "The values for 'from' and 'to' could not be parsed into dates where 'from' <= 'to'.")
expect_error(get_daily_data_from_prepared_zip(station_type = "all"),
             pattern = "The station_type must be one of 'land', 'sea' or 'both'.")




# **********************************************************************************************************************
# check deprecated functions
# **********************************************************************************************************************
dd <- get_daily_data_from_prepared_zip()
dd_dep <- get_climate_data_zip()

expect_equal(dd, dd_dep)


dd <- get_daily_data_from_prepared_zip(station_type = "both")
dd_dep <- get_climate_data_zip(return_only_land = FALSE)

expect_equal(dd, dd_dep)
