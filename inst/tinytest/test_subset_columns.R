library(dplyr)
library(lubridate)
library(stringr)

# for stand-alone testing:
# library(tinytest)
# source("./R/getClimateDayDataZip.R")
# source("./R/utility_functions.R")
# source("./R/subsetKNMIdata.R")

# **********************************************************************************************************************
# prepare data for subsetting columns
#
# NB setup is independent from the KNMIr-renaming function.
# **********************************************************************************************************************
station_count <- 1L
column_count <- 41L
from <-
  "2005"
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
expect_equal(ncol(dd), column_count)

# **********************************************************************************************************************
# test default subset of columns
# **********************************************************************************************************************
id_columns <-
  c("STN", "YYYYMMDD", "date", "doy", "year", "month", "week", "day")

default_set_of_columns <-
  c(id_columns, "FG","TG","TN","TX","SQ","SP","Q","RH","NG")

dd_subset <-
  subset_data(dd)

expect_true(all(colnames(dd_subset) %in% default_set_of_columns),
            info = "test default subset of columns")

# **********************************************************************************************************************
# test specific subset of columns
# **********************************************************************************************************************

# subset of defaul set
specific_set_of_columns <-
  c("FG","TG","TN","TX")

dd_subset <-
  subset_data(dd, variables = specific_set_of_columns)

expect_true(all(colnames(dd_subset) %in% c(id_columns, specific_set_of_columns)),
            info = "test specific subset of default columns")

# columns not in defaul set
specific_set_of_columns <-
  c("PG","PX","UX","EV24")

dd_subset <-
  subset_data(dd, variables = specific_set_of_columns)

expect_true(all(colnames(dd_subset) %in% c(id_columns, specific_set_of_columns)),
            info = "test non-default specific subset of columns")

# **********************************************************************************************************************
# test default from-year
# **********************************************************************************************************************

# expect_equal(min(dd_subset$year), max(2006, as.numeric(from)),
#              info = "test default from-year")

# **********************************************************************************************************************
# test specific from-year
# **********************************************************************************************************************

new_from_year <-
  "2010"
dd_subset <-
  subset_data(dd, startyear = new_from_year)

expect_equal(min(dd_subset$year), as.numeric(new_from_year),
             info = "test specific from-year")
