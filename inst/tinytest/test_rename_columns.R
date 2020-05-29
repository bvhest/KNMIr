library(dplyr)
library(lubridate)
library(stringr)

# **********************************************************************************************************************
# prepare data for renaming columns
#
# NB setup is independent from the KNMIr-subsetting function.
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

knmi_column_names <-
  colnames(dd)

describing_column_names <-
  c("stationID", "stationNaam", "YYYYMMDD", "datum", "VectorgemiddeldeWindrichting", "Vectorgemiddeldewindsnelheid",
    "gemWind","maxWind", "uurMaxWind", "minWind", "uurMinWind", "maxWindstoot", "uurMaxWindstoot",
    "gemTemp", "minTemp", "uurMinTemp", "maxTemp", "uurMaxTemp", "minTemp10cm", "dagdeelMinTemp10cm",
    "zon", "percZon", "straling", "duurNeerslag",
    "dagTotaalNeerslag", "maxUurNeerslag","uurUurNeerslag","refGewasverdamping",
    "gemLuchtdruk", "maxUurLuchtdruk", "uurMaxUurLuchtdruk", "minUurLuchtdruk", "uurMinUurLuchtdruk",
    "minZicht", "uurMinZicht","maxZicht", "uurMaxZicht",
    "gemBewolking",
    "gemRelVocht","maxRelVocht","uurMaxRelVocht","minRelVocht","uurMinRelVocht",
    "standaardDev_minTemp","standaardDev_gemTemp","standaardDev_maxTemp","standaardDev_neerslag",
    "neerslagKans","windkracht","dogVanJaar", "jaar", "maand","week","dag")

# **********************************************************************************************************************
# test renaming of all 41 columns from land-based stations
# **********************************************************************************************************************
dd_renamed <-
  rename_columns_KNMI_data(dd)

# expect_equal(colnames(dd_renamed), describing_column_names)
# expect_identical(colnames(dd_renamed), describing_column_names)
# expect_equivalent(colnames(dd_renamed), describing_column_names)

expect_true(all(colnames(dd_renamed) %in% describing_column_names))

# **********************************************************************************************************************
# test renaming of subset of columns (the 31 columns from sea-based stations)
# **********************************************************************************************************************
dd <- get_daily_data_from_prepared_zip(stationID = 201,
                                       from = from,
                                       to = to,
                                       station_type = "sea")

dd_renamed <-
  rename_columns_KNMI_data(dd)

expect_true(all(colnames(dd_renamed) %in% describing_column_names))


# **********************************************************************************************************************
# test renaming of subset of columns (the 6 day weather forecast)
# **********************************************************************************************************************
dd <- get_6day_weather_forecast()

dd_renamed <-
  rename_columns_KNMI_data(dd)

expect_true(all(colnames(dd_renamed) %in% describing_column_names))


# **********************************************************************************************************************
# test renaming of subset of columns (the 14 day weather forecast)
# **********************************************************************************************************************
dd <- get_14day_weather_forecast()

dd_renamed <-
  rename_columns_KNMI_data(dd)

expect_true(all(colnames(dd_renamed) %in% describing_column_names))
