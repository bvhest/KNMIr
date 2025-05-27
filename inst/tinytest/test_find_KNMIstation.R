library(tidyverse)

# **********************************************************************************************************************
# positive scenario 1: nearest to Tilburg (== Gilze-Rijen)
# **********************************************************************************************************************
location <- data.frame(lat = 51.6107, lon = 5.1447)

# version 1
nearest_station <- find_nearest_KNMI_station(location$lat, location$lon)

# positive scenario
expect_equal(nearest_station$plaats, "Gilze-Rijen", info = "test finding nearest KNMI station to a given location I.")

# version 2
nearest_station <- find_nearest_KNMI_station2(location)

# positive scenario
expect_equal(nearest_station$plaats, "Gilze-Rijen", info = "test finding nearest KNMI station to a given location II.")

# **********************************************************************************************************************
# positive scenario 2: nearest to Kloetinge (== Hansweert, but nearest with temperature sensor == Wilhelminadorp)
# **********************************************************************************************************************
location <- data.frame(lat = 51.48219, lon = 3.919015)

nearest_station <- find_nearest_KNMI_station(location$lat, location$lon)

expect_equal(
  nearest_station$plaats,
  "Wilhelminadorp",
  info = "test finding nearest KNMI station to a given location III."
)

nearest_station <- find_nearest_KNMI_station(location$lat, location$lon, temperature_sensor = FALSE)

expect_equal(nearest_station$plaats, "Hansweert", info = "test finding nearest KNMI station to a given location IV.")

# **********************************************************************************************************************
# check parameters
# **********************************************************************************************************************
expect_error(find_nearest_KNMI_station(location$lat))
expect_error(find_nearest_KNMI_station(location$lon))
expect_error(find_nearest_KNMI_station(location$lat, -181))
expect_error(find_nearest_KNMI_station(91, location$lon))
expect_error(find_nearest_KNMI_station(location$lat, "a"))
expect_error(find_nearest_KNMI_station("a", location$lon))
expect_error(find_nearest_KNMI_station(location$lat, rep(45, 2)))
expect_error(find_nearest_KNMI_station(rep(45, 2), location$lon))

