# location to test:
location <-
  data.frame(lat = 51.6107, lon = 5.1447)

nearest_station <-
  find_nearest_KNMI_station(location$lat, location$lon)

# positive scenario
# expect_equal(nearest_station$plaats,
#              "Hupsel")
expect_equal(nearest_station$plaats,
             "Hupsel")

# input checks
expect_error(find_nearest_KNMI_station(location$lat))
expect_error(find_nearest_KNMI_station(location$lon))
expect_error(find_nearest_KNMI_station(location$lat, -181))
expect_error(find_nearest_KNMI_station(91, location$lon))
expect_error(find_nearest_KNMI_station(location$lat, "a"))
expect_error(find_nearest_KNMI_station("a", location$lon))
expect_error(find_nearest_KNMI_station(location$lat, rep(45 , 2)))
expect_error(find_nearest_KNMI_station(rep(45 , 2), location$lon))
