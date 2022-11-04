# **********************************************************************************************************************
# positive scenario 1: check number of available KNMI stations
# **********************************************************************************************************************
# active and with temperature sensor
expect_equal(nrow(list_stations()), 47)
# inactive and with temperature sensor
expect_equal(nrow(list_stations(active = FALSE)), 3)
# active and without temperature sensor
expect_equal(nrow(list_stations(temperature_sensor = FALSE)), 13)

# **********************************************************************************************************************
# positive scenario 2: check number of returned columns
# **********************************************************************************************************************
expect_equal(ncol(list_stations()), 8)
expect_equal(ncol(list_stations(identifying_columns = TRUE)), 3)
