# **********************************************************************************************************************
# positive scenario 1: check number of available KNMI stations
# **********************************************************************************************************************
# active and with temperature sensor
expect_equal(nrow(list_stations()), 47, info = "test number of active stations with temperature sensor.")
# inactive and with temperature sensor
expect_equal(nrow(list_stations(active = FALSE)), 3, info = "test number of inactive stations with temperature sensor.")
# active and without temperature sensor
expect_equal(
  nrow(list_stations(temperature_sensor = FALSE)),
  13,
  info = "test number of active stations without temperature sensor."
)

# **********************************************************************************************************************
# positive scenario 2: check number of returned columns
# **********************************************************************************************************************
expect_equal(ncol(list_stations()), 8, info = "test number of fields for the active stations with temperature sensor.")
expect_equal(
  ncol(list_stations(identifying_columns = TRUE)),
  3,
  info = "test number of identifying fields for the stations."
)

