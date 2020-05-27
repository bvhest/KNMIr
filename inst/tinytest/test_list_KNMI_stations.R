# check number of available KNMI stations
expect_equal(nrow(list_stations()), 45)
expect_equal(nrow(list_stations(is_active = FALSE)), 48)

# check number of returned columns
expect_equal(ncol(list_stations()), 8)
expect_equal(ncol(list_stations(identifying_columns = TRUE)), 3)
