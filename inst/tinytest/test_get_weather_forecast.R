# 6 daagse (KNMI, de Bilt) voorspelling
expect_equal(nrow(get_6day_weather_forecast()), 6)

# 14 daagse (Weerplaza, de Bilt) voorspelling
expect_equal(nrow(get_14day_weather_forecast()), 14)

# 14 daagse voorspelling: Gilze-Rijen
fc <- get_14day_weather_forecast()
expect_equal(fc$STN[1], 260)

fc <- get_14day_weather_forecast("De Bilt")
expect_equal(fc$STN[1], 260)

fc <- get_14day_weather_forecast(350)
expect_equal(fc$STN[1], 350)

fc <- get_14day_weather_forecast("Gilze-Rijen")
expect_equal(fc$STN[1], 350)

# invalid parameter
expect_error(get_14day_weather_forecast(999))
