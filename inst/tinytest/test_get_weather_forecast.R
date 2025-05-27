library(tidyverse)

# 6 daagse (KNMI, de Bilt) voorspelling
expect_equal(nrow(get_6day_weather_forecast()), 6, info = "test 6-day weather forcast.")

# 14 daagse (Weerplaza, de Bilt) voorspelling
expect_equal(nrow(get_14day_weather_forecast()), 14, info = "test 14-day weather forcast I.")

# 14 daagse voorspelling: Gilze-Rijen
fc <- get_14day_weather_forecast()
expect_equal(fc$STN[1], 260, info = "test 14-day weather forcast II (default).")

fc <- get_14day_weather_forecast("De Bilt")
expect_equal(fc$STN[1], 260, info = "test 14-day weather forcast III (station by name).")

fc <- get_14day_weather_forecast(350)
expect_equal(fc$STN[1], 350, info = "test 14-day weather forcast III (station by number).")

fc <- get_14day_weather_forecast("Gilze-Rijen")
expect_equal(fc$STN[1], 350, info = "test 14-day weather forcast IV (station by name II).")

# invalid parameter
expect_error(get_14day_weather_forecast(999), info = "test 14-day weather forcast V (invalid station).")

