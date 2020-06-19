# Work-in-progress

## ToDo

  - check if station code is valid, else issue error message.
  - change station-parameter to stations-parameter (so that a list of stations can be downloaded in one call)
  - provide English names for the variable-codes, next to the Dutch translations.
    - change source of KNMI stations to http://projects.knmi.nl/datacentrum/catalogus/catalogus/catalogus-gegevens-overzicht.html
      zie http://projects.knmi.nl/datacentrum/catalogus/catalogus/content/nl-obs-surf-stationslijst.htm
  - test-scripts: tidyverse library toegevoegd (kan mogelijk compacter obv alleen dplyr).
  - replace data.table function by tijdyverse equivalent
  - add the station name to all daily/hourly data.
  - make function subset_KNMI_data suitable for subsetting df's with renamed columns
  - add support for KMI (BE) and DWD (DE)

## Done

  - 2018-xx-xx: add the data from the sea-based stations
  - 2018-xx-xx: retrieve the six day forecast from the KNMI-website.
  - 2018-xx-xx: retrieve the 14 day forecast from the Weerplaza-website.
  - 2020-06-18: corrigeer get_xxday_weather_forecast: maak weerstation configurabel
  - 2020-06-18: remove as many package dependencies as possible,
    change to the tidyverse-packages. Note: there is advice not to do this in packages (or at least not to use the pipe operator. This may need some changes to be reverted).

## Bugs

 1.  Error when calling the zip-api.
 
       climateData.r <-
         KNMIr::get_climate_data_zip(stationID = "260", # De Bilt
                                     from = "19000101")
      Error in KNMIr::get_climate_data_zip(stationID = "260", from = "19000101") : 
        object 'station' not found

 2.  get_daily_data_from_prepared_zip(from = 2020, to = 2019)
   Krijg:
    Error in get_daily_data_from_prepared_zip(from = 2020, to = 2019) : 
    The values for 'from' and 'to' must be a string with a value that describes the date in the format 'YYYY', 'YYYYMM' or 'YYYYMMDD'.
   Verwacht:

 2. SOLVED (2020-5-28)
    Issue with column-names (and number) for hourly API.
 
