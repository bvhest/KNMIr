# Work-in-progress

## ToDo

  - 2021-02-26: package is broken. The current script-based UI seems no longer supported. The following url fails:
  http://projects.knmi.nl/klimatologie/daggegevens/getdata_dag.cgi?start=20200101&end=20210325&stns=350&ALL

modify package to deal with new API. See https://developer.dataplatform.knmi.nl/get-started
     * realtime data: https://api.dataplatform.knmi.nl/open-data/datasets/weer_en_luchtdruk/versions/1.0/files
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
  - (re)enable automatic use of tinytest test-framework for build tests

## KMI

[Automatic weather station (AWS) daily observations](https://opendata.meteo.be/geonetwork/srv/dut/catalog.search;jsessionid=9509BF8341659133C1A4D7DB8E419D3A#/metadata/RMI_DATASET_AWS_1DAY)

Link naar data:
- [JSON-formaat](https://opendata.meteo.be/service/aws/wfs?request=GetFeature&service=WFS&version=1.1.0&typeName=aws:aws_1day&outputFormat=json)
- [Climate statistics](https://opendata.meteo.be/geonetwork/srv/dut/catalog.search;jsessionid=9509BF8341659133C1A4D7DB8E419D3A#/metadata/RMI_DATASET_CLIMATE_STATISTICS)


## Done

  - 2018-xx-xx: add the data from the sea-based stations
  - 2018-xx-xx: retrieve the six day forecast from the KNMI-website.
  - 2018-xx-xx: retrieve the 14 day forecast from the Weerplaza-website.
  - 2020-06-18: corrigeer get_xxday_weather_forecast: maak weerstation configurabel
  - 2020-06-18: remove as many package dependencies as possible,
    change to the tidyverse-packages. Note: there is advice not to do this in packages (or at least not to use the pipe operator. This may need some changes to be reverted).
  - DONE: fixed Error when calling the zip-api.
  	 climateData.r <-
  	   KNMIr::get_climate_data_zip(stationID = "260", # De Bilt
  	                               from = "19000101")
  	Error in KNMIr::get_climate_data_zip(stationID = "260", from = "19000101") :
  	  object 'station' not found
  - 2022-11-21: fixed error with download data for all stations in one call.
      a. fails for current year, download using prepared KNMI zip-file. Cause is change in KNMI file format. fixed
      b. fails for specific year < current year because KNMI API restricts the download size. Cause is a download size restriction. Not fixed, but 'ALL' is no longer permitted when startdate < now - 5 years.

## Known bugs

  - none
