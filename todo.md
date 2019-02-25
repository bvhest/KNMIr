ToDo:

  - DONE: add the data from the sea-based stations
  - turn station-parameter in stations-parameter (so that a list of stations can be downloaded in one call)
  - DONE: retrieve the six day forecast from the KNMI-website.
  - provide English names for the variable-codes, next to the Dutch translations.
  - change source of KNMI stations to http://projects.knmi.nl/datacentrum/catalogus/catalogus/catalogus-gegevens-overzicht.html


Bugs

 1.  Error when calling the zip-api.
	 climateData.r <-
	   KNMIr::get_climate_data_zip(stationID = "260", # De Bilt
	                               from = "19000101")
	Error in KNMIr::get_climate_data_zip(stationID = "260", from = "19000101") : 
	  object 'station' not found

