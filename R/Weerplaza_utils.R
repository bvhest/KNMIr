# Weerplaza utilities

parse_weerplaza_weer_tabel <- function(station_ID, station_Name, weather_row) {
  # 10 rijen:
  # rij 1: datum + samenvatting
  # rij 2: zonkans + UV
  # rij 3: min/max temp
  # rij 4: neerslag (kans en hoeveelheid)
  # rij 5: wind
  # idem voor rij 6-10
  #
  # Nb elke rij bevat in de td-elementen een attribuut met de datum (@data-day)

  # extraheer waarden in lists:
  datum <- rvest::html_nodes(weather_row[1], xpath = "td/@data-day")
  maxTemp <- rvest::html_nodes(weather_row, xpath = 'td/div[@class="red temp"]/text()')
  minTemp <- rvest::html_nodes(weather_row, xpath = 'td/div[@class="blue temp"]/text()')
  neerslag <- rvest::html_nodes(weather_row, xpath = 'td/div[@class="blue"][not(i/@class)]/text()')
  neerslagKans <- rvest::html_nodes(weather_row, xpath = 'td/div[@class="blue"][i/@class="fa fa-tint blue"]/text()')
  percZon <- rvest::html_nodes(weather_row, xpath = 'td/div[@class="orange"]/text()')
  wind <- rvest::html_nodes(weather_row, xpath = 'td/div[@class="hidden"]/text()')

  #datum <-
  #  unique(rvest::html_text(datum))
  size <- length(datum)

  voorspelling <- data.frame(
    stationID = rep(station_ID, size),
    station = rep(station_Name, size),
    YYYYMMDD = rep("20000101", size),
    date = rep(as.Date("2000-01-01"), size),
    maxTemp = double(size),
    maxTempSD = double(size),
    minTemp = double(size),
    minTempSD = double(size),
    gemTemp = double(size),
    gemTempSD = double(size),
    neerslag = double(size),
    neerslagSD = double(size),
    neerslagKans = double(size),
    percZon = double(size),
    windkracht = double(size),
    stringsAsFactors = FALSE
  )

  # nu alles in een loopje:
  for (i in 1:size) {
    voorspelling$date[i] <- lubridate::dmy(rvest::html_text(datum[i]))
    voorspelling$YYYYMMDD[i] <- format(voorspelling$date[i], format = "%Y%m%d")
    # maxTemp
    voorspelling$maxTemp[i] <- mean(as.numeric(extraheer_numerieke_waarden(maxTemp[i])))
    voorspelling$maxTempSD[i] <- 0
    # minTemp
    voorspelling$minTemp[i] <- mean(as.numeric(extraheer_numerieke_waarden(minTemp[i])))
    voorspelling$minTempSD[i] <- 0
    # gemTemp
    voorspelling$gemTemp[i] <- mean(c(voorspelling$minTemp[i], voorspelling$maxTemp[i]))
    voorspelling$gemTempSD[i] <- sqrt(
      (voorspelling$minTempSD[i] * voorspelling$minTempSD[i] + voorspelling$maxTempSD[i] * voorspelling$maxTempSD[i]) /
        2
    )
    # neerslag
    voorspelling$neerslag[i] <- mean(
      as.numeric(stringr::str_replace(rvest::html_text(neerslag[i]), pattern = ",", replacement = "."))
    )
    voorspelling$neerslagSD[i] <- 0
    # neerslagKans
    voorspelling$neerslagKans[i] <- mean(as.numeric(extraheer_numerieke_waarden(neerslagKans[i])))
    # percZon
    voorspelling$percZon[i] <- mean(as.numeric(extraheer_numerieke_waarden(percZon[i])))
    # wind
    voorspelling$windkracht[i] <- mean(as.numeric(extraheer_numerieke_waarden(wind[i])))
  }

  # kolom-namen omzetten van leesbaar, naar  codes (kan weer worden teruggezet met de functie 'rename_columns()')
  colnames(voorspelling) <- c(
    "STN",
    "station",
    "YYYYMMDD",
    "date",
    "TX",
    "SD_maxTemp",
    "TN",
    "SD_minTemp",
    "TG",
    "SD_gemTemp",
    "RH",
    "SD_neerslag",
    "neerslagKans",
    "SP",
    "windkracht"
  )

  return(voorspelling)
}

# decode station id of naam voor Weerplaza URL
get_URL_based_on_location <- function(station) {
  locationURL <- dplyr::case_when(
    station == 210 | station == "Valkenburg" ~ "valkenburg/19412/",
    station == 215 | station == "Voorschoten" ~ "voorschoten/19838/",
    station == 225 | station == "IJmuiden" ~ "ijmuiden/11054/",
    station == 235 | station == "De Kooy" ~ "de-kooy/2753344/",
    station == 240 | station == "Schiphol" ~ "schiphol/5577/",
    station == 242 | station == "Vlieland" ~ "vlieland/2743351/",
    station == 248 | station == "Wijdenes" ~ "wijdenes/20205/",
    station == 249 | station == "Berkhout" ~ "berkhout/6569/",
    station == 251 | station == "Hoorn Terschelling" ~ "hoorn/2750195/",
    station == 257 | station == "Wijk aan Zee" ~ "wijk-aan-zee/20209/",
    station == 260 | station == "De Bilt" ~ "de-bilt/8500/",
    station == 265 | station == "Soesterberg" ~ "amersfoort/5551/",
    station == 267 | station == "Stavoren" ~ "stavoren/18367/",
    station == 269 | station == "Lelystad" ~ "lelystad/12571/",
    station == 270 | station == "Leeuwarden" ~ "leeuwarden/12537/",
    station == 273 | station == "Marknesse" ~ "marknesse/13455/",
    station == 275 | station == "Deelen" ~ "deelen/8552/",
    station == 277 | station == "Lauwersoog" ~ "lauwersoog/12418/",
    station == 278 | station == "Heino" ~ "heino/10580/",
    station == 279 | station == "Hoogeveen" ~ "hoogeveen/10870/",
    station == 280 | station == "Eelde" ~ "eelde-paterswolde/8967/",
    station == 283 | station == "Hupsel" ~ "hupsel/2749993/",
    station == 286 | station == "Nieuw Beerta" ~ "nieuw-beerta/2747446/",
    station == 290 | station == "Twenthe" | station == "Twente" ~ "twente/2743881/",
    station == 308 | station == "Cadzand" ~ "cadzand/7340/",
    station == 310 | station == "Vlissingen" ~ "vlissingen/19806/",
    station == 311 | station == "Hoofdplaat" ~ "hoofdplaat/10860/",
    station == 315 | station == "Hansweert" ~ "hansweert/10434/",
    station == 319 | station == "Westdorpe" ~ "westdorpe/20115/",
    station == 323 | station == "Wilhelminadorp" ~ "wilhelminadorp/20231/",
    station == 324 | station == "Stavenisse" ~ "stavenisse/18364/",
    station == 330 | station == "Hoek van Holland" ~ "hoek-van-holland/10780/",
    station == 331 | station == "Tholen" ~ "tholen/18860/",
    station == 340 | station == "Woensdrecht" ~ "woensdrecht/20308/",
    station == 343 | station == "Rotterdam Geulhaven" ~ "rotterdam/16707/",
    station == 344 | station == "Rotterdam" ~ "rotterdam/16707/",
    station == 348 | station == "Cabauw Mast" ~ "cabauw/13004/",
    station == 350 | station == "Gilze-Rijen" ~ "gilze-rijen/9950/",
    station == 356 | station == "Herwijnen" ~ "herwijnen/10680/",
    station == 370 | station == "Eindhoven" ~ "eindhoven/9020/",
    station == 375 | station == "Volkel" ~ "volkel/19829/",
    station == 377 | station == "Ell" ~ "ell/9091/",
    station == 380 | station == "Maastricht" ~ "maastricht/13193/",
    station == 391 | station == "Arcen" ~ "arcen/5741/",
    TRUE ~ "de-bilt/8500/"
  )

  return(locationURL)
}

