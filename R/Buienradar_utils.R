# Buienradar utilities

parse_buienradar_weer_tabel <- function(weather_row) {
  # 8 rijen:
  # rij 2: neerslag (mm)
  # rij 3: neerslagkans (%)
  # rij 4: min temp
  # rij 5: max temp
  # rij 6: windkracht (bft)
  # rij 8: zonneschijn (%)

  # extraheer waarden in lists:
  # de datum ontbreekt, maar de 1e dag is altijd 'vandaag'
  datum <- lubridate::today() + seq(0, 5, by = 1)
  maxTemp <- rvest::html_nodes(weather_row[5], xpath = 'td/text()')
  minTemp <- rvest::html_nodes(weather_row[4], xpath = 'td/text()')
  neerslag <- rvest::html_nodes(weather_row[2], xpath = 'td/text()')
  neerslagKans <- rvest::html_nodes(weather_row[3], xpath = 'td/text()')
  percZon <- rvest::html_nodes(weather_row[8], xpath = 'td/text()')
  wind <- rvest::html_nodes(weather_row[6], xpath = 'td/text()')

  size <- length(datum)
  station_ID <- 260 # De Bilt
  station_Name <- stations$plaats[stations$stationID == station_ID]

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
    voorspelling$date[i] <- datum[i]
    voorspelling$YYYYMMDD[i] <- format(voorspelling$date[i], format = "%Y%m%d")
    # maxTemp
    voorspelling$maxTemp[i] <- mean(as.numeric(extraheer_numerieke_waarden(maxTemp[i + 1])))
    voorspelling$maxTempSD[i] <- 0
    # minTemp
    voorspelling$minTemp[i] <- mean(as.numeric(extraheer_numerieke_waarden(minTemp[i + 1])))
    voorspelling$minTempSD[i] <- 0
    # gemTemp
    voorspelling$gemTemp[i] <- mean(c(voorspelling$minTemp[i], voorspelling$maxTemp[i]))
    voorspelling$gemTempSD[i] <- sqrt(
      (voorspelling$minTempSD[i] * voorspelling$minTempSD[i] + voorspelling$maxTempSD[i] * voorspelling$maxTempSD[i]) /
        2
    )
    # neerslag
    neerslag_v <- rvest::html_text(neerslag[i + 1])
    voorspelling$neerslag[i] <- mean(
      as.numeric(stringr::str_sub(neerslag_v, start = 1, end = stringr::str_locate(neerslag_v, pattern = "-")[1] - 1))
    )
    voorspelling$neerslagSD[i] <- 0
    # neerslagKans
    voorspelling$neerslagKans[i] <- mean(as.numeric(extraheer_numerieke_waarden(neerslagKans[i + 1])))
    # percZon
    voorspelling$percZon[i] <- mean(as.numeric(extraheer_numerieke_waarden(percZon[i + 1])))
    # wind
    voorspelling$windkracht[i] <- mean(as.numeric(extraheer_numerieke_waarden(wind[i + 1])))
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

