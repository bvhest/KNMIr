# **********************************************************************************************************************
#
# hulp-functies
#
# **********************************************************************************************************************

# function to rename column-names when not all columns are present.
#
# source: https://stackoverflow.com/questions/20987295/rename-multiple-columns-by-names
#
set_column_names <-
  function(x,
           col.from,
           col.to,
           allow.absent.cols = TRUE) {
    existing <- match(col.from, names(x))
    names(x)[na.omit(existing)] <- col.to[which(!is.na(existing))]

    return(x)
  }

# **********************************************************************************************************************
#
# hulp-functies
#
# **********************************************************************************************************************

tidy_data <-
  function(data) {
    data <-
      data %>%
      # 3) separate date-string into year, month, day
      dplyr::mutate(
        date = lubridate::ymd(YYYYMMDD),
        doy = lubridate::yday(date),
        year = lubridate::year(date),
        month = lubridate::month(date),
        week = lubridate::week(date),
        day = lubridate::wday(date)
      ) %>%
      # change order of columns
      dplyr::select(STN, YYYYMMDD, date, doy, year, month, week, day, everything()) %>%
      # 4a) convert temp to degrees Celcius

      # ToDo: use something like mutate all

      dplyr::mutate(
        TG = TG / 10.0,
        TN = TN / 10.0,
        TX = TX / 10.0,
        T10N = T10N / 10.0,
        RH = RH / 10.0,
        RHX = RHX / 10.0,
        # gemTemp = gemTemp/10.0,
        # minTemp = minTemp/10.0,
        # maxTemp = maxTemp/10.0,
        # minTemp10cm = minTemp10cm/10.0,
        # 4b) convert neerslag to ml/m2 en verwijder de negatieve waarden:
        # dagTotaalNeerslag = pmax(dagTotaalNeerslag/10,0),
        # maxUurNeerslag = pmax(maxUurNeerslag/10,0),
        # 4c) convert zonneschijn naar uren
        # zon = zon/10.0,
        SQ = SQ / 10.0,
        # 4d) convert luchtdruk naar hPa
        # gemLuchtdruk <- gemLuchtdruk/10.0,
        # maxUurLuchtdruk <- maxUurLuchtdruk/10.0,
        # minUurLuchtdruk <- minUurLuchtdruk/10.0,
        PG <- PG / 10.0,
        PX <- PX / 10.0,
        PN <- PN / 10.0,
        # 4e) convert Referentiegewasverdamping naar mm
        # refGewasverdamping <- refGewasverdamping/10.0,
        EV24 <- EV24 / 10.0,
        # 4f) convert windkracht naar m/s
        # gemWind <- gemWind/10.0,
        # maxWind <- maxWind/10.0,
        # minWind <- minWind/10.0,
        # maxWindstoot <- maxWindstoot/10.0,
        # Vectorgemiddeldewindsnelheid <- Vectorgemiddeldewindsnelheid/10.0,
        FG <- FG / 10.0,
        FHX <- FHX / 10.0,
        FHN <- FHN / 10.0,
        FXX <- FXX / 10.0,
        FHVEC <- FHVEC / 10.0
      )

    return(data)
  }

# **********************************************************************************************************************
#
# hulp-functies
#
# **********************************************************************************************************************

#   distance in kilometers between two long/lat positions (from "fossil" package)
earth.distance <-
  function(lat1, lon1, lat2, lon2) {
    rad <- pi / 180
    a1 <- lat1 * rad
    a2 <- lon1 * rad
    b1 <- lat2 * rad
    b2 <- lon2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat / 2))^2 + cos(a1) * cos(b1) * (sin(dlon / 2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 6378.145
    d <- R * c

    return(d)
  }
