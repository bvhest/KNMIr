KNMIr-package
================
Bart van Hest
2020-06-25 (2025-05-27)

## R functions for retrieving climate data from the Dutch Meteorological Institute (KNMI) API.

### Description

This package provides functions to retrieve climate data (up to
‘yesterday’) for the offical KNMI weather stations from the official
Dutch meteorological institute (KNMI). In addition, a six day forecast
can be retrieved from the KNMI-website and a 14-day forecast from the
Weerplaza-website. The data-set contains multiple variables. For some
stations, data goes back to 1900.

The data can be subsetted based on year and/or specified variables. The
returned data is converted to SI-units, when necessary. Several
additional functions are available for plotting the locations of the
weather stations and a meteogram.

The package also contains two KNMI data-sets, one with metadata for the
measurement stations, both on land and at sea, and the other with a
reference set of long-term averages over the period 1999-2014. In
addition there is a data-set that can be used to convert wind-scales.
Finally there is a map of The Netherlands.

### Examples

See the [package
vignette](https://github.com/BvHest/KNMIr/blob/master/vignettes/HowToUseKNMIr.Rmd)
for some examples. The vignette can be viewed from R/Rstudio with

```r
vignette("HowToUseKNMIr")
```

More information can be found in the package documentation.
