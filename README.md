KNMIr-package
================
Bart van Hest
2017-08-04

<!-- README.md is generated from README.Rmd. Please edit that file -->
KNMIr-package - R functions for retrieving climate data from the Dutch meteorological institute KNMI.
-----------------------------------------------------------------------------------------------------

### Description

This package provides functions to retrieve climate data (up to 'yesterday') for the offical KNMI weather stations from the official Dutch meteorological institute (KNMI). In addition, a six day forecast can be retrieved from the KNMI-website and a 14-day forecast from the Weerplaza-website. The data-set contains multiple variables. For some stations, data goes back to 1900.

The data can be subsetted based on year and/or specified variables. The returned data is converted to SI-units, when necessary. Several additional functions are available, such as a function that calculate the Huglin- or VE-index.

The package also contains two KNMI datasets, one with the measurement stations, both on and and at sea, and the other with a reference set of long term averages over the period 1999-2014.

### Examples

See the [package vignette](https://github.com/BvHest/KNMIr/blob/master/vignettes/HowToUseKNMIr.Rmd) for some examples. The vignette can be viewed from R/Rstudio with

``` r
vignette("HowToUseKNMIr")
```

More information can be found in the package documentation.
