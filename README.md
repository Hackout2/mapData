[![Build_Status](https://travis-ci.org/Hackout2/mapData.svg)](https://travis-ci.org/Hackout2/mapData)


# mapData

## Description
Starting from line list-type data, this package provides functions which simply and easily perform some common tasks in spatial epidemiology and disease mapping. For example, it can be used to:
* calculate prevalence by area
* perform aggregate summaries by area
* geocode missing lat/longs and invalid lat/longs. 

The package is designed in conjunction with the `epimap` package and some functions provide input to `epimap`, for example prevalence by area for drawing choropleth maps.

## Installation
Install this version of the package from GitHub. To run the vignette you will also need to install the `epimap` package. 

	devtools::install_github("Hackout2/mapData")
	devtools::install_github("Hackout2/epimap")

## Demo
A demonstration of the package's use is provided as a vignette.

## Uninstall
If you would like to remove the package for any reason, use:

	remove.packages("mapData")