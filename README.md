[![Build_Status](https://travis-ci.org/Hackout2/mapData.svg)](https://travis-ci.org/Hackout2/mapData)


# mapData

## Description
This package provides helper functions to run summary stats in line list datasets, including geocoding missing lat/longs, invalid lat/longs. The package will also provide functionality to calculate incidence/prevalence, aggregate summaries, clusters and smoothing functions.

## Installation
To install this version of the package from GitHub:

	devtools::install_github("Hackout2/mapData")

## Demo
The demonstration uses data on cholera deaths collected in Soho by John Snow during the 1854 outbreak. First, load the package and the line list data. (Note that although the locations of deaths are Snow's, the sex and age of cases has been simulated.)

	library(mapData)
	data(snow_linelist)

### density_data
Begin by calculating the number of cases at each unique location.

	density_data(snow_linelist)

### area_id

### agg_summaries

### calculate_prevalence

### unusual_prevalence

### estimate_density

## Uninstall
If you would like to remove the package for any reason, use:

	remove.packages("mapData")