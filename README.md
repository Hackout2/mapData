[![Build_Status](https://travis-ci.org/Hackout2/mapData.svg)](https://travis-ci.org/Hackout2/mapData)


# mapData

## Description
This package provides helper functions to run summary stats in line list datasets, including geocoding missing lat/longs, invalid lat/longs. The package will also provide functionality to calculate incidence/prevalence, aggregate summaries, clusters and smoothing functions.

## Installation
To install this version of the package from GitHub:

	devtools::install_github("Hackout2/mapData")

## Demo
The demonstration uses data on cholera deaths collected in Soho by John Snow during the 1854 outbreak. First, load the package and the line list data and shapefiles. (Note that although the locations of deaths are Snow's, the sex and age of cases has been simulated.)

	library(mapData)
	data(snow_linelist)
	data(snow_polygons)

### density_data
Begin by calculating the number of cases at each unique location. The `density_data` function returns a data frame giving the number of cases at each location.

	locations <- density_data(cases)
	head(locations)

	nrow(cases@data)  # 489 cases were reported in total
	nrow(locations)   # ... involving 250 unique locations
	
### area_id
It may be useful to allocate cases to geographical areas in which they occurred: countries, administrative areas, or in our case areas defined by the nearest public water supply.

	cases <- area_id(cases, pump_areas, "name")
	head(cases)

### agg_summaries
Now we know to which "pump area" each case belongs, we can calculate summary statistics on the cases by area. For example, let's look at the mean age in each area:

	agg_summaries(cases@data, var = "age", group = "name", FUN = mean)
	
In this example, as the ages were generated randomly from a uniform distribution, the means do not vary much!

### calculate_prevalence
Calculate the prevalence of infection within each area using the `calculate_prevalence` function:

### unusual_prevalence

### estimate_density

## Uninstall
If you would like to remove the package for any reason, use:

	remove.packages("mapData")