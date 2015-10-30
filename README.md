[![Build_Status](https://travis-ci.org/Hackout2/mapData.svg)](https://travis-ci.org/Hackout2/mapData)


# mapData

## Description
This package provides helper functions to run summary stats in line list datasets, including geocoding missing lat/longs, invalid lat/longs. The package will also provide functionality to calculate incidence/prevalence, aggregate summaries, clusters and smoothing functions.

## Installation
Install this version of the package from GitHub. To run this tutorial you will also need to install the `epimap` package. 

	devtools::install_github("Hackout2/mapData")
	devtools::install_github("Hackout2/epimap")

## Demo
The demonstration uses data on cholera deaths collected in Soho by John Snow during the 1854 outbreak. First, load the package and the line list data and shapefiles. (Note that although the locations of deaths are Snow's, the sex and age of cases and the area populations have been simulated.)

	library(mapData)
	library(epimap)
	data(snow_linelist)
	data(snow_polygons)

### Cases at unique locations
Begin by calculating the number of cases at each unique location. The `density_data` function returns a data frame giving the number of cases at each location.

	locations <- density_data(cases)
	head(locations)

	nrow(cases@data)  # 489 cases were reported in total
	nrow(locations)   # ... involving 250 unique locations
	
### Allocating point cases to areas
It may be useful to allocate cases to geographical areas in which they occurred: countries, administrative areas, or in our case areas defined by the nearest public water supply.

	cases <- area_id(cases, pump_areas, "name")
	head(cases)

### Summaries area
Now we know to which "pump area" each case belongs, we can calculate summary statistics on the cases by area. For example, let's look at the mean age in each area:

	agg_summaries(cases@data, var = "age", group = "name", FUN = mean)
	
In this example, as the ages were generated randomly from a uniform distribution, the means do not vary much!

### Prevalence by area
Calculate the prevalence of infection within each area using the `calculate_prevalence` function.

	cases@data <- merge(cases@data, pump_areas@data) # add the population sizes to the line list data
	prev <- calculate_prevalence(cases@data, 
		pops=pump_areas@data[,c(1,3)], 
		region.head="pump.id")
	print(prev)

The calculated prevalence can be included as information about each pump area, and plotted on a map using the `epimap` package.

	pump_areas@data <- merge(
		pump_areas@data, 
		data.frame(pump.id = prev$region, prev = prev$prevalence)
		)
		
	choroMap(pump_areas, col.by="prev", directView="browser", alpha=0.5)

### Unusually high or low prevalence
From the map, the area surrounding the Broad Street pump seems to have a higher prevalence than areas near other pumps. But was it "unusually" high, or just the result of random local variation?

	unusual_prevalence_region(prev, prev[,1:2], region.head="region", region.i="Broad Street")

### estimate_density

## Uninstall
If you would like to remove the package for any reason, use:

	remove.packages("mapData")