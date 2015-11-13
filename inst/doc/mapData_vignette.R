## ---- message=FALSE------------------------------------------------------
library(mapData)
library(epimap)
data(snow_linelist)
data(snow_polygons)

## ------------------------------------------------------------------------
head(cases@data)

## ------------------------------------------------------------------------
locations <- density_data(cases)
head(locations)
nrow(cases@data)  

## ------------------------------------------------------------------------
nrow(locations)   

## ------------------------------------------------------------------------
cases <- area_id(cases, pump_areas, "name")
head(cases@data)

## ------------------------------------------------------------------------
agg_summaries(cases@data, var = "age", group = "name", FUN = mean)

## ------------------------------------------------------------------------
cases@data <- merge(cases@data, pump_areas@data) # add the population sizes to the line list data
prev <- calculate_prevalence(cases@data, 
	pops=pump_areas@data[,c(1,3)], 
	region.head="pump.id",
	conf.level=0.95)
print(prev)

## ---- fig.show='hold'----------------------------------------------------
pump_areas@data <- merge(
	pump_areas@data, 
	data.frame(pump.id = prev$region, prev = prev$prevalence)
	)
		
choroMap(pump_areas, col.by="prev", directView="disabled", alpha=0.5)

# In fact, the figure below was made by running 
# choroMap(pump_areas, col.by="prev", directView="browser", alpha=0.5),
# which brings up the figure in a browser window.

## ------------------------------------------------------------------------
	unusual_prevalence_region(cases@data, pump_areas@data[2:3], region.head="name", region.i="Broad Street")

## ------------------------------------------------------------------------
	calculate_prevalence_unusual_pval(cases@data, pump_areas@data[2:3], region.head="name")

