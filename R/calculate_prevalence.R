# function to calculate prevalence, by aggregated region

# data is a line list-type data frame (one line per case), including a column with the region in which the case occurred
# pops is a data frame containing one line per region, with one column for region name and another for population
# region is a string containing the name of the column specifying region

# returns a data frame containing total population, number of cases and prevalence by region

calculate_prevalence <- function ( data, pops=NULL, region.head="region" ){

	regioncol <- which(names(data) == region.head)

	data[,regioncol] <- factor( sanitize_text(as.character(data[,regioncol])) )
	
	# make up some populations for now. delete this later
	if(is.null(pops))
		pops <- data.frame(
					region = unique(data[ order(data[,regioncol]) ,regioncol]),
					population = as.numeric(table(data$region)) * 
						(rlnorm( length(unique(data$region))) + 1)
					)
	
	prev <- as.data.frame(tapply(data$departement, data$departement, length))	
	names(prev) <- "cases"
	prev <- merge(pops, prev, by.x="region", by.y="row.names", all=TRUE)				
	
	prev$prevalence <- prev$cases/prev$population
	
	return(prev)
}