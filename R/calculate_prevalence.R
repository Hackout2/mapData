# function to calculate prevalence, by aggregated region

#' Calculate prevalence within each region of aggregation

#' @param data A line list-type data frame (one line per case), including a column with the region in which the case occurred
#' @param pops A data frame containing one line per region in "data", with first column for region name and second for population
#' @param region A string containing the name of the column specifying the region for each case
#' @param conf.level Level of confidence interval required for prevalence estimates.

#' @return A data frame containing total population, number of cases and prevalence by region

calculate_prevalence <- function ( data, pops=NULL, conf.level=0.95, region.head="region" ){

	regioncol <- which(names(data) == region.head)

	data[,regioncol] <- factor( sanitize_text(as.character(data[,regioncol])) )
	 
	##################################
	# make up some populations for now. delete this later
	if(is.null(pops))
		pops <- data.frame(
					region = unique(data[ order(data[,regioncol]) ,regioncol]),
					population = ceiling(
							as.numeric(table(data$region)) * 
						(rlnorm( length(unique(data$region))) + 1)
						)
					)
	##################################
					
	names(pops) <-c("region", "population")
	
	prev <- as.data.frame(tapply(data$departement, data$departement, length))	
	names(prev) <- "cases"
	prev <- merge(pops, prev, by.x="region", by.y="row.names", all=TRUE)				
	
	prev <- cbind(prev,binconf(prev$cases, prev$population))

	names(prev) <- c("region", "population", "cases", "prevalence", "lower", "upper")	

	return(prev)
}