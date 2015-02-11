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
	
	prev$prevalence <- prev$cases/prev$population
	
	prev$lower <- qnorm(
							(1 - conf.level)/2, 
							prev$prevalence, 
							sqrt( prev$prevalence * (1 - prev$prevalence) / prev$population)
						)

	prev$upper <- qnorm(
							(1 + conf.level)/2, 
							prev$prevalence, 
							sqrt( prev$prevalence * (1 - prev$prevalence) / prev$population)
						)

	# return a warning if low numbers have resulted in a negative confidence interval 
	if(any( prev$lower < 0 ))
		warning("Some regions had low populations. Approximation has resulted in negative CIs for prevalences.")
	
	# prev$lower <- prev$upper <- NA
	
	# for(i in 1:nrow(prev)) {
		# prev$lower[i] <- ( binom.test( prev$cases[i], prev$population[i], conf.level = conf.level) )$conf.int[1]
		# prev$upper[i] <- ( binom.test( prev$cases[i], prev$population[i], conf.level = conf.level) )$conf.int[2]
		# }

	return(prev)
}