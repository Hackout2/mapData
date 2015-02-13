# function to calculate prevalence, by aggregated region and stratified on other factors

#' Calculate prevalence within each region of aggregation

#' @param data A line list-type data frame (one line per case), including a column with the region in which the case occurred and columns with stratification variables.
#' @param pops A data frame containing one line per region/stratification variable combination in "data". The first column should give the region name, then should come the stratification variables, and finally the population size in that region, included in that stratum. If pops is NULL, the population in each area and stratum is taken as 1, and numbers of cases (rather than prevalences) are calculated.
#' @param region.head A string containing the name of the column specifying the region for each case
#' @param stratify.vars A vector of strings containing the names of the column specifying the stratification variables. These must be factors.
#' @param conf.level Level of confidence interval required for prevalence estimates.
#' @param scale Scaling with which to report prevalence (per head, per 100 000, etc.)

#' @return A list containing 1) "prevalence.list", a list of data frames containing total population, number of cases and prevalence by region, for each unique combination of stratification variables. 2) "stratification.levels", a vector of the stratification levels, in the same order that the stratified prevalence data frames are presented.

calculate_stratified_prevalence <- function( data, stratify.vars, pops=NULL, region.head="region", conf.level=0.95, scale=1){
		
	stratify.cols <- which(names(data) %in% stratify.vars)
	
	# add a column giving unique combination of stratification variables
	data$stratvar <- factor(apply(as.matrix(data[,stratify.cols]), 1, paste, collapse="."))
	if(!is.null(pops))
		pops$stratvar <- factor(apply(
										as.matrix(pops[, which(names(pops) %in% stratify.vars)]), 
										1, 
										paste, 
										collapse="."
										) )
	
	# now apply the prevalence calculation function, at each level of stratification	
	
	prevalence <- vector("list", length(unique(data$stratvar)) )
	
	for(i in 1:length( unique(data$stratvar) ) ){

		sub <- subset(data, data$stratvar == ( unique(data$stratvar) )[i])
		
		if(is.null(pops))
			prevalence[[i]] <- calculate_prevalence( sub, NULL, conf.level, region.head, scale )
		else
			prevalence[[i]] <- calculate_prevalence( sub, pops[pops$stratvar == ( unique(data$stratvar) )[i], c(1, ncol(pops)-1)], conf.level, region.head, scale )}
	
	return(list(
				stratification.levels = unique(data$stratvar), 
				prevalence.list = prevalence
				))
	
}