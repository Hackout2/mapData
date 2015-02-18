#' @import Hmisc
#'
#' @param data A line list-type data frame (one line per case), including a column with the region in which the case occurred
#' @param pops A data frame containing one line per region in \code{data}, with first column for region name and second for population
#' @param region.head A string containing the name of the column specifying the region for each case
#' @param conf.level Level of confidence interval required for prevalence estimates.
#' @param scale Scaling with which to report prevalence (per head, per 100 000, etc.)
#' @return A data frame containing total population, number of cases and prevalence by region
#' @export
#' @examples
#' my.data <- data.frame(	county.id = ceiling(3*runif(10)),
#'                      	age = rlnorm(10),
#'							sex = factor(floor(2*runif(10)), levels=c(0,1), labels=c("male", "female"))
#'                      )
#'
#' my.populations <- data.frame(county.id = 1:3,
#'								population = c(10, 50, 100)
#'								)
#'
#' # example without population of each region
#' calculate_prevalence(my.data, region.head="county.id")
#'
#' # example with populations
#' calculate_prevalence(my.data, region.head="county.id", pops=my.populations)
#'


calculate_prevalence <- function ( data, pops=NULL, conf.level=0.95, region.head="region", scale=1 ){

    ci <- !is.null(pops)
    regioncol <- which(names(data) == region.head)
    data[, regioncol] <- factor(sanitize_text(as.character(data[, regioncol])))
    if (!ci) 
        pops <- data.frame(region = unique(data[, regioncol]), population = 1)
    names(pops) <- c("region", "population")
    prev <- as.data.frame(tapply(data[, regioncol], data[, regioncol], length))
    names(prev) <- "cases"
    prev <- merge(pops, prev, by.x = "region", by.y = "row.names", all = TRUE)
    prev$cases[is.na(prev$cases)] <- 0
	
	#print(prev)
	
    if (ci) {
        prev <- cbind(prev, scale * binconf(prev$cases, prev$population, alpha = 1 - 
            conf.level))
        names(prev) <- c("region", "population", "cases", "prevalence", "lower", 
            "upper")
    } else {
        prev <- cbind(prev, scale * prev$cases/prev$population)
        names(prev) <- c("region", "population", "cases", "prevalence")
    }
    return(prev)
} 
