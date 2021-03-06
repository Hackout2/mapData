#' Calculate prevalence of infection
#'
#' A function to calculate the prevalence of infection within regions of aggregation from line-list data: add up the total number of cases in each area, divide by the population and scale as appropriate. 
#'
#' @import Hmisc
#'
#' @param data A line list-type data frame (one line per case), including a column with the region in which the case occurred
#' @param pops A data frame containing one line per region included in \code{data}, with first column for region name and second for population. If \code{pops} is missing, the total number of cases in each region will be calculated. 
#' @param region.head A string containing the name of the column in \code{data} which specifies the region in which each case occurred
#' @param conf.level Level of confidence interval required for prevalence estimates.
#' @param scale Scaling with which to report prevalence. Default (\code{scaling=1}) is per head; \code{scaling=100000} would indicate prevalence per 100,000, etc.
#' @return A data frame containing total population (if regional populations were specified), number of cases and prevalence (if appropriate) for each region.
#'
#' @export
#'
#' @examples
#' my.data <- data.frame(county.id = ceiling(3*runif(10)),
#'                      age = rlnorm(10),
#'						sex = factor(floor(2*runif(10)), levels=c(0,1), labels=c("male", "female"))
#'                      )
#'
#' my.populations <- data.frame(county.id = 1:3,
#'				population = c(10, 50, 100)
#'				)
#'
#' # example without population of each region
#' calculate_prevalence(my.data, region.head="county.id")
#'
#' # example with populations
#' calculate_prevalence(my.data, region.head="county.id", pops=my.populations)
#'
calculate_prevalence <- function (data, pops=NULL, conf.level=0.95, region.head="region", scale=1 ){

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

    if (ci) {
        prev <- cbind(prev, scale * binconf(prev$cases, prev$population, alpha = 1 -
            conf.level))
        names(prev) <- c("region", "population", "cases", "prevalence", "lower",
            "upper")
        return(prev)
    } else {
        prev <- prev[,-2]
        names(prev) <- c("region", "cases")
        return(prev)
    }
}
