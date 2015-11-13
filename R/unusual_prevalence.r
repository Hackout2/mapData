#' unusual.prevalence.region
#'
#' Calculating the p-value testing whether a particular region has
#' unusual prevalence compared to the overall mean using leave-one-out
#' crossvalidation. This means that the overall mean is computed
#' leaving out the data from the region being tested. If a population
#' dataset is supplied, Fisher's exact test is used to calculate the
#' p-value. If no population data is supplied, as null hypothesis
#' Poisson distributed number of cases per region is assumed.
#'
#' @param data a line list-type dataframe containing a line for each case, with its region and the regional population.
#' @param pops dataframe containing the region ID in the first and the population size for each region in the dataset in the second column.
#' @param region.head variable name of the column specifying the region in data.
#' @param region.i ID of the region being tested.
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
#' # Is prevalence in county 2 particularly high or low (given populations)?
#' unusual_prevalence_region(my.data, 'county.id', '2', my.populations)
#'
#' # Same thing, with no population data
#' unusual_prevalence_region(my.data, 'county.id', '2')
#'
#' @return
#' If a population dataset is supplied, a Fisher p-value under the null 
#' hypothesis that the prevalence in the region being tested is the same 
#' as in all other areas. If no population data is supplied, the null 
#' hypothesis is a Poisson distribution for the number of cases per region.

unusual_prevalence_region <- function(data, region.head, region.i, pops=NULL) {
    data$curr.col <- (data[, region.head] == region.i)
    if (is.null(pops)) {
        tt <- table(data[, region.head] == region.i)
        expected <- tt["FALSE"]/(length(table(data[, region.head])) - 1)
        # if the region variable is coded as a factor, then we'll get the number of levels in this factor as the baseline
        #, if it isn't, then it'll be the number of regions with non-zero incidence.
        p.value <- ppois(q = tt["TRUE"], expected)
        p.value <- 2 * min(p.value, 1 - p.value)  # for the 2-tailed test
    } else {
        names(pops) <- c("region", "population")
        pops.i <- aggregate(pops$pop, by = list(region = (pops$region == region.i)), sum)
        inc.mat <- calculate_prevalence(data = data, pops = pops.i, region.head = "curr.col")[, c("population", "cases")]
        p.value <- fisher.test(inc.mat)$p.value
    }
    p.value
}



#' calculate_prevalence_unusual_pval
#'
#' Testing if any regions in the dataset have a significantly
#' different prevalence than the overall mean using leave-one-out
#' crossvaliation. This means that the overall mean is re-calculated
#' for each region, leaving out the data from the region in
#' question. If population sizes for the regions are supplied,
#' Fisher's exact test is used to calculate the p-value. If no
#' population data is supplied, as null hypothesis Poisson distributed
#' number of cases per region is assumed. P-values are corrected for
#' multiple testing using the Bonferroni correction.
#'
#' @param data a dataframe containing the number of cases and total population for all regions in the dataset.
#' @param pops dataframe containing the region ID in the first and the population size for each region in the dataset in the second column
#' @param conf.level  Confidence level to be used for calculating the confidence intervals on the prevalence estimates.
#' @param region.head  variable name of the incidence column in data.
#' @param scale  Scaling with which to report prevalence (per head, per 100 000, etc.)
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
#' # Is prevalence in county 2 particularly high or low (given populations)?
#' calculate_prevalence_unusual_pval(my.data, pops = my.populations, region.head = 'county.id')
#'
#' # Same thing, with no population data
#' calculate_prevalence_unusual_pval(my.data, region.head = 'county.id')
#'
#' @return
#' If a population dataset is supplied, a data frame containing Fisher p-values
#' under the null hypothesis that the prevalence in the region being tested is 
#' the same as in all other areas. If no population data is supplied, the null 
#' hypothesis is a Poisson distribution for the number of cases per region. The 
#' data frame also contains a column indicating whether the observed prevalence 
#' was higher (+1) or lower (-1) than expected.

calculate_prevalence_unusual_pval <- function(data, pops = NULL, conf.level = 0.95,
    region.head = "region", scale = 1) {
    prev <- calculate_prevalence(data = data, pops = pops, conf.level = conf.level,
        region.head = region.head, scale = scale)
    p.values <- sapply(1:nrow(prev), function(i) unusual_prevalence_region(data = data,
        pops = pops, region.head = region.head, region.i = prev$region[i]))
    p.bonferroni <- 1 - (1 - p.values)^nrow(prev)
    prev$p.val.bonferroni <- p.bonferroni
    
    if(!is.null(pops))
        prev$sign <- sign(prev$prevalence - sum(prev$cases)/sum(prev$population))
    else
    	prev$sign <- sign(prev$cases - mean(prev$cases) + prev$cases/nrow(prev)) # is no. cases greater or smaller than leave-one-out mean?
    return(prev)
}
