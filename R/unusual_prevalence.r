#' unusual.performance.region
#'
#' more text describing the function
#' @param data foo
#' @param pops foo
#' @param region.head foo
#' @param region.i foo
unusual_performance_region <- function(data, pops, region.head, region.i) {
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
#' more text here
#' @param data placeholder text
#' @param pops  placeholder text
#' @param conf.level  placeholder text
#' @param region.head  placeholder text
#' @param scale  placeholder text
calculate_prevalence_unusual_pval <- function(data, pops = NULL, conf.level = 0.95, 
    region.head = "region", scale = 1) {
    prev <- calculate_prevalence(data = data, pops = pops, conf.level = conf.level, 
        region.head = region.head, scale = scale)
    p.values <- sapply(1:nrow(prev), function(i) unusual.performance.1region(data = data, 
        pops = pops, region.head = region.head, region.i = prev$region[i]))
    p.bonferroni <- 1 - (1 - p.values)^nrow(prev)
    prev$p.val.bonferroni <- p.bonferroni
    prev$sign <- sign(prev$prevalence - sum(prev$cases)/sum(prev$population))
    return(prev)
}