
#' agg_summaries
#'
#' Aggregate summaries on foo
#'
#' @import dplyr
#' @import assertthat
#' @importFrom lazyeval interp
#' @import maptools
#' @export
#' @examples \dontrun{
#' agg_summaries(dat, var = "latitude", group = "code", mean)
#' agg_summaries(dat, var = "latitude", group = "code", median)
#' agg_summaries(dat, var = "latitude", group = "code", quantile, .1)
#' }
agg_summaries <- function(foo, var, group, FUN = mean, ...) {
  browser()
  fun <- match.fun(FUN)
  interp(~fun(var, ...)
  foo %>% group_by_(group) %>% summarise_(interp(~fun(var, ...), var = as.name(var)))
}





