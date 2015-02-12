
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
#' agg_summaries(dat, var = "latitude", group = "code", mean) %>% head
#' agg_summaries(dat, var = "latitude", group = "code", median) %>% head
#' agg_summaries(dat, var = "latitude", group = "code", quantile, .1) %>% head
#' }
agg_summaries <- function(foo, var, group, FUN = mean, ...) {
  fun <- match.fun(FUN)
  foo %>% group_by_(group) %>% summarise_(interp(~fun(var, ...), var = as.name(var)))
}





