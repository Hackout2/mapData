
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
#' # For quantiles, use quants arg and provide the probability
#' agg_summaries(dat, var = "latitude", group = "code", quantile, 1)
#' agg_summaries(dat, var = "latitude", group = "code", quants, .25)
#' }
agg_summaries <- function(foo, var, group, FUN = mean, quants = NULL) {
  if(is.null(quants)) {
  fun <- match.fun(FUN) }
  else {
    fun <- function(var, probs = quants) { quantile(var, probs) }
  }
  foo %>% group_by_(group) %>% summarise_(.dots = interp(~fun(var), var = as.name(var))) %>% select(everything(), aggregate = starts_with("fun"))
}




