#' agg_summaries
#'
#' Aggregate summaries on foo
#'
#' @param input_df The input line incidence file
#' @param  var Variable to aggregate
#' @param  group Variable to group by
#' @param  FUN use mean or median
#' @param  quants If you need quantiles, skip FUN and set the probability here. 
#' @importFrom lazyeval interp
#' @export
#' @examples \dontrun{
#' agg_summaries(dat, var = 'latitude', group = 'code', mean)
#' agg_summaries(dat, var = 'latitude', group = 'code', median)
#' # For quantiles, use quants arg and provide the probability
#' agg_summaries(dat, var = 'latitude', group = 'code', quantile, 1)
#' agg_summaries(dat, var = 'latitude', group = 'code', quants, .25)
#' }
agg_summaries <- function(input_df, var, group, FUN = mean, quants = NULL) {
    if (is.null(quants)) {
        fun <- match.fun(FUN)
    } else {
        fun <- function(var, probs = quants) {
            quantile(var, probs)
        }
    }
    input_df %>% dplyr::group_by_(group) %>% dplyr::summarise_(.dots = interp(~fun(var), var = as.name(var))) %>% 
        dplyr::select(everything(), aggregate = starts_with("fun"))
} 
