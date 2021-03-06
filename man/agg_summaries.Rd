% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/agg_summaries.R
\name{agg_summaries}
\alias{agg_summaries}
\title{agg_summaries}
\usage{
agg_summaries(input_df, var, group, FUN = mean, quants = NULL)
}
\arguments{
\item{input_df}{The input line incidence file}

\item{var}{Variable to aggregate}

\item{group}{Variable to group by}

\item{FUN}{use mean or median}

\item{quants}{If you need quantiles, skip FUN and set the probability here.}
}
\description{
Aggregate summaries on foo
}
\examples{
\dontrun{
agg_summaries(dat, var = 'latitude', group = 'code', mean)
agg_summaries(dat, var = 'latitude', group = 'code', median)
# For quantiles, use quants arg and provide the probability
agg_summaries(dat, var = 'latitude', group = 'code', quantile, 1)
agg_summaries(dat, var = 'latitude', group = 'code', quants, .25)
}
}

