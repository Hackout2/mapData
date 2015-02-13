#' Estimate density
#'
#' function to estimate 2d density, from data frame output by density_data
#' Produce a 2-d estimated intensity function (cases per unit area) from an (x, y, (corrected) counts) data frame. 
#' @param density_data A data frame with columns x, y and (corrected) number of individuals, as output by the density_data function.
#' @return A SpatialGridDataFrame object containing intensity/prevalence estimates (@@data$v), at a grid of points (@@grid). 
estimate_density <- function(density_data) {
    # construct a ppp object for use by the density estimator
    my.ppp <- ppp(xs, ys, window = owin(pointspatients@bbox[1, ], pointspatients@bbox[2, 
        ]), n = as.vector(ns), marks = rep(1, times = sum(!duplicated(paste(pointspatients@coords[, 
        1], pointspatients@coords[, 2])))))
    # density estimator
    my.density <- density(my.ppp)
    # return as a spatial grid data frame
    return(as.SpatialGridDataFrame.im(my.density))
} 
