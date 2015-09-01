#' Estimate density
#'
#' function to estimate 2d density, from data frame output by density_data
#' Produce a 2-d estimated intensity function (cases per unit area) from an (x, y, (corrected) counts) data frame.
#'
#' @param density_data A data frame with columns x, y and (corrected) number of individuals, as output by the \code{density_data} function.
#' @param show.density Set to TRUE to plot the data and calculated density.
#'
#' @return A SpatialGridDataFrame object containing intensity/prevalence estimates (\code{@@data$v}), at a grid of points (\code{@@grid}).
#'
#' @export
#'
#' @examples
#' if(require(sp)){
#' # simulate a line list of patient locations
#' my.linelist <- data.frame(	longitude = round(runif(100, 0, 10), 0.01),
#'								latitude = round(runif(100, 0, 10), 0.01)
#'							)
#'
#' # transform patients dataframe into spatialpointsdataframe
#' pointspatients <- SpatialPointsDataFrame(my.linelist, data=data.frame(id=1:100))
#'
#' my.density.data <- density_data(pointspatients)
#' my.density.estimate <- estimate_density(my.density.data, show.density=TRUE)
#'
#' }
#'
estimate_density <- function(density_data, show.density=FALSE){

	# construct a ppp object for use by the density estimator
	my.ppp <- spatstat::ppp(
					density_data$x,
					density_data$y,
					window = owin(range(density_data$x), range(density_data$x)),
					n = density_data$count,
					marks = rep(1, times = sum(!duplicated(density_data[, 1:2])) )
					)

	# density estimator

	my.density <- spatstat::density(my.ppp)

	if(show.density){
		plot(my.density)
		points(density_data$x, density_data$y, pch=16, col=rgb(0,0,0,density_data$count/max(density_data$count)))
		}

	# return as a spatial grid data frame
	return(marmap::as.SpatialGridDataFrame.im( my.density ) )

}
