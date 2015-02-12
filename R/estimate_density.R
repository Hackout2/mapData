# function to estimate 2d density of cases or prevalence

#' Produce a 2-d estimated intensity function (cases per unit area) from geographical location of cases. Weighting by 1/(local population density) gives an estimate of prevalence.

#' @param points SpatialPointsDataFrame containing the line list of patients. Should include: 1) a slot @coords with an n x 2 vector giving the x and y coordinates of each case. (More than one case can have the same co-ordinates.) 2) a slot @bbox giving the maximum and minimum x and y co-ordinates. 

#' @param weights If required, the local population density at each (x, y) coordinate.

#' @return A SpatialGridDataFrame object containing intensity/prevalence estimates (@data$v), at a grid of points (@grid). 

estimate_density <- function(points, weights=NULL){
	
	# x co-ordinates
	xs <- pointspatients@coords[which(!duplicated(paste(pointspatients@coords[,1], pointspatients@coords[,2]))),1]

	# y co-ordinates
	ys <- pointspatients@coords[which(!duplicated(paste(pointspatients@coords[,1], pointspatients@coords[,2]))),2]

	# number of cases at each point
	ns <- tapply(
					pointspatients@data[,1],
					paste(pointspatients@coords[,1], pointspatients@coords[, 2], sep=","),
					length)
	
	# construct a ppp object for use by the density estimator			
	my.ppp <- ppp(
					xs,
					ys,
					window = owin(pointspatients@bbox[1,], pointspatients@bbox[2,]),
					n = as.vector(ns),
					marks = rep(1, times = sum(!duplicated(paste(pointspatients@coords[,1], pointspatients@coords[,2]))) )
					)

	# density estimator
	
	if(is.null(weights))
		my.weights <- NULL
	else
		my.weights <- weights[which(!duplicated(paste(pointspatients@coords[,1], pointspatients@coords[,2])))]

	my.density <- density(	my.ppp, my.weights)
	
	# return as a spatial grid data frame
	return(as.SpatialGridDataFrame.im( my.density ) )
	
}