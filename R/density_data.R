# process a SpatialPointsDataFrame to give numbers of cases at each unique (x,y) location. Weighting by 1/(local population density) gives corrected numbers which can be used to calculate estimated prevalence.

#' Process a SpatialPointsDataFrame to give numbers of cases at each unique (x,y) location. Weighting by 1/(local population density) gives corrected numbers which can be used to calculate estimated prevalence.

#' @param points SpatialPointsDataFrame containing the line list of patients. Should include: 1) a slot @coords with an n x 2 vector giving the x and y coordinates of each case. (More than one case can have the same co-ordinates.) 2) a slot @bbox giving the maximum and minimum x and y co-ordinates. 

#' @param weights If required, the local population density at each (x, y) coordinate.

#' @return A data frame containing x and y co-ordinates, and (weighted) counts. For use by other functions including estimate_density. 

density_data <- function(points, weights=NULL){
	
	# x co-ordinates
	xs <- pointspatients@coords[which(!duplicated(paste(pointspatients@coords[,1], pointspatients@coords[,2]))),1]

	# y co-ordinates
	ys <- pointspatients@coords[which(!duplicated(paste(pointspatients@coords[,1], pointspatients@coords[,2]))),2]

	# number of cases at each point
	ns <- tapply(
					pointspatients@data[,1],
					paste(pointspatients@coords[,1], pointspatients@coords[, 2], sep=","),
					length)

	if(!is.null(weights))
		ns <- ns*weights
	
	return(data.frame(	x = xs,
						y = ys,
						count = ns,
						row.names = 1:length(xs)
						))
	
}