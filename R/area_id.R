#'  area_id function
#'
#' This is the function description
#' @param points SpatialPointsDataFrame containing the line list of patients
#' @param polygons SpatialPolygonsDataFrame containing the areas
#' @param polygonID columnname of the SpatialPolygonsDataFrame specifying the ID of the area
#' @import sp
#' @export
#' @return a SpatialPolygonsDataFrame containing the inputted line list, with an additional column specifying the ID of the area per patient
#'
area_id <- function(points, polygons, polygonID) {
    if (proj4string(points) == proj4string(polygons)) {
        x <- over(points, polygons)
        ID <- as.factor(x[, polygonID])
        points@data$newval <- ID
        names(points@data)[names(points@data) == "newval"] <- polygonID
        return(points)
    } else {
        stop("ERROR: projections do not match")
    }
}
