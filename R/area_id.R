#'  Allocate cases to areas
#'
#' Allocates the spatial location of infected cases to the area/region within which they fall, to allow later aggregation and other manipulation by area.
#' @param points SpatialPointsDataFrame containing the line list of patients
#' @param polygons SpatialPolygonsDataFrame containing the areas
#' @param polygonID columnname of the SpatialPolygonsDataFrame specifying the ID of the area
#' @import sp
#' @export
#' @return a SpatialPolygonsDataFrame containing the inputted line list, with an additional column specifying the ID of the area per patient
#'
#' @examples
#' data(France)
#' data(sheep_data)
#' sheep_spdf <- SpatialPointsDataFrame(
#' 		data.frame(longitude = sheep_data$longitude, latitude = sheep_data$latitude), 
#'		data = sheep_data,
#'		proj4string = CRS("+proj=longlat +datum=WGS84"))
#' # write the area id in a new column
#' sheep_spdf <- area_id(sheep_spdf, France, 'NOM_DEPT')
#' # compare to the departement already in the sheep_data frame
#' # usually they agree!
#' summary(sheep_spdf$NOM_DEPT == sheep_spdf$departement)
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
