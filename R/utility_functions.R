#' Geocode a location
#'
#' Geocodes a location using Google Maps.
#' @param location The name of a location
#' @importFrom ggmap geocode
#' @export
md_geocoder <- function(location = NULL) {
    if (!is.null(location)) {
        geocode(location)
    }
} 
