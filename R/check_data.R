#' Valid latitude
#'
#' Test for valid latitude. Between -90 and 90
#' @param x A latitude
#' @export
#' @examples \dontrun{
#' expect_true(test_lat(dat$latitude))
#'}
test_lat <- function(x) {
    all(x > -90 & x < 90)
}
#' Valid longitude
#' Test for valid longitude. Between -180 and 180
#' @param x A longitude
#' @export
#' @examples \dontrun{
#' expect_true(test_lat(dat$longitude))
#'}
test_long <- function(x) {
    all(x > -180 & x < 180)
}
#' Check Data
#' 
#' Checks line list data for valid locations
#' @import assertthat
#' 
check_data <- function(df = NULL, lat = NULL, long = NULL) {
    if (not_empty(df)) 
        {
            if (is.null(lat) && is.null(long)) {
                expected <- c("latitude" %in% names(df) && "longitude" %in% names(df), 
                  "lat" %in% names(df) && "long" %in% names(df))  # end of lat/long else 
                if (all(expected)) {
                  stop("Could not detect columns containing lat/long. Please specify in function call")
                }
                if (expected[1]) {
                  # If they are named latitude and longitude
                  lat_check <- test_lat(df$latitude)
                  long_check <- test_long(df$longitude)
                }
                if (expected[2]) {
                  # If they are named lat/long
                  lat_check <- test_lat(df$lat)
                  long_check <- test_long(df$long)
                }
            } else {
                # if they specify column names
            }
            if (!lat_check) {
                message("Errors detected in latitude data")
            }
            if (!long_check) {
                message("Errors detected in longitude data")
            }
        }  # not_empty
}  # full function 
