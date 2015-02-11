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
#' @param df input data.frame
#' @param lat column name of the latitude data
#' @param column name of the longitude data
#' Checks line list data for valid locations
#' @importFrom assertthat assert_that not_empty
#' @import dplyr
#' @examples \dontrun{
#' # This one below is obviously a trivial example
#' # The iris dataset contains no spatial information but we can pretend that these two columns are lat/long
#' check_data(iris, 'Petal.Length', 'Petal.Width')
#' # Now let's add an incorrect longitude
#' iris$Petal.Width <- -1800
#' check_data(iris, 'Petal.Length', 'Petal.Width')
#' # This should return FALSE
#' }
check_data <- function(df = NULL, lat = NULL, long = NULL) {
    if (not_empty(df)) 
        {
            if (is.null(lat) && is.null(long)) {
                expected <- c("latitude" %in% names(df) && "longitude" %in% names(df), 
                  "lat" %in% names(df) && "long" %in% names(df))  # end of lat/long else 
                if (any(expected) == FALSE) {
                  message("Could not detect columns containing lat/long. Please specify in function call")
                  return(FALSE)
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
                # If the user specifies a non-standard name for lat and long
                lat <- df[, which(names(df) %in% lat)]
                long <- df[, which(names(df) %in% long)]
                lat_check <- test_lat(lat)
                long_check <- test_long(long)
                # if they specify column names
            }
            if (!lat_check) {
                message("Errors detected in latitude data")
                return(FALSE)
            }
            if (!long_check) {
                message("Errors detected in longitude data")
                return(FALSE)
            }
        }  # not_empty
}  # full function 
#' Converts columns with dates into a R date class
#'
#' Using lubridate, this function will transform dates from mdy, mdy_h, mdy_hm, mdy_hms (or starting with day instead of month) into valid date classes. It will return an error it if cannot coerce the date itself.
#' @param df The input data.frame
#' @param  date The column name containing the dates
#' @param  format The format of the date. 
#' @export
#' @import lubridate
#' @examples \dontrun{
#' new <- fix_dates(goat_data, 'start.date', 'dmy')
#'}
fix_dates <- function(df, date = NULL, format = NULL) {
    if (is.null(format)) 
        stop("Please specify a format. Formats can be mdy, mdy_h, mdy_hm, mdy_hms, ymd, ymd_h, ymd_hm, ymd_hms")
    if (is.null(date)) 
        stop("Please specify a date column")
    format <- match.fun(format)
    df[, which(names(df) %in% date)] <- format(df[, which(names(df) %in% date)])
    df
}
new <- fix_dates(z, "start.date", "dmy") 
