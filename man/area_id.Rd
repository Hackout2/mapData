% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/area_id.R
\name{area_id}
\alias{area_id}
\title{area_id function}
\usage{
area_id(points, polygons, polygonID)
}
\arguments{
\item{points}{SpatialPointsDataFrame containing the line list of patients}

\item{polygons}{SpatialPolygonsDataFrame containing the areas}

\item{polygonID}{columnname of the SpatialPolygonsDataFrame specifying the ID of the area}
}
\value{
a SpatialPolygonsDataFrame containing the inputted line list, with an additional column specifying the ID of the area per patient
}
\description{
This is the function description
}

