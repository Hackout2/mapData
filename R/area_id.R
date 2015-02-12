#############################################################
#This section can be deleted later on, just included to have some input data

# library(maptools)
# library(rgdal)
# library(sp)

# setwd("N:/Hackout2/mapR")

# #read data
# #linelist
# patients<- read.csv("OIE_outbreaks_France_20120510.csv")
# patients[, "ID"]<- c(seq(1: length(patients[,"departement"])))
# #shapefile
# shpfrance<- readShapeSpatial("DEPARTEMENT/DEPARTEMENT")

# #add right projection to shapefile
# proj4string(shpfrance) <- CRS("+init=epsg:2154")
# shpfrance <- spTransform(shpfrance, CRS("+proj=longlat +datum=WGS84"))

# #transform patients dataframe into spatialpointsdataframe
# pointspatients<- SpatialPointsDataFrame(cbind(patients[,"longitude"], patients[,"latitude"]),patients)
# proj4string(pointspatients) <- CRS("+proj=longlat +datum=WGS84")
# pointspatients <- spTransform(pointspatients, CRS("+proj=longlat +datum=WGS84"))
#############################################################################

# area_id function

#' @param points SpatialPointsDataFrame containing the line list of patients
#' @param polygons SpatialPolygonsDataFrame containing the areas
#' @param polygonID columnname of the SpatialPolygonsDataFrame specifying the ID of the area
#' @import sp
#' @return a SpatialPolygonsDataFrame containing the inputted line list, with an additional column specifying the ID of the area per patient


area_id<- function(points, polygons, polygonID){
        if (proj4string(points)== proj4string(polygons)){
                x<- over(points, polygons)
                ID<- as.factor(x[,polygonID])
                points@data$newval<- ID
                names(points@data)[names(points@data)=="newval"]<- polygonID
                return(points) 
        }
        else {
                stop("ERROR: projections do not match")
        }
}


#pointspatients<-  area_id(pointspatients, shpfrance, polygonID="CODE_DEPT")


