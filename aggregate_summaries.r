rm(list=ls(all=TRUE))
library(sp)
library(maptools)
library(rgdal)

library(devtools)

shp = readShapePoly("../mapR/DEPARTEMENT/DEPARTEMENT.SHP")
proj4string(shp) = CRS("+init=epsg:2154")
shp = spTransform(shp, CRS("+proj=longlat +datum=WGS84"))

dat = read.csv("../mapR/OIE_outbreaks_France_20120510.csv")
dat.sp = SpatialPoints(cbind(dat$longitude,dat$latitude))
proj4string(dat.sp) = CRS("+proj=longlat +datum=WGS84")

plot(shp)
axis(1)
axis(2)
points(dat$longitude,dat$latitude,col=2)

dep.df = over(dat.sp,shp)

dat = cbind(dat,polygonID=dep.df$CODE_DEPT)


by.var = as.factor(dep.df$CODE_DEPT)
quant.var = dat$latitude




## documentation.
#' @param x numeric vector to be summarised.
#' @param by factor vector of the same length as x defining the classes of aggregation for x.
#' @param FUN summary function to be used, e.g. mean, median, quantile
#' Aggregates the variable supplied in x into classes defined by by. The syntax is similar to that of function aggregate.
#' @return A dataframe containing the levels defined in variable by, and the summary(ies) of x using function FUN.
fun.mean.by.var = function(x,by,FUN="mean",...) {
    ## to do: make the names(tt.full) pretty or informative.
    ## to do: optimize this using the pipe operator %>%
    if(!is.numeric(x)) stop("error: numeric class required for aggregating the mean.\n")

    if(FUN=="count") {
        tt = table(by) # this will give the right number of columns as long as by is a factor.
        tt.full = data.frame(by=names(tt),count=as.numeric(tt))

    } else {
    
        x.agg = aggregate(x,by=list(by=by),FUN=FUN,...) ## this won't contain the empty depts.
        my.names = colnames(x.agg$x)
        if(is.null(my.names)) {
            my.names = "x"
        } else {
            my.names = paste("x.",my.names,sep="")
        }
        x.agg = data.frame(by=x.agg$by,x.agg$x) ## flatten out in case the FUN returns a matrix rather than a vector.
        names(x.agg)[-1] = my.names
        ## if(is.null(dim(x.agg$x))) {
        ##     names(x.agg)[-1] = "x"
        ## } else {
        ##     names(x.agg)[-1] = colnames(x.agg$x)
        ## }
        tt.full = merge(data.frame(by=levels(by)),x.agg,by="by",all=TRUE)
    }
    return(tt.full)
}

#' @param linelist dataframe containing both the variable to be aggregated and that defining the aggregation categories
#' @param x either an integer indicating the column number or a text string indicating the variable name of the variable to be aggregated.
#' @param by either an integer indicating the column number or a text string indicating the variable name of the variable supplying the aggregation categories.
#' @param na.rm logical, passed to function mean.
#' @return A dataframe containing the levels defined in variable by, and the summary(ies) of x using function FUN.
mean.by = function(linelist,x,by,na.rm=FALSE,...) {
    # check if variables of the names supplied exist in the dataframe:
    if(is.character(x)) x = match(x,names(linelist))
    if(x>ncol(linelist) | is.na(x)) stop("error: invalid x supplied.\n")
    if(is.character(by)) by = match(by,names(linelist))
    if(by>ncol(linelist) | is.na(by)) stop("error: invalid by supplied.\n")
    
    return(fun.mean.by.var(x=linelist[,x],by=linelist[,by],FUN=mean,na.rm=na.rm))
}
    
mean.by(dat,"longitude","polygonID",na.rm=TRUE)

head(fun.mean.by.var(quant.var,by.var,"mean"))
head(fun.mean.by.var(quant.var,by.var,"median"))
head(fun.mean.by.var(quant.var,by.var,"quantile",probs=c(0.1,0.5,0.9)))
