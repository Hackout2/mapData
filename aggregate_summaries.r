rm(list=ls(all=TRUE))
library(sp)
library(maptools)
library(rgdal)

library(devtools)

shp = readShapePoly("DEPARTEMENT/DEPARTEMENT.SHP")
proj4string(shp) = CRS("+init=epsg:2154")
shp = spTransform(shp, CRS("+proj=longlat +datum=WGS84"))

dat = read.csv("OIE_outbreaks_France_20120510.csv")
dat.sp = SpatialPoints(cbind(dat$longitude,dat$latitude))
proj4string(dat.sp) = CRS("+proj=longlat +datum=WGS84")

plot(shp)
axis(1)
axis(2)
points(dat$longitude,dat$latitude,col=2)

dep.df = over(dat.sp,shp)

dat = cbind(dat,dep.df$CODE_DEPT)


by.var = as.factor(dep.df$CODE_DEPT)
quant.var = dat$latitude

## counting cases by.var:
table(by.var) ## if by.var is a factor, this will contain even the empty depts.
# count:
aggregate(!is.na(by.var),by=list(dept=dep.df$CODE_DEPT),sum) ## this won't contain the empty depts.


## documentation.
fun.mean.by.var = function(x,by,FUN=mean,...) {
    ## to do: make the names(tt.full) pretty or informative.
    ## to do: optimize this using the pipe operator %>%
    if(class(x) != "numeric") stop("error: numeric class required for aggregating the mean.\n")
    
    x.agg = aggregate(x,by=list(by),FUN=FUN,...) ## this won't contain the empty depts.
    x.agg = data.frame(Group.1=x.agg$Group.1,x.agg$x) ## flatten out in case the FUN returns a matrix rather than a vector.
    tt.full = merge(data.frame(by=levels(by)),x.agg,by.x="by",by.y="Group.1",all=TRUE)
    return(tt.full)
}


fun.mean.by.var(quant.var,by.var,mean)
fun.mean.by.var(quant.var,by.var,median)
fun.mean.by.var(quant.var,by.var,quantile,probs=c(0.1,0.5,0.9))
