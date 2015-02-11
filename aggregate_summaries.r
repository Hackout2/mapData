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
    if(class(x) != "numeric") stop("error: numeric class required for aggregating the mean.\n")
    
    tt.full = rep(NA,length(levels(by)))
    names(tt.full) = levels(by)
    x.agg = aggregate(x,by=list(by),FUN=FUN,...) ## this won't contain the empty depts.
    tt.full[match(x.agg$Group.1,names(tt.full))] = x.agg$x
    return(tt.full)
}

%>% # pipe operator. for optimization


fun.mean.by.var(quant.var,by.var,mean)
fun.mean.by.var(quant.var,by.var,median)
fun.mean.by.var(quant.var,by.var,quantile,probs=c(0.1,0.5,0.9))
