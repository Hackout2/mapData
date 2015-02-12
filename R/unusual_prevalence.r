rm(list=ls(all=TRUE))
library(maptools)
library(assertthat)
library(Hmisc)
library(fields)
library(RColorBrewer)

source("check_data.R")
source("calculate_prevalence.R")
dat = read.csv("../exampledata.csv",stringsAsFactors=FALSE)#,colClasses=c("numeric","numeric","character","numeric"
dat$CODE_DEPT = sprintf("%02d",dat$CODE_DEPT)
#dat$CODE_DEPT = factor(dat$CODE_DEPT,levels=shp$CODE_DEPT)
dat = dat[,-1]

shp = readShapePoly("../../mapR/DEPARTEMENT/DEPARTEMENT.SHP")
shp.com = readShapePoly("../../mapR/1_DONNEES_LIVRAISON_2014-12-00066/GEOFLA_2-0_SHP_LAMB93_FR-ED141/COMMUNE/COMMUNE.SHP")


unusual.performance.1region = function(data,pops,region.head,region.i) {
    data$curr.col = (data[,region.head]==region.i)
    if(is.null(pops)) {
        tt = table(data[,region.head]==region.i)
        expected = tt["FALSE"]/(length(table(data[,region.head]))-1) # if the region variable is coded as a factor, then we'll get the number of levels in this factor as the baseline, if it isn't, then it'll be the number of regions with non-zero incidence.
        p.value = ppois(q=tt["TRUE"],expected) 
        p.value = 2*min(p.value,1-p.value) # for the 2-tailed test
    } else {
        names(pops) = c("region","population")
        pops.i = aggregate(pops$pop,by=list(region=(pops$region==region.i)),sum)
        inc.mat = calculate_prevalence(data=data,pops=pops.i,region.head="curr.col")[,c("population","cases")]
        p.value = fisher.test(inc.mat)$p.value 
    }
    return(p.value)
}


calculate_prevalence_unusual_pval = function(data,pops=NULL,conf.level=0.95,region.head="region",scale=1) {
    prev = calculate_prevalence(data=data,pops=pops,conf.level=conf.level,region.head=region.head,scale=scale)
    p.values = sapply(1:nrow(prev),function(i) unusual.performance.1region(data=data,pops=pops,region.head=region.head,region.i=prev$region[i]))
    p.bonferroni = 1-(1-p.values)^nrow(prev)
    prev$p.val.bonferroni = p.bonferroni
    prev$sign = sign(prev$prevalence-sum(prev$cases)/sum(prev$population))
    
    return(prev)
}




my.pops = aggregate(shp.com$POPULATION,by=list(CODE_DEPT=shp.com$CODE_DEPT),sum)

ptm = proc.time()
prev = calculate_prevalence_unusual_pval(data=dat,pops=my.pops,region.head="CODE_DEPT")
proc.time()-ptm

prev = prev[match(shp$CODE_DEPT,prev$region),]

n.cols = 100
col.breaks = seq(0-1e-10,max(prev$prevalence)+1e-10,length.out=n.cols+1)

par(mfrow=c(1,2))
colors = brewer.pal(5,"RdYlBu")
pal = colorRampPalette(colors)
my.cols = rev(pal(n.cols))

col.breaks = seq(0-1e-10,max(prev$prevalence)+1e-10,length.out=n.cols+1)
plot(shp,col=my.cols[findInterval(prev$prevalence,col.breaks)])
image.plot(legend.only=TRUE,col=my.cols,zlim=range(col.breaks),horizontal=TRUE)
title(main="prevalence")

p.val.2ended = prev$p.val.bonferroni
p.val.2ended[prev$sign==-1] = 2-prev$p.val.bonferroni[prev$sign==-1]
p.val.2ended = 2-p.val.2ended
col.breaks=c(seq(-1e-10,0.1,length.out=n.cols/2),1,seq(1.9,2+1e-10,length.out=n.cols/2))
plot(shp,col=my.cols[findInterval(p.val.2ended,col.breaks)])
image.plot(legend.only=TRUE,col=my.cols,breaks=col.breaks,horizontal=TRUE,zlim=range(col.breaks))
title(main="p-value")

mean.prev = sum(prev$cases)/sum(prev$population)
sample.size = round(10^seq(4,7,length.out=100))
CI = binconf(round(mean.prev*sample.size),sample.size)

pct.2.5 = sapply(1:length(sample.size), function(i) qbinom(1-(1-0.025)^(1/nrow(prev)),size=sample.size[i],prob=mean.prev)/sample.size[i])
pct.97.5 = sapply(1:length(sample.size), function(i) qbinom((1-0.025)^(1/nrow(prev)),size=sample.size[i],prob=mean.prev)/sample.size[i])


plot(prev$population,prev$prevalence,col=my.cols[findInterval(p.val.2ended,col.breaks)],log="x")
lines(c(1,max(prev$population)),rep(mean.prev,2),lty=2,col="grey")
lines(pop,CI[,"Lower"],col=2)
lines(pop,CI[,"Upper"],col=2)

lines(sample.size,pct.2.5,col=4)
lines(sample.size,pct.97.5,col=4)
