require(sp) 
require(xts) 
require(spacetime) 
require(gstat)
library(dplyr)



load("~/UStemperature/results/weather_data3.RData")
lon.lat.1to10 <- weather_data3[,c(1,6,7)]
lon.lat.1to10.uni <- distinct(lon.lat.1to10,lon,lat,.keep_all = T)


library(dplyr)
temp.1to10 <- NULL
for(i in 1:10){
  day <- filter(weather_data3, num==i)
  dat <- day[,c(8,9,6,7,5)]
  ### ШЅжи
  out <- distinct(dat,lon,lat,.keep_all = T)
  tmp = left_join(lon.lat.1to10.uni,out,by = c("lon","lat"))
  tmp = arrange(tmp,lon,lat)
  temp.1to10 = rbind(temp.1to10, tmp)
  
}
temp.1to10 = temp.1to10[,c(1,4,5,2,3,6)]

### test
dim(temp.1to10)[1] == 10*nrow(lon.lat.1to10.uni)

## generate STFDF
sp.1to10 = lon.lat.1to10.uni[,c(2,3)]
row.names(sp.1to10) = lon.lat.1to10.uni$stn
coordinates(sp.1to10)=~lon+lat
library(sp)
sp.1to10 = SpatialPoints(sp.1to10,
                         proj4string=CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 
                                         +datum=WGS84 +no_defs +towgs84=0,0,0"))

time = as.Date("2015-01-01")+0:9
temp.data = data.frame(temp.1to10$temp)
colnames(temp.data)="temp"
library(spacetime)
sp.1to10.time = STFDF(sp.1to10, time, temp.data, endTime = delta(time))


## a[i, j, k] selects spatial features i,
## temporal instances j, and data variable(s) k.
## eg. sp.1to10.time[, "2015-01-01::2015-01-02"]  select data of these two days

## Ensure consistent time reference
Sys.setenv(TZ="UTC") 


na.stations <- which(apply(as(sp.1to10.time, "xts"), 2, function(x) sum(is.na(x))>0))
sp.1to10.time = sp.1to10.time[-na.stations,]
save(sp.1to10.time,file="~/UStemperature/results/sp.1to10.time.RData")

## summarize the structure of sp.1to10.time
summary(sp.1to10.time)

## Calculate sample or residual variogram
## Compare the  lumped variogram ???tted with the model for a certain special day
### (1) lumped variogram
index <- 1:10
spdf.lst <- lapply(index, function(i) {x = sp.1to10.time[,i]; x$ti = i; x})
# lapply returns a list of the same length as x, 
# each element: the result of applying FUN to x 
spdf <- do.call(rbind, spdf.lst) # data of SpatialPointsDataFrame has 2437*10 rows
summary(spdf)


vl <- variogram(temp ~ ti, spdf[!is.na(spdf$temp),], dX=0) 
save(vl,file="~/UStemperature/results/vl.RData")
plot(vl, plot.numbers=T, ylim=range(vl$gamma)+c(-10,10), main=paste("Lumped temperature for 10 days in 2015")) 
(vlm <- fit.variogram(vl, model=vgm(range(vl$gamma)[2], "Exp", range(vl$dist)[2]/3, 0)))
plot(vl, plot.numbers=T, model=vlm, ylim=range(vl$gamma)+c(-10,10),
     main="Lumped temperature for 10 days in 2015")

# plot(p1, split=c(1,1,2,1), more=T) 
# plot(p2, split=c(2,1,2,1), more=F)

### (2) variogram of certain special day (with highest temperature)
(ix <- which.max(sp.1to10$temp))
(temp.max <- sp.1to10$temp[ix])
(station.id <- ix%%dim(sp.1to10)[1])
(station.name <- attributes(sp.1to10@sp@coords)$dimnames[[1]][station.id])
station.xts <- sp.1to10[station.id,] 
hist(station.xts$temp, breaks=20) 
rug(station.xts$temp)

## date of the highest temperature across all stations
(date.xts <- station.xts[which.max(station.xts$temp)])
(date.ix <- index(date.xts))
temp.max.spdf <- sp.1to10[, as(date.ix,"character")] 
temp.max.spdf@coords = temp.max.spdf@coords[,1:2]


## The role of the variogram is to help us understand the structure 
## of spatial-temporal variation rather than to accurately predict the model.

## Investigating spatio-temporal structure
system.time(vst <- variogramST(temp ~ polym(lon, lat, degree=2, raw=TRUE), sp.1to10.time))
summary(vst)
save(vst,file="~/UStemperature/results/vst.RData")
plot(vst, xlab="separation (km)", ylab="separation (+days)", main="Semivariance, Temp")
plot(vst, map = FALSE, xlab="separation (km)", ylab = "Semivariance, Temp")

## 3D wireframe plot
require(lattice) 
plot(vst, wireframe=TRUE)


## Modelling spatio-temporal structure
### (1) Metric models
dim(tmp <- vst[(vst$gamma > 280) & (vst$gamma < 360) & (vst$timelag !=0),])
summary(metric.aniso <- tmp$spacelag/tmp$timelag)
#### initial metric variogram model: 
(vgm.metric <- vgmST(stModel="metric", 
                     joint=vgm(370,"Exp",20,0),
                     nugget=20, stAni=median(metric.aniso)))
vgmf.metric <- fit.StVariogram(vst, vgm.metric, method="L-BFGS-B", 
                               control=list(maxit=1024)) 
print(vgmf.metric)
plot(vst, vgmf.metric) 
plot(vst, vgmf.metric, map=FALSE)


### (2) Separable models
(estimated.sill <- quantile(na.omit(vst$gamma), .8))
(vgm.sep <- vgmST(stModel="separable", space=vgm(0.9,"Exp", 300,0.1), 
                  time=vgm(0.95,"Exp", 2, 0.05), sill=estimated.sill))
vgmf.sep <- fit.StVariogram(vst, vgm.sep, method="L-BFGS-B", 
                            lower=c(100,0.001,1,0.001,40), 
                            control=list(maxit=500)) 
attr(vgmf.sep, "optim.output")$par
attr(vgmf.sep, "optim.output")$value   #??
plot(vst, vgmf.sep) 
plot(vst, vgmf.sep, map=FALSE)

### (3) Sum-metric models
(v.sp <- vst[vst$timelag==0,c("spacelag","gamma")])
(v.t <- vst[vst$spacelag==0,c("timelag","gamma")])
#### To estimate the parameters of the joint variogram
p1 <- plot(vst) 
p2 <- plot(vst, map=F) 
plot(p1, split=c(1,1,2,1), more=T) 
plot(p2, split=c(2,1,2,1), more=F)

vgm.sum.metric <- vgmST(stModel="sumMetric", 
                        space=vgm(0.9*max(v.sp$gamma, na.rm=TRUE), 
                                  "Exp", 600/3,0), 
                        time=vgm(0.9*max(v.t$gamma, na.rm=TRUE), 
                                 "Exp", 8/3, 0), 
                        joint=vgm(30, "Exp",200/3, 0), 
                        stAni=600/8)

#### Lower bound of marginal sills
(sp.sill.lb <- 0.7 * max(v.sp$gamma, na.rm=TRUE))
(sp.range.lb <- v.sp[which(v.sp > sp.sill.lb)[1]-1, "spacelag"])
(t.sill.lb <- 0.7 * max(v.t$gamma, na.rm=TRUE))
(t.range.lb <- v.t[which(v.t$gamma > t.sill.lb)[1]-1, "timelag"])
(sp.sill.ub <- max(v.sp$gamma, na.rm=TRUE))
(sp.range.ub <- max(v.sp$spacelag))
(t.sill.ub <- max(v.t$gamma, na.rm=TRUE))
(t.range.ub <- max(v.t$timelag))

system.time(vgmf.sum.metric <- fit.StVariogram(vst, vgm.sum.metric, 
                                               method="L-BFGS-B", 
                                               control=list(maxit=500), 
                                               lower=c(sp.sill.lb, sp.range.lb/3,0, 
                                                       t.sill.lb, t.range.lb/3,0,0,1,0,1)))

upper=c(sp.sill.ub, sp.range.ub/3,Inf, 
        t.sill.ub, t.range.ub/3,Inf,Inf,Inf,Inf,Inf)

attr(vgmf.sum.metric, "optim.output")$par
attr(vgmf.sum.metric, "optim.output")$value
plot(vst, vgmf.sum.metric) 
plot(vst, vgmf.sum.metric, map=FALSE)

## Compare the goodness-of-???t of the three models 
attr(vgmf.metric, "optim.output")$value
attr(vgmf.sep, "optim.output")$value
attr(vgmf.sum.metric, "optim.output")$value

load("~/UStemperature/results/US.grid.RData")
colnames(US.grid@coords) = c("lon","lat")
US.grid <- STF(sp=as(US.grid,"SpatialPoints"), time=sp.1to10.time@time)
proj4string(US.grid)=proj4string(sp.1to10.time)

k.de.met <- krigeST(temp~polym(lon, lat, degree=2, raw=TRUE), 
                    data=sp.1to10.time, newdata=US.grid, modelList=vgmf.metric) 
gridded(k.de.met@sp) <- TRUE
k.de.sep <- krigeST(temp~polym(lon, lat, degree=2, raw=TRUE),
                    data=sp.1to10.time, newdata=US.grid, modelList=vgmf.sep) 
gridded(k.de.sep@sp) <- TRUE
k.de.sum.metric <- krigeST(temp~polym(lon, lat, degree=2, raw=TRUE),
                           data=sp.1to10.time, newdata=US.grid, modelList=vgmf.sum.metric) 
gridded(k.de.sum.metric@sp) <- TRUE

#plot.zlim = seq(floor(min(k.de.met$var1.pred)),ceiling(max(k.de.met$var1.pred)),by=0.5)
plot.zlim <- seq(floor(min(k.de.met$var1.pred, 
                           k.de.sep$var1.pred, 
                           k.de.sum.metric$var1.pred)), 
                 ceiling(max(k.de.met$var1.pred, 
                             k.de.sep$var1.pred, 
                             k.de.sum.metric$var1.pred)), by = 0.5)


stplot(k.de.met, main="Residual of the temperature in USA", sub="Metric model",
       col.regions=bpy.colors(length(plot.zlim)), at=plot.zlim) 
stplot(k.de.sep, main="Residual of the temperature in USA", sub="Separable space-time model", 
       col.regions=bpy.colors(length(plot.zlim)), at=plot.zlim) 
stplot(k.de.sum.metric, main="Residual of the temperature in USA", sub="Sum-metric space-time model", 
       col.regions=bpy.colors(length(plot.zlim)), at=plot.zlim)


