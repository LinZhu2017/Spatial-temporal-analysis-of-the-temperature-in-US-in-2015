#install.packages("readr")
library(readr)
weather_data = read.csv("/home/zhulin2017/UStemperature/results/US_weather_2015.csv")
weather_stations = read.csv("/home/zhulin2017/UStemperature/results/weather_stations.csv")
save(weather_data,file="~/UStemperature/results/weather_data.RData")
save(weather_stations,file="~/UStemperature/results/weather_stations.RData")

library(ggplot2)
statesMap <- map_data("state")

library(gstat)
library(sp)
library(dplyr)


## Extract the states in the main US territory
states = unique(statesMap$region)
weather_stations_sub = filter(weather_stations, lat < 50 & lat > 25 & lon > (-125) & lon < (-65))

## Merge weather and location
weather_data2 = filter(weather_data, stn %in% weather_stations_sub$stn)
matchPos = match(weather_data2$stn, weather_stations_sub$stn)
weather_data2 = cbind(weather_data2, weather_stations_sub[matchPos, c("lon", "lat","elev")])

weather_data2 = na.omit(weather_data2)

## Remove Duplicate Rows based on all variables
weather_data2 = distinct(weather_data2)
weather_data2 = mutate(weather_data2, num = as.numeric(ISOdate(2015,month,day)-ISOdate(2015,1,1),
                                                       units="days")+1)
weather_data2 = mutate(weather_data2, date = as.Date(paste(year,month,day,sep = "-")))

## Sort data by date, lon and lat
weather_data2 = arrange(weather_data2, num, lon, lat)
weather_data2 = mutate(weather_data2, lon_rank = min_rank(lon), lat_rank = min_rank(lat))
save(weather_data2,file="~/UStemperature/results/weather_data2.RData")

## Get the data on Jan 1st, 2015
day1 = filter(weather_data2, num==1) 

## Plot US temperature by Weather Station on Jan 1st, 2015
day1_= day1
day1_$hover <- with(day1_, paste("<br>", "Lon:", lon,  
                                 "<br>","Lat:", lat, 
                                 "<br>","Elev:", elev,
                                 "<br>", "Temp:", temp))
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white'))

p1 <- plot_geo(day1_, lat = ~lat, lon = ~lon) %>%
  add_markers(
    text = ~hover, showlegend=FALSE,
    marker=list(color = ~temp, showscale=FALSE),
    hoverinfo = "text") %>%
  layout(title = 'US temp (Fahrenheit) on Jan 1st, 2015',
         geo = g, showlegend=T)
library(plotly)
ggplotly(p1)


## Mean vs. Month:
month_mean=weather_data2 %>%
  group_by(month)%>%
  summarise(temp_month = mean(temp, na.rm=TRUE))


## Histogram of Average monthly temperature 
fit <- fitted(loess(month_mean$temp_month ~ month_mean$month))
p_hist <- plot_ly(data = month_mean, x = ~month, y = ~temp_month, type = "bar", showlegend=FALSE,
                  marker=list(color=~month, showscale=FALSE)) %>%
  add_lines(y = fit, showlegend=FALSE, color = 'black') %>%
  layout(title = 'Average monthly temperature in the US in 2015',
         showlegend=FALSE, xaxis = list(side="right", showgrid=FALSE),
         yaxis=list(title = "temp (Fahrenheit)",showgrid=FALSE))
p_hist

## Boxplot of US monthly temperature
p_box <- ggplot(weather_data2, aes(month, temp, group = month, color = month, fill = month)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_gradientn(colours = terrain.colors((12))) + 
  theme(legend.position='none') +
  ggtitle("Boxplot of US monthly temperature")

# Need to modify the plotly object and make outlier points have opacity equal to 0
p_box <- plotly_build(p_box)

p_box$data <- lapply(p_box$data, FUN = function(x){
  x$marker = list(opacity = 0)
  return(x)
})

p_box


library(graphics)
## Mean vs. Latitude:
lat_mean=day1 %>%
  group_by(lat)%>%
  summarise(temp_lat = mean(temp, na.rm=TRUE)) 

f1 = ggplot(data = lat_mean) + 
  geom_point(mapping = aes(x = lat, y = temp_lat))+
  geom_smooth(mapping = aes(x = lat, y = temp_lat)) +
  labs(x = "lat",y = "temp", title = 'Mean temp by Latitude')


## Variance vs. Latitude:
lat_var=day1 %>%
  group_by(lat)%>%
  summarise(temp_var_lat = var(temp, na.rm=TRUE))
f2 = ggplot(data = lat_var) + 
  geom_point(mapping = aes(x = lat, y = temp_var_lat))+
  geom_smooth(mapping = aes(x = lat, y = temp_var_lat))+
  labs(x = "lat",y = "variance of temp", title = 'Variance in temp by Latitude')

require(gridExtra)
grid.arrange(f1, f2, ncol=2)

## Mean vs. Longitude:
lon_mean=day1 %>%
  group_by(lon)%>%
  summarise(temp_lon = mean(temp, na.rm=TRUE)) 
f3 = ggplot(data = lon_mean) + 
  geom_point(mapping = aes(x = lon, y = temp_lon))+
  geom_smooth(mapping = aes(x = lon, y = temp_lon))+
  labs(x = "lon",y = "temp", title = 'Mean temp by Longitude')

## Variance vs. Longitude:
lon_var=day1 %>%
  group_by(lon)%>%
  summarise(temp_var_lon = var(temp, na.rm=TRUE))
f4 = ggplot(data = lon_var) + 
  geom_point(mapping = aes(x = lon, y = temp_var_lon))+
  geom_smooth(mapping = aes(x = lon, y = temp_var_lon))+
  labs(x = "lon",y = "variance of temp", title = 'Variance in temp by Longitude')

#require(gridExtra)
grid.arrange(f3, f4, ncol=2)

## Mean vs. Elevation:
elev_mean=day1 %>%
  group_by(elev)%>%
  summarise(temp_elev = mean(temp, na.rm=TRUE)) 
f5 = ggplot(data = elev_mean) + 
  geom_point(mapping = aes(x = elev, y = temp_elev))+
  geom_smooth(mapping = aes(x = elev, y = temp_elev))+
  labs(x = "elev",y = "temp", title = 'Mean temp by Elevation')

## Variance vs. Elevation:
elev_var=day1 %>%
  group_by(elev)%>%
  summarise(temp_var_elev = var(temp, na.rm=TRUE))
f6 = ggplot(data = elev_var) + 
  geom_point(mapping = aes(x = elev, y = temp_var_elev))+
  geom_smooth(mapping = aes(x = elev, y = temp_var_elev))+
  labs(x = "elev",y = "variance of temp", title = 'Variance in temp by Elevation')

#require(gridExtra)
grid.arrange(f5, f6, ncol=2)



## Get training data and test data
day1 = distinct(day1,lon,lat,.keep_all = T)
X_new = day1[,6:7]
Y_new = day1[5]
Z_new = day1[,8]
save(Y_new,file="~/UStemperature/results/Y_new.RData")
save(X_new,file="~/UStemperature/results/X_new.RData")
save(Z_new,file="~/UStemperature/results/Z_new.RData")
save(day1,file="~/UStemperature/results/day1.RData")

##-------------------------------------------##
## Finally, we use Matern model to fit model.##
##-------------------------------------------##
train_X = X_new
train_y = Y_new
train_Z = Z_new

## parameter estimation
#(fields use great circle distances, approxiamte the earth by a ball)
library(fields)
par_est_whole = spatialProcess(train_X,train_y,Z=train_Z,
                               mKrig.args = list(m = 3),
                               Distance = "rdist.earth",
                               cov.args = list(Covariance = "Matern",smoothness = 1))
# where m=3 means quadratic form
print(par_est_whole)
sigma.square = as.numeric(par_est_whole$sigma.MLE)^2     # nugget variance (sigma^2)  4.225
rho = as.numeric(par_est_whole$rho.MLE)                  # process variance (rho)   60.12
theta = as.numeric(par_est_whole$theta.MLE)              # range parameter (theta)  150.646(miles)
save(par_est_whole,file="~/UStemperature/results/par_est_whole.RData")

## The maximum distance among all pairs 
out=rdist.earth(cbind(day1$lon,day1$lat),miles = T)
range(out)[2]
# [1] 3167.165(miles)

# Note the likelihood can be maximized analytically 
# over the parameters of the ???xed part of the spatial model 
# and with the nugget (sigma) and sill (rho) reduced to 
# the single parameter lambda= sigma^2/rho. 
# The likelihood is maximized numerically over lambda and theta 
# if there are additional covariance parameters 
# (such as smoothness for the Matern) these need to be ???xed and 
# so the MLE is found for the covariance conditional on these additional parameter values. 

plot(par_est_whole)  
# plot 1 data vs. predicted values
# plot 2 residuals vs. predicted

par_est_whole$ind.drift
# TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
# ind.drift can distinguish between polynomial part 
# and the extra covariates coefficients associated with Z.

par_est_whole$eff.df
# [1] 743.922

par_est_whole$lnProfileLike.FULL
# [1] -5752.888

par_est_whole$GCV
# [1] 6.229168
# Estimated value of the GCV function.

## use empirical variogram to test the fitted model
coordinates(day1) = ~lon+lat
v = variogram(temp~1+elev+polym(lon, lat, degree=2, raw=TRUE), day1)



## Create testing data by meshgrid  resolution of (60/50)бу x (25/50)бу of longitude and latitude
lon_num = 50
lat_num = 50
lon_seq = seq(-125,-65,length.out=lon_num)
lat_seq = seq(25,50,length.out=lat_num)

## Restrict the grid to the USA mainland
require(mapdata) 
tmp <- map('worldHires', 'Usa', fill=TRUE, plot=FALSE) 
require(maptools) 
US.boundary <- map2SpatialPolygons(tmp, IDs = tmp$names, proj4string = CRS(as.character(NA))) 
US.bbox.grid <- SpatialPoints(cbind(rep(lon_seq,length(lat_seq)), 
                                    rep(lat_seq,each=length(lon_seq)))) 
gridded(US.bbox.grid) <- TRUE
US.grid <- US.bbox.grid[!is.na(over(US.bbox.grid, US.boundary)),] 
save(US.grid,file="~/UStemperature/results/US.grid.RData")

Z = as.matrix(rep(mean(Z_new),length(US.grid@coords[,1])))
pre_whole = predict(par_est_whole, xnew = US.grid@coords,Z = Z) # Z is not necessary
save(pre_whole,file="~/UStemperature/results/pre_whole.RData")

#----------------------------------------#

## Plot of krigged temperature
df=as.data.frame(US.grid)
df$z <- pre_whole
colnames(df)=c("lon","lat","temp")

library(maps)
statesMap <- map_data("state")

library(viridis)
p.krig = ggplot(df, aes(lon, lat,fill = temp)) + 
  geom_raster(interpolate = F) + 
  scale_fill_continuous(type="viridis",alpha=0.05) +
  geom_polygon(data = statesMap, aes(x=long, y = lat, group = group), color = "black",fill="grey", alpha=0) +
  labs(x = "lon",y = "lat", title = 'Krigged US temp (Fahrenheit) on Jan 1st, 2015') + 
  theme(plot.title = element_text(hjust = 0.4))

save(p.krig,file="~/UStemperature/results/p.krig.RData")

## spatial pattern of the residuals after fitting
library(plotly)
testdata = cbind(X_new,Z_new,par_est_whole$residuals)
colnames(testdata)=c("lon","lat","elev", "temp")
testdata$hover <- with(testdata, paste("<br>", "Lon:", lon,  
                                       "<br>","Lat:", lat, 
                                       "<br>","Elev:", elev,
                                       "<br>", "ResTemp:", temp))
plot_ly(testdata, x = ~lon, y = ~lat) %>%
  add_markers(
    text = ~hover, showlegend=FALSE,
    marker=list(color = ~temp, 
                size = ~2*abs(temp), opacity = 0.5, 
                type = 'scatter', mode = 'markers', showscale=FALSE),
    hoverinfo = "text") %>%
  layout(title = 'Residual temperature from linear model with Matern class',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))


