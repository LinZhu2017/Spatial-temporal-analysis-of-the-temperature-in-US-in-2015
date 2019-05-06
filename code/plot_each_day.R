## Get the data in each month of 2015
Jan = filter(weather_data2, month==1)
Jan = mutate(Jan, month.id = "Jan")
Feb = filter(weather_data2, month==2)
Feb = mutate(Feb, month.id = "Feb")
Mar = filter(weather_data2, month==3)
Mar = mutate(Mar, month.id = "Mar")
Apri = filter(weather_data2, month==4)
Apri = mutate(Apri, month.id = "Apri")
May = filter(weather_data2, month==5)
May = mutate(May, month.id = "May")
Jun = filter(weather_data2, month==6)
Jun = mutate(Jun, month.id = "Jun")
July = filter(weather_data2, month==7)
July = mutate(July, month.id = "July")
Aug = filter(weather_data2, month==8)
Aug = mutate(Aug, month.id = "Aug")
Sep = filter(weather_data2, month==9)
Sep = mutate(Sep, month.id = "Sep")
Oct = filter(weather_data2, month==10)
Oct = mutate(Oct, month.id = "Oct")
Nov = filter(weather_data2, month==11)
Nov = mutate(Nov, month.id = "Nov")
Dec = filter(weather_data2, month==12)
Dec = mutate(Dec, month.id = "Dec")

library(graphics)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white'))
month = Jan
## !! change name of different plots when saving


## (1) Histogram
h1 = hist(month$temp, breaks=30,
          main=paste("Histogram of US Temp in", month.id), 
          xlab="Temperature (Fahrenheit)")

## (2) Discrete points' temperature distribution image
day = filter(month, day==1)
day_ = day
day_$hover <- with(day_, paste("<br>", "Lon:", lon,  
                               "<br>","Lat:", lat, 
                               "<br>","Elev:", elev,
                               "<br>", "Temp:", temp))

p1 <- plot_geo(day_, lat = ~lat, lon = ~lon) %>%
  add_markers(
    text = ~hover, showlegend=FALSE,
    marker=list(color = ~temp, showscale=FALSE),
    hoverinfo = "text") %>%
  layout(title = paste('US temp (Fahrenheit) on the 1st day of', month.id),
         geo = g, showlegend=T)
# library(plotly)
# ggplotly(p1)

# library(graphics)
## (3) Mean vs. Latitude:
lat_mean=month %>%
  group_by(lat)%>%
  summarise(temp_lat = mean(temp, na.rm=TRUE)) 

f1 = ggplot(data = lat_mean) + 
  geom_point(mapping = aes(x = lat, y = temp_lat))+
  geom_smooth(mapping = aes(x = lat, y = temp_lat)) +
  labs(x = "lat",y = "temp", title = paste('Mean temp by Lat in', month.id))

## (3) Variance vs. Latitude:
lat_var=month %>%
  group_by(lat)%>%
  summarise(temp_var_lat = var(temp, na.rm=TRUE))
f2 = ggplot(data = lat_var) + 
  geom_point(mapping = aes(x = lat, y = temp_var_lat))+
  geom_smooth(mapping = aes(x = lat, y = temp_var_lat))+
  labs(x = "lat",y = "variance of temp", title = paste('Variance in temp by Lat in', month.id))

## (4) Mean vs. Longitude:
lon_mean=month %>%
  group_by(lon)%>%
  summarise(temp_lon = mean(temp, na.rm=TRUE)) 
f3 = ggplot(data = lon_mean) + 
  geom_point(mapping = aes(x = lon, y = temp_lon))+
  geom_smooth(mapping = aes(x = lon, y = temp_lon))+
  labs(x = "lon",y = "temp", title = paste('Mean temp by Lon in', month.id))

## (4) Variance vs. Longitude:
lon_var=month %>%
  group_by(lon)%>%
  summarise(temp_var_lon = var(temp, na.rm=TRUE))
f4 = ggplot(data = lon_var) + 
  geom_point(mapping = aes(x = lon, y = temp_var_lon))+
  geom_smooth(mapping = aes(x = lon, y = temp_var_lon))+
  labs(x = "lon",y = "variance of temp", title = 'Variance in temp by Lon in', month.id)

