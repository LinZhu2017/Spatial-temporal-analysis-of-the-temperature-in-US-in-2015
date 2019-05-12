#===============================================================

# Spatial-temporal Analysis of the temperature

# [Lin Zhu]

# May, 2019

#===============================================================



#===============================================================

# Reproducibility: Reproducible_SA script

#===============================================================



# This file contains instructions for loading the data, all

# analyses in the report.



# Download the repository from  GitHub [https://github.com/LinZhu2017/Spatial-temporal-analysis-of-the-temperature-in-US-in-2015].

# The following script assumes the working directory has

# been set to this folder.



# As denoted below, raw data is too large to be uploaded. Therefore, the resulting

#  data files are available in the **to load** folder. Otherwise,

# codes represented in the final report.rmd file have been run, thus they are not reprodu

# -ced here.



#===============================================================

# Step 0: Install necessary packages;

#===============================================================

## This step is represented in the README.md file.

# Taking exploratory data analysis on the first day of the raw data and conducting preprocessing steps.

## It should be noted that the whole spatial analysis is based on the first day (i.e. day1.RData).




setwd("../to load")

load("day1.RData")

setwd("../report/code")

source("spatial analysis.R")



## Necessary packages

library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(maps)
library(mapdata) 
library(maptools)
library(graphics)
library(gstat)
library(sp)
library(viridis)
library(fields)
library(gridExtra)
library(xts) 
library(spacetime)



#===============================================================

# Step 1: Choosing parameter model.

#===============================================================



setwd("../report/code")

source("choose_model.R")

# In this file, we construct two model **par_est** and **par_est2** with exponential and Matern

# covariance matrix respectively. And then evaluate these in accordance with effective degree of freedom,

# log profile likelihood and respective mean prediction error. Finally, we use the Matern covariance model.



#===============================================================

# Step 2: Redefine train data and predict on the grid data

#===============================================================



setwd("../report/code")

source("spatial analysis.R")


# Similar to the procedure in choosing model, we further decide the form of the trend 

# with quadratic polynomial. And define the grid data on the mainland of the United States.

# At last, predict on the grid data and explore the spatial pattern of the residuals 

# after fitting via bubble plot.





#===============================================================

# Step 3: Construct spatial-temporal analysis

#===============================================================


setwd("../report/code")

source("Space-temporal.R")

# This file generates the STFDF object which is used to construct spatial-temporal analysis
# However, the separable covariance model in view of space and time indexes may not be the most 
# appropriate model. The further target is to figure out a more complex and feasible covariance model.


    
#===============================================================

# Step 4: Shiny

#===============================================================    
    
setwd("../report/code")

source("plot_each_day.R")

# Obtaining the monthly data 

# You should load weather_data2.RData which cannot be avaible. In this case, you can load 

# the raw data from https://www.kaggle.com/markhamilton1/get-us-weather-data/output to find

# US_weather_2015.csv and weather_stations.csv. 

weather_data = read.csv("US_weather_2015.csv")

weather_stations = read.csv("weather_stations.csv")

source("spatial analysis.R")

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

# Now, you can obtain weather_data2 for shiny

setwd("../shiny")

source("ui.R")

source("server.R")

  




