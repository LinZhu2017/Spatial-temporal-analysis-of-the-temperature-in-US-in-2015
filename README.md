# Spatial-temporal-analysis-of-the-temperature-in-US-in-2015

## Introduction

The film “the wandering earth”, which has attracted a lot of attention recently, sets up an extremely harsh climate which is close to 
collapse after a great change of climate. Such a catactrophic scenario is not just a figment of the bad weather that is common 
in many places. Climate change and potential global warming have become the significant issue of the day, monitoring temperatures 
across the region, therefore, is becoming increasingly important in response to weather-related disasters.

## Data description and availability

In this project, we focus on analyzing the temperature in the United States in 2015. The raw dataset US_weather_2015.csv 
and weather_stations.csv can be available via (https://www.kaggle.com/markhamilton1/get-us-weather-data/output). We rename 
these two files as weather_data and weather_stations correspondingly which contain temperature of the United States at 
different stations and their locations (marked by longitude, latitude and elevation) from January 1 to December 31, 2015, 
a period of 365 days.



## Report/code folder
 
- **spatial analysis.R** is used to spatial analysis
- **choose_model.R** is used to choose a feasible spatial model for the temperature.
- **Space-temporal.R** is used to construct spatial-temporal analysis
- **plot_each_day.R** is used to generate the object in shiny

## Reproducibility

To save running time, we put the generated important results in the **to load** folder. If necessary, you can load these files 
according to the steps in Spatial-temporal analysis of the temperature in US in 2015.Rmd (in the **Report** folder). 

### Research objectives

In geostatistics, we generally consider the temperature is normally distributed with mean and covariance structure being functions 
of latitude, longitude and time. Furthermore, the mean function may be the linear combination or other nonlinear forms of 
three factors. Apart from this, the covariance may introduce the Matern class, which is popular in spatial statistics. 
Considering these cases, we will figure out the structure of the normal distribution by the following steps.

- 1. Construct spatial data analysis regardless of the influence of time and find the most appropriate model.
- 2. Construct Spatial-Temporal analysis.
- 3. Various forms of Kriging can be used to attempt to fill gaps so that we can obtain the complete temperature variation 
diagram across the United States. 
