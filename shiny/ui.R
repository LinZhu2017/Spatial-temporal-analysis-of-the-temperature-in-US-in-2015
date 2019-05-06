load("weather_data2.RData")
library(dplyr)
library(graphics)
library(plotly)
library(shiny)
library(ggplot2)
library(graphics)
library(gridExtra)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white'))

name = c("Jan","Feb","Mar","Apri","May","Jun","July","Aug","Sep","Oct","Nov","Dec")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Analysis of monthly data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("month",
                  label = "Choose the month to display:",
                  min = 1,
                  max = 12, step = 1, value = 2)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("image1"),
      plotlyOutput("image2"),
      plotOutput("image3"),
      plotOutput("image4")
      
    )
  )
))
