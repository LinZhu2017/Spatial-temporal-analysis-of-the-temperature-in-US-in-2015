## server.R

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

shinyServer(function(input, output) {
  
  output$image1 <- renderPlot({
    
    # generate bins based on input$bins from ui.R

    data = filter(weather_data2, month==input$month)
    hist(data$temp, breaks=30,
         main=paste("Histogram of US Temp in", name[input$month]), 
         xlab="Temperature (Fahrenheit)")
  })
  
  output$image2 <- renderPlotly({
    
    # generate bins based on input$bins from ui.R
    
    data = filter(weather_data2, month==input$month)
    day = filter(data, day==1)
    day$hover <- with(day, paste("<br>", "Lon:", lon,  
                                   "<br>","Lat:", lat, 
                                   "<br>","Elev:", elev,
                                   "<br>", "Temp:", temp))
    
    p1 <- plot_geo(day, lat = ~lat, lon = ~lon) %>%
      add_markers(
        text = ~hover, showlegend=FALSE,
        marker=list(color = ~temp, showscale=FALSE),
        hoverinfo = "text") %>%
      layout(title = paste('US temp (Fahrenheit) on the 1st day of', name[input$month]),
             geo = g, showlegend=T)
    ggplotly(p1)
  })
  
  output$image3 <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    
    data = filter(weather_data2, month==input$month)
    lat_mean = data %>%
      group_by(lat)%>%
      summarise(temp_lat = mean(temp, na.rm=TRUE))
    
    f1 = ggplot(data = lat_mean) +
      geom_point(mapping = aes(x = lat, y = temp_lat))+
      geom_smooth(mapping = aes(x = lat, y = temp_lat)) +
      labs(x = "lat",y = "temp", title = paste('Mean temp by Lat in', name[input$month]))
    
    lat_var=data %>%
      group_by(lat)%>%
      summarise(temp_var_lat = var(temp, na.rm=TRUE))
    f2 = ggplot(data = lat_var) +
      geom_point(mapping = aes(x = lat, y = temp_var_lat))+
      geom_smooth(mapping = aes(x = lat, y = temp_var_lat))+
      labs(x = "lat",y = "variance of temp", title = paste('Variance in temp by Lat in', name[input$month]))
    
    grid.arrange(f1, f2, ncol=2)
  })
  
  output$image4 <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    
    data = filter(weather_data2, month==input$month)
    lon_mean=data %>%
      group_by(lon)%>%
      summarise(temp_lon = mean(temp, na.rm=TRUE))
    f3 = ggplot(data = lon_mean) +
      geom_point(mapping = aes(x = lon, y = temp_lon))+
      geom_smooth(mapping = aes(x = lon, y = temp_lon))+
      labs(x = "lon",y = "temp", title = paste('Mean temp by Lon in', name[input$month]))
    lon_var=data %>%
      group_by(lon)%>%
      summarise(temp_var_lon = var(temp, na.rm=TRUE))
    f4 = ggplot(data = lon_var) +
      geom_point(mapping = aes(x = lon, y = temp_var_lon))+
      geom_smooth(mapping = aes(x = lon, y = temp_var_lon))+
      labs(x = "lon",y = "variance of temp", title = 'Variance in temp by Lon in', name[input$month])
    
    grid.arrange(f3, f4, ncol=2)
  })
  
})
