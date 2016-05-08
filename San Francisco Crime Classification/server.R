##
## Kaggle Competition - San Francisco Crime Classification
## Visualizes location of crime based on year and type
##

library(shiny)

# Define server logic required for the app
shinyServer(function(input, output) {
    #Loads libraries
    library(ggmap)
    library(ggplot2)
    library(dplyr)
    library(lubridate)
    
    #Initializes data
    data = read.csv("Input/train.csv")

    #Render checkbox group based on data from choices
    output$crimeSelector <- renderUI({
        checkboxGroupInput("checkGroup", 
                            h3("Crime Categories"), 
                            levels(data$Category))
    })
    
    #Convert dates into year value
    data$Dates = year(data$Dates)
    
    #Render slider based on data from years
    output$yearSlider <- renderUI({
        sliderInput("range", 
                    label = "Year of interest:",
                    min = min(data$Dates), max = max(data$Dates), value = min(data$Dates))
    })
    
    #Setup maps of San Francisco
    map = get_map(location="sanfrancisco",zoom=12,source="osm")
        
    #Plots based on year and crime
    output$locPlot = renderPlot({
        map_crime = function(crime_df, crime, year) {
            filtered = filter(crime_df, Category %in% crime)
            filtered = filter(filtered, Dates %in% year)
            plot = ggmap(map, extent='device') + 
                geom_point(data = filtered, aes(x = X, y = Y, color=Category))
            return(plot)
        }
        
        #Plots based on crimes selected in checkGroup
        if(length(input$checkGroup) != 0)
            map_crime(data, c(input$checkGroup), input$range)
        else{
            ggmap(map, extent='device')
        }
    })
})