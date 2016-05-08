##
## Kaggle Competition - San Francisco Crime Classification
## Visualizes location of crime based on year and type
##

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Title
    titlePanel("San Francisco Crime Map"),
    
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
        sidebarPanel(
            helpText("Choose the categories of crimes to display on the map as well as the year"),
            checkboxGroupInput("checkGroup", 
                               label = h3("Crime Categories"), 
                               choices = list("Warrants" = 'WARRANTS', "Vandalism" = 'VANDALISM',
                                              "Prostitution" = 'PROSTITUTION', "Bribery" = 'BRIBERY')),
            sliderInput("range", 
                        label = "Year of interest:",
                        min = 2003, max = 2015, value = 2003)
        ),
        
        # Show a plot of the generated distribution of crimes on a map of SF
        mainPanel(
            plotOutput("locPlot")
        )
    )
))