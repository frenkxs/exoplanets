#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Exoplanets"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput('year', 'Year:',
                        min = 2009,
                        max = 2019,
                        value = 2019),
            
            checkboxGroupInput('type', 'Select exoplanet type',
                               c('Hot Jupiter' = 'hot_jupiters',
                                 'Cold Gas Giant' = 'cold_gas_giants',
                                 'Rocky' = 'rocky',
                                 'Other' = 'others'),
                                 
                               selected = levels(exo$type) )),
            
        
    # Show a plot of the generated distribution
    mainPanel(plotOutput("main"))
    )
)

