
library(shiny)

# Define UI for application that draws 
fluidPage(tags$head(tags$style(
    HTML('

        body, label, input, button, select { 
          font-family: "Courier New";
        }'))),

    # Application title
    titlePanel("Exploring exoplanets"),

    # Sidebar for user input 
    sidebarLayout(
        sidebarPanel(id="sidebar",
            
            # Select the period covered on a slider
            sliderInput('year', 'Year:',
                        min = 2009,
                        max = 2019,
                        value = 2019),
            
            # Select exoplanet type as checkboxes
            checkboxGroupInput('type', 'Select exoplanet type:',
                               c('Hot Jupiter' = 'hot_jupiters',
                                 'Cold Gas Giants' = 'cold_gas_giants',
                                 'Rocky' = 'rocky',
                                 'Others' = 'others', 
                                 'All' = 'all'),
                               selected = c('all') )),
        
    # Show the plot 
    mainPanel(plotOutput("main"))
    )
)

