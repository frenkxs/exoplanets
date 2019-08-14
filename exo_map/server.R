#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

# Define server logic required to draw a histogram
function(input, output) {

    output$main <- renderPlot({
        
        
        # filter data based on user input
        data <- exo %>% 
            select(year, mass, dist, type) %>%
            {if('all' %in% input$type) {
                filter(., year <= input$year)}
                else{
                    (filter(., year <= input$year, type %in% input$type))}
            }
        
        # Create an empty data frame to display when none of the exoplanet type is selected. (This will display an empty plot, rather than an error message)
        if(length(input$type) == 0) {
            data <- tibble(year   = integer(1),
                               mass = numeric(1),
                               dist = numeric(1),
                               type = character(1))
        } else {
            data <- data
        }
        
        # Draw the scatterplot with the specified user input
        ggplot(data = data, aes(mass %>% log, dist %>% log)) +
            geom_point(aes(colour = type), size = 3) +
            
            # Add custom theme
            theme_pv() +
            theme(aspect.ratio = 0.45) +
        
            # Add the text and description
            labs(title = 'Mass of the planet versus its distance from the Sun',
                 subtitle = '', 
                 caption = 'Data: Open Exoplanet Catalogue\nPlot: @frenkxs',
                 y = 'Distance from the Sun in parsecs\n(log scale)',
                 x = 'Mass in multiples of Jupiter masses (log scale)') +
            
            # Add custom tick labels to y and x axes (with values converted form log)
            scale_x_continuous(breaks = c(log(1), log(10), log(100), log(1000)),
                               label = c('1', '10','100','1000'),
                               limits = c(log(0.0001), log(500))) +
            
            scale_y_continuous(breaks = c(log(1), log(10), log(100), log(1000)),
                               label = c('1', '10','100','1000'),
                               limits = c(log(1), log(10000))) +
            
            # Add custom colour scale (see theme_pv.R for details)
            scale_colour_pv(name = 'Exoplanet type') +
            
            # override the alpha level for the legend key set in geom_point
            guides(colour = guide_legend(override.aes = list(alpha = 1, size = 4))) +
            
            theme(legend.title = element_text(size = rel(1.7)),
                  legend.text = element_text(size = rel(1.2)),
                  plot.title = element_text(size = rel(2.3)))

    })

}
