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
        
        data <- exo %>% 
            select(year, mass, dist, type, meth) %>%
            filter(year <= input$year, type %in% input$type)
            
                
        
        # draw the histogram with the specified number of bins
        ggplot(data = data, aes(mass %>% log, dist %>% log)) +
            geom_point(aes(colour = meth), alpha = 0.4) +
            ggtitle('Mass of the planet versus\nits distance from the Sun') +
            theme_pv() +
            xlab('Mass (log scale)') +
            ylab('Distance from the sun (log scale)') +
            scale_color_viridis_d(option = "B", name = "Method of\ndiscovery") +
            
            # override the alpha level for the legend key set in geom_point
            guides(colour = guide_legend(override.aes = list(alpha = 1)))
        

    })

}
