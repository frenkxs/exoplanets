# generating custom theme used for all plots in the project
library(ggthemes)
library(ggplot2)

theme_pv <- function(base_size = 12,
                      base_family = 'mono',
                      base_rect_size = base_size / 170){
  
  
  theme(
    text = element_text(family = base_family),
    plot.title = element_text(
      color = 'black',
      face = "bold",
      hjust = 0,
      size = 17),
    axis.title = element_text(
      colour = rgb(105, 105, 105, maxColorValue = 255),
      size = rel(1.1), margin = margin(t = 20, r = 20)),
    
    axis.title.x = element_text(margin = margin(t = 10, b = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.caption = element_text(hjust = 1, 
                                colour = rgb(150, 150, 150, maxColorValue = 255)),
    
    axis.text = element_text(
      color = rgb(105, 105, 105, maxColorValue = 255), size = rel(1)),
    axis.ticks = element_line(colour = rgb(105, 105, 105, maxColorValue = 255)),
    axis.ticks.length.y = unit(.25, "cm"),
    axis.ticks.length.x = unit(.25, "cm"),
    
    
    panel.grid.major.y = element_line(
      rgb(105, 105, 105, maxColorValue = 255),
      linetype = "dotted",
      size = rel(1.3)),
    panel.ontop = FALSE,
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.background = element_blank(),
    panel.background = element_rect(fill = rgb(245, 245, 245, maxColorValue = 255)),
    
    legend.key = element_rect(fill = "white", colour = NA),
    legend.title = element_text(size = rel(1.2)),
    aspect.ratio = 0.7

  )
}
