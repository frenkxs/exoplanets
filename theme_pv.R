# generating custom theme used for all plots in the project
library(ggthemes)
library(ggplot2)

theme_new <- function(base_size = 11,
                      base_family = 'mono',
                      base_rect_size = base_size / 170){
  
  
  theme(
    text = element_text(family = base_family),
    plot.title = element_text(
      color = 'black',
      face = "bold",
      hjust = 0,
      size = 16),
    axis.title = element_text(
      color = rgb(105, 105, 105, maxColorValue = 255),
      size = rel(0.75)),
    axis.text = element_text(
      color = rgb(105, 105, 105, maxColorValue = 255),
      size = rel(0.5)),
    panel.grid.major.y = element_line(
      rgb(0, 0, 0, maxColorValue = 255),
      linetype = "dotted",
      size = rel(1)),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    axis.ticks.length.y = unit(.25, "cm"),
    axis.ticks.length.x = unit(.25, "cm"),
    legend.key = element_rect(fill = "white", colour = NA)
  )
}
