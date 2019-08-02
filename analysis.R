# load libraries
library(readr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(viridis)
library(ggthemes)
library(ggiraph)


# 1) Import the dataset exo_data.csv as a tibble. Columns 1, 16, 17, 18, 25 should # # be characters. Columns 2, 14 should be factors. Column 15 should be integers. The # remaining columns should be doubles.


# read in data, spccifying the column types
exo <- read_csv('exo_data.csv', col_types = 'cfdddddddddddficccddddddc' )

# additionally we can parse the recency and right ascension to date and time respectively
exo$recency <- parse_date(exo$recency, '%y/%m/%d', na = 'NA')
exo$r_asc <- parse_time(exo$r_asc, '%H %M %OS', na = 'NA')

# 2) Exclude the exoplanets with an unknown method of discovery.

exo <- exo %>% subset(not( meth %>% is.na() ) )


# 3) Create a histogram for the log-distances from the Sun, highlighting the methods of discovery.
ggplot(data = exo, aes(dist %>% log, color = meth, fill = meth)) +
  geom_histogram(alpha = 0.5, aes(fill = meth)) +
  ggtitle('Number of exoplanets by their distance \nfrom the sun (log parsec)') +
  theme_new() +
  ylab('') +
  xlab('log-distance from the Sun') +
  scale_color_viridis_d('C', 'Method of\n discovery') +
  scale_fill_viridis_d('Method of\n discovery') 


# 4) Create scatterplots of the log-mass versus log-distances, separating by methods of discovery. Hovering with the cursor highlights the point and displays its name, and, if you click, the exoplanet's page on the Open Exoplanet Catalogue will be opened. (paste the id after http://www.openexoplanetcatalogue.com/planet/ ).
# 

p2 <- ggplot(data = exo, aes(mass %>% log, dist %>% log)) +
  geom_point(aes(color = meth),alpha = 0.5) +
  ggtitle('log mass vs log distance from the sun') +
  theme_new() +
  scale_color_viridis_d(option = "B") 

p3 <- p2 + geom_point_interactive(aes(tooltip = id, color = meth, data_id = id), 
                                  alpha = 0.5, 
                                  size = 2) + 
  scale_color_viridis_d(option = "B") 

ggiraph(code = print(p3), width = 0.65)

crimes$onclick = sprintf("window.open(\"%s%s\")",
                         "http://en.wikipedia.org/wiki/",
                         as.character(crimes$state))



