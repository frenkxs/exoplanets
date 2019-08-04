# load libraries
library(readr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(viridis)
library(ggthemes)
library(ggiraph)
library(tidyr)



# 1) Import the dataset exo_data.csv as a tibble. Columns 1, 16, 17, 18, 25 should # # be characters. Columns 2, 14 should be factors. Column 15 should be integers. The # remaining columns should be doubles.


# read in data, spccifying the column types
exo <- read_csv('exo_data.csv', col_types = 'cfdddddddddddficccddddddc' )

# additionally we can parse the recency and right ascension to date and time respectively
exo$recency <- parse_date(exo$recency, '%y/%m/%d', na = 'NA')
exo$r_asc <- parse_time(exo$r_asc, '%H %M %OS', na = 'NA')

# 2) Exclude the exoplanets with an unknown method of discovery.

exo <- exo %>% subset(not( meth %>% is.na() ) )


# 3) Create a histogram for the log-distances from the Sun, highlighting the methods of discovery.

# Load custome theme (the file must be in the same directory as the main R code (analysis.R))
source('theme_pv.R')

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

# static plot
p2 <- ggplot(data = exo, aes(mass %>% log, dist %>% log)) +
  geom_point(aes(color = meth),alpha = 0.4) +
  ggtitle('Mass of the planet versus\nits distance from the Sun') +
  theme_pv() +
  xlab('Mass (log scale)') +
  ylab('Distance from the sun (log scale)') +
  scale_color_viridis_d(option = "B", name = "Method of\ndiscovery") +
  
  # override the alpha level for the legend key set in geom_point
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
  
# add another column to the dataset specifying the url to each planet
exo$onclick = sprintf("window.open(\"%s%s\")",
                      "http://www.openexoplanetcatalogue.com/planet/",
                      exo$id)


# add interactive layers - tooltip, colour and onclick
p3 <- p2 + geom_point_interactive(aes(tooltip = id, 
                                      color = meth, 
                                      data_id = id,
                                      onclick = onclick), 
                                  alpha = 0.2,
                                  size = 1.2) 
# print off the plot
ggiraph(code = print(p3), width = 0.75)


# 5) Rename the radius into jupiter_radius, and create a new column called earth_radius which is 11.2 times the Jupiter radius.

exo <- exo %>% 
  # add new column earth_radius
  mutate(earth_radius =  radius * 11.2) %>% 
  
  # rename radius to jupiter_radius
  rename(jupiter_radius = radius) %>%
  
  # reorder the columns so that earth_radius comes right after jupiter_radius
  select(id:jupiter_radius, earth_radius, period:onclick) 

  
# 6) Focus only on the rows where log-radius and log-period have no missing values, and perform kmeans with four clusters on these two columns.

# get a new dataset with just the two radii
radii <- exo %>% 
  select(id, earth_radius, period) %>%
  mutate(earth_radius = earth_radius %>% log, 
         period = period %>% log,
         type = NA)
  
# runn kmeans algorithm and store the results
fit <- kmeans(na.omit(radii[, 2:3]), 4)


# 7*) Add the clustering labels to the dataset through a new factor column called 'type', with levels 'rocky', 'hot_jupiters', 'cold_gas_giants', 'others'; similarly to https://en.wikipedia.org/wiki/Exoplanet#/media/File:ExoplanetPopulations-20170616.png

# Add the cluster labels to the radii dataset, rows with missing values in either earth_radius or period will have also have the type label NA
radii$type[which( complete.cases(radii[, 2:3]) )] <- fit$cluster

# Copy type column from radii over to exo
exo <- exo %>%
  mutate(type = factor(radii$type)) %>%
  mutate(type = factor(radii$type, levels = c('hot_jupiters',
                                              'cold_gas_giants',
                                              'others',
                                              'rocky')) )

df <- mtcars %>% mutate(cyl = factor(cyl, levels = c(4, 6, 8)))


ggplot(exo, aes(x = log(period), y = log(earth_radius))) +
  geom_point(aes(color = type))

# hot jupiters = 1
# rocky =  4
# cold_gas_giants = 2
# others = 3

