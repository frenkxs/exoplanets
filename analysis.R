###########################################################################
######### Advanced Data Programming with R - Final project ################
###########################################################################
####################### Premysl Velek, 16213669 ###########################
###########################################################################

# This is the main source code ('analysis.R'), the final project also include further three files to run. They should all be placed in the same directory as this file: 

# 1. theme_pv.R (with a custom ggpplot2 theme applied to all plots in the project) 
# 2. a subdirectory 'exo_map' with the server and ui files to run the shiny app
# 3. a stan file 'period-lm.stan' to fit the regression models to the data

# The code assumes that the directory with all the files is the Working directory in RStudio

# Clean the environment
rm(list=ls())


# load necessary libraries

# to read in the csv file 
library(readr) 
library(lubridate)

# to clean and manipulate the data
library(dplyr)
library(magrittr)
library(tidyr)

# to plot the data
library(ggplot2)

# to create interactive and animated plots
library(ggiraph)
library(gganimate)

# to run th Shiny app
library(shiny)



####################################################################################
# 1) Import the dataset exo_data.csv as a tibble. Columns 1, 16, 17, 18, 25 should be characters. Columns 2, 14 should be factors. Column 15 should be integers. The remaining columns should be doubles.
####################################################################################

# Read in data into a tibble, specifying the column types
exo <- read_csv('exo_data.csv', col_types = 'cfdddddddddddficccddddddc' )

# Additionally we can parse the recency and right ascension to date and time respectively
exo$recency <- parse_date(exo$recency, '%y/%m/%d', na = 'NA')
exo$r_asc <- parse_time(exo$r_asc, '%H %M %OS', na = 'NA')

####################################################################################
# 2) Exclude the exoplanets with an unknown method of discovery.
####################################################################################

exo <- exo %>%
  drop_na(meth) # using drop_na() from tidyr package

#######################################################################################
# 3) Create a histogram for the log-distances from the Sun, highlighting the methods of discovery.
#######################################################################################

# Reorder factors in meth so that the methods are represented by the same colour throughtout the project
exo$meth <- factor(exo$meth, 
                   levels = c('timing', 'imaging', 'microlensing', 'RV', 'transit'))



# Load custom ggplot2 theme from 'theme_pv.R'
source('theme_pv.R')

ggplot(data = exo, aes(dist %>% log, fill = meth)) +
  geom_histogram(alpha = 0.75, aes(fill = meth)) +
  
  # add custom theme
  theme_pv() +
  
  # add text and descriptions
  labs(title = 'Exploring exoplanets',
       subtitle = 'Number of exoplanets by their distance from the Sun',
       caption = 'Data: Open Exoplanet Catalogue\nPlot: @frenkxs',
       x = 'Distance from the Sun (parsec, log scale)',
       y = '') +
  
  # add custom colour scale and legend title
  scale_fill_brewer(type = 'qual', palette = 'Paired', 
                    name = "Method of\ndiscovery") +
  
  # add custom tick labels to x axis (with values converted form log-parsec to parsec)
  scale_x_continuous(breaks = c(log(1), log(10), log(100), 
                                log(1000), log(10000)),
                     label = c('1', '10', '100', '1000', '10000'))


#######################################################################################
# 4) Create scatterplots of the log-mass versus log-distances, separating by methods of discovery. Hovering with the cursor highlights the point and displays its name, and, if you click, the exoplanet's page on the Open Exoplanet Catalogue will be opened. (paste the id after http://www.openexoplanetcatalogue.com/planet/ ).
########################################################################################

# Add another column to the dataset specifying the url to each planet in the Open Exoplanet Catalogue
exo$onclick <- sprintf("window.open(\"%s%s\")",
                       "http://www.openexoplanetcatalogue.com/planet/",
                       exo$id)

# Start with a static plot, save it in a new container p2
p2 <- ggplot(data = exo, aes(mass %>% log, dist %>% log)) +
  geom_point(aes(colour = meth), alpha = 0.75) +
  
  # Add text and descriptions
  labs(title = 'Exploring exoplanets',
       subtitle = 'Mass of the planet versus its distance from the Sun',
       caption = 'Data: Open Exoplanet Catalogue\nPlot: @frenkxs',
       x = 'Mass in multiples of Jupiter mass (log scale)',
       y = 'Distance from the Sun in parsecs (log scale)') +
  
  # Add custom theme
  theme_pv() +
  
  # Add palette for categorical variables
  scale_color_brewer(type = 'qual', palette = 'Paired', 
                     name = "Method of\ndiscovery") +

  # add custom tick labels to x and y axes (with values converted back form log scale)
  scale_x_continuous(breaks = c(log(0.0001),log(0.01), log(1), log(100)),
                     label = c('0.0001', '0.01', '1', '100')) +
  
  # Override the alpha level for the legend key set in geom_point
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
  

# Add an interactive layers to the p2 plot - tooltip, colour and onclick
p3 <- p2 + geom_point_interactive(aes(tooltip = id, colour = meth,
                                      data_id = id, onclick = onclick)) 

# Print off the interactive plot
girafe(ggobj = p3) %>%
  
  # Allow zooming, colour the labels with planet id according to method of discovery
  girafe_options(opts_zoom(min = 0.5, max = 4), 
                 opts_tooltip(use_fill = TRUE, use_stroke = TRUE))

########################################################################################
# 5) Rename the radius into jupiter_radius, and create a new column called earth_radius which is 11.2 times the Jupiter radius.
########################################################################################

exo <- exo %>% 
  
  # Add new column earth_radius
  mutate(earth_radius =  radius * 11.2) %>% 
  
  # Rename radius to jupiter_radius
  rename(jupiter_radius = radius) %>%
  
  # Reorder the columns so that earth_radius comes right after jupiter_radius
  select(id:jupiter_radius, earth_radius, period:onclick) 

########################################################################################
# 6) Focus only on the rows where log-radius and log-period have no missing values, and perform kmeans with four clusters on these two columns.
########################################################################################

exo_type <- exo %>% 
  
  # Create a new dataset with just the three variables: id, earth_radius and period
  select(earth_radius, period) %>%
  
  # Log transform the variables; add new variable to store the results of the kmeans 
  mutate(earth_radius = earth_radius %>% log, 
         period = period %>% log,
         type = NA) 
  
#set seed so the clusters and their labels can be easily reproduced (this will help to keep the mapping of the clusters to the exoplanets type consistent for multiple runs of the code, see below)
set.seed(12)

# run kmeans algorithm and store the results in fit. (filter out all missing values)
fit <- exo_type[, 1:2] %>%
  drop_na() %>%
  kmeans(., centers = 4, iter.max = 120, nstart = 30)

########################################################################################
# 7*) Add the clustering labels to the dataset through a new factor column called 'type', with levels 'rocky', 'hot_jupiters', 'cold_gas_giants', 'others'; similarly to https://en.wikipedia.org/wiki/Exoplanet#/media/File:ExoplanetPopulations-20170616.png
########################################################################################

# Add the cluster labels to the exo_type dataset, while skipping rows with missing values in either earth_radius or period (they will have the type label NA) We select the complete rows by selecting the indices in exo_type$type that correspond to rows with no missing values.
exo_type$type[which( complete.cases(exo_type[, 1:2]) )] <- fit$cluster


# plot the clusters to help map the clusters to the exoplanet types. 

ggplot(exo, aes(log(period), log(earth_radius))) +
         geom_point(aes(colour = factor(exo_type$type)), alpha = 0.7) +
         geom_point(data = as.data.frame(fit$centers), aes(x = period, y = earth_radius),
                    colour = 'black', size = 2) +
  scale_y_continuous(breaks = c(log(0.001), log(0.1), log(1), log(10)),
                     label = c(0.001, '0.1', '1', '10')) +
  scale_x_continuous(breaks = c(log(0.001), log(0.1), log(1), log(10)),
                     label = c(0.001, '0.1', '1', '10'))




# I assume hot_jupiters will be planets with large radius and their orbit period will be shorter (ie. they are closer to their star). Cold gas giant will be big in radius and far from their star (ie long period). Rocky planets will be closer to Earth proportions in term of radius and period, and have in general smaller radius.

#Based on the plot above the following mapping has been determined:
# '1' = 'others'
# '2' = 'rocky'
# '3' = 'hot_jupiters',
# '4' = 'cold_gas_giants'


# Copy type column type from exo_type over to exo and rename the factor labels
exo <- exo %>%
  mutate(type = factor(exo_type$type)) %>%
  mutate(type = recode(type, '1' = 'others',
                             '2' = 'rocky',
                             '3' = 'hot_jupiters',
                             '4' = 'cold_gas_giants')) 

########################################################################################
# 8) Use a histogram and a violin plot to illustrate how these clusters relate to the log-mass of the exoplanet.
########################################################################################

# plotting the histogram with removed NAs in type variable
exo %>% drop_na(type) %$%
  ggplot(., aes(x = mass %>% log)) +
           geom_histogram(aes(fill = type), alpha = 0.6) + 
  
  # Add custom theme
  theme_pv() +
  
  # Add text and descriptions
  labs(title = 'Exploring exoplanets',
       subtitle = "Distribution of exoplanets's masses grouped by type",
       caption = 'Data: Open Exoplanet Catalogue\nPlot: @frenkxs',
       x = 'Mass in multiples of Jupiter mass (log scale)',
       y = '') +
  
  # Add custom colour scale and legend title
  scale_fill_brewer(type = 'qual', palette = 'Paired', 
                    name = "Exoplanet type") +
  
  # Add custom tick labels to x axis (with values converted form log-parsec to parsec)
  scale_x_continuous(breaks = c(log(0.001), log(0.1), log(10)),
                     label = c(0.001, '0.1', '10'))


# Violin plot
exo %>% drop_na(type) %$%
  ggplot(.) +
  geom_violin(aes(fill = type, y = mass %>% log, x = type), alpha = 0.5) +
  
  # add custom theme
  theme_pv() +
  
  # add text and descriptions
  labs(title = 'Exploring exoplanets',
       subtitle = 'Distribution of mass for each exoplanet type',
       caption = 'Data: Open Exoplanet Catalogue\nPlot: @frenkxs',
       x = 'Exoplanet type',
       y = 'Mass in multiples of Jupiter mass (log scale)') +
  
  # add custom colour scale and legend title
  scale_fill_brewer(type = 'qual', palette = 'Paired', guide = FALSE) +

  # add custom tick labels to x axis (with values converted form log-parsec to parsec)
  scale_y_continuous(breaks = c(log(0.001), log(0.1), log(10)),
                     label = c(0.001, '0.1', '10'))


  

########################################################################################
# 9*) transform r_asc and decl into the equivalent values in seconds and use these as coordinates to represent a celestial map for the exoplanets.
########################################################################################

# convert the time value in r_asc and decl to seconds
exo <- exo %>%
  mutate(r_asc = period_to_seconds(hms(r_asc)),
         decl = period_to_seconds(hms(decl)))

# scatterplot with r_asc and decl
ggplot(exo, aes(x = r_asc, y = decl)) +
  geom_point(alpha = 0.5, colour = 'grey50') +
  theme_pv() +
  
  # add text and descriptions
  labs(title = 'Exploring exoplanets',
       subtitle = 'Celestial map of exoplanets',
       caption = 'Data: Open Exoplanet Catalogue\nPlot: @frenkxs',
       x = 'Right ascension in seconds',
       y = 'Declination in seconds') +
  

#######################################################################################
# 10) create an animated time series where multiple lines illustrate the evolution over time of the total number of exoplanets discovered for each method up to that year.
########################################################################################

########################
## Data preparation ####
########################

# Get counts and cumulative counts of discovered exoplanets per year and per method. Save the data in a new tibble 'discovery'
discovery <- exo %>% 
  group_by(year, meth, .drop = FALSE) %>% 
  count() %>% 
  group_by(meth) %>%
  mutate(cum = cumsum(n))

# Arrange the data by year and the number of planet discovered by each method
discovery <- discovery %>%
  arrange(year, cum) %>%
  ungroup() %>%
  ungroup()


# Get the log values in for better visualisation (the number of exoplanets discovered by transit method is much higher than the number of exoplanets discovered by other methods. Plotting these numbers in a normal scale will make it difficult to visualise the difference in numbers between the different methods)
discovery <- discovery %>%
  mutate(log_cum = cum %>% log)

# Replace the infinity values for cum == 0
discovery <- discovery %>%
  mutate(log_cum = replace(log_cum, log_cum < 0, 0))

# Add missing year 1993 (it will have the same counts as 1992, but the animation will not skip it)
discovery <- discovery %>% 
  add_row(year = rep(1993, 5), 
          meth = discovery$meth[1:5],
          n = rep(0, 5),
          cum = discovery$cum[1:5],
          log_cum = discovery$log_cum[1:5], .after = 5)

# Add missing year label for 2019
discovery$year[is.na(discovery$year)] <- 2019


######################
# Line animated plot #
###################### 

ggplot(data = discovery, aes(x = year, y = log_cum, colour = meth, group = meth)) +
  
  # The main timeline for each method
  geom_line(size = 2) +
  
  # A line connecting the labels with their corresponding lines
  geom_segment(aes(xend = 2020, yend = log_cum), linetype = 2, colour = 'grey') + 
  
  # Point at the end of the each line
  geom_point(size = 3) + 
  
  # Labels at the right hand side of the plot
  geom_text(aes(x = 2020, label = meth), hjust = 0, size = 5, family = 'mono') + 
  
  # Add custom theme
  theme_pv() +

  # Turn cliping off so that the labels are not cut off
  coord_cartesian(clip = 'off') + 
  
  # Add the text and description
  labs(title = 'Exploring exoplanets',
    subtitle = 'How many exoplanets were discovered by each method (1992-2019)', 
    caption = 'Data: Open Exoplanet Catalogue\nPlot: @frenkxs',
    y = 'Number of exoplanets discovered (log scale)',
    x = 'Year') +
  
  # Add custom tick labels to y and x axes (with values converted back form log sum)
  scale_y_continuous(breaks = c(log(1), log(10), log(100), log(1000)),
                     label = c('1', '10','100','1000')) +
  
  scale_x_continuous(limits = c(1992, 2020),
    breaks = seq(1995, 2020, by = 5),
                     label = c(1995, '2000', '2005', '2010', 2015, '2020')) +
  
  # Add custom colour scale
  scale_colour_brewer(type = 'qual', palette = 'Paired') +
  
  # Add margin to the plot so that the text on the right is not cut off
  theme(plot.margin = margin(5, 80, 5, 5),
        legend.position = 'none') +
  
  # Animate the plot
  transition_reveal(year) 


######################
# Bar chart race ##### 
######################


ggplot(data = discovery, aes(x = meth, group = meth)) +
  
  # Use tiles to draw the racing bars
  geom_tile(aes(y = log_cum / 2, 
                height = log_cum,
                width = 0.9, fill = meth), alpha = 0.7) +
  
  # Add annotation to each bar
  geom_text(aes(y = log_cum, label = as.character(cum)), 
            size = 7, nudge_y = 1.1, family = 'mono') +
  geom_text(aes(x = 1.5, y = (log(20000)), label = paste0(year)), size = 8, 
            color = 'gray45', family = 'mono') +
  
  # Flip the axis
  coord_flip() +
  
  # Add the text and description
  labs(title = 'Exploring exoplanets',
       subtitle = 'How many exoplanets were discovered by each method (1992-2019)', 
       caption = 'Data: Open Exoplanet Catalogue\nPlot: @frenkxs',
       y = 'Number of exoplanets discovered (log scale)',
       x = '') +
  
  # Add custome theme
  theme_pv() +
  
  # Add custom colour scale
  scale_fill_brewer(type = 'qual', palette = 'Paired') +
  
  # Add custom tick labels to y and x axes (with values converted back form log sum)
  scale_y_continuous(breaks = c(log(1), log(10), log(100), log(1000)),
                     label = c('1', '10','100','1000'),
    
                     # stretch the plot a bit so the year on the right hands fits in
                     limits = c(log(1), log(40000))) +
  
  # Tweak the theme
  theme(legend.position = 'none',
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = rel(1.3))) +
  
  # Animate the plot
  transition_states(year, transition_length = 12, state_length = 10) +
  ease_aes('cubic-in-out') 
  


#######################################################################################
# 11*) create an interactive plot with Shiny where you can select the year (slider widget, with values >= 2009) and exoplanet type. Exoplanets appear as points on a scatterplot (log-mass vs log-distance coloured by method) only if they have already been discovered. If type is equal to "all" all types are plotted together.
########################################################################################

# See the 'ui.R' and 'server.R' scripts

runApp('exo_map')


# 12) Use STAN to perform likelihood maximisation on a regression model where log-period is the response variable and the logs of host_mass, host_temp and axis are the covariates (exclude rows that contain at least one missing value). Include an intercept term in the regression model.
 
# create a new dataset to be fed into stan
exo_period <- exo %>%
  select(period, host_mass, host_temp, axis) %>%
  drop_na() %>%
  mutate(period = period %>% log, 
         host_mass =  host_mass %>% log, 
         host_temp = host_temp %>% log,
         axis = axis %>% log)
  

# Set it up in a list with the same names as in the stan file
exo_period_lm <- list(N = nrow(exo_period),
                     x = as.matrix(select(exo_period, host_mass, host_temp, axis)),
                     y = exo_period$period,
                     K = 3)


# Maximum likelihood model
stan_model <- stan_model('period-lm.stan')
stan_run <- optimizing(stan_model, data = exo_period_lm)
print(stan_run)

# 13) Extend the model in (12) by specifying standard Gaussian priors for the intercept and slope terms, and a Gamma(1,1) prior for the standard deviation of errors. Obtain approximate samples from the posterior distribution of the model. 




# 14) Include in your RMarkdown document a few posterior summaries plots (e.g. estimated posterior densities) from (13) for the parameters of interest.
# 
# 15) Embed the Shiny app from (11) in your RMarkdown document.
