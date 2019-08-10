# load libraries
library(readr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(viridis)
library(ggthemes)
library(ggiraph)
library(tidyr)
library(lubridate)
library(gganimate)



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
  geom_point(aes(color = meth), alpha = 0.4) +
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
fit <- kmeans(na.omit(radii[, 2:3]), centers = 4, iter.max = 20, nstart = 3)


# 7*) Add the clustering labels to the dataset through a new factor column called 'type', with levels 'rocky', 'hot_jupiters', 'cold_gas_giants', 'others'; similarly to https://en.wikipedia.org/wiki/Exoplanet#/media/File:ExoplanetPopulations-20170616.png

# Add the cluster labels to the radii dataset, while skipping rows with missing values in either earth_radius or period (they will have the type label NA)
radii$type[which( complete.cases(radii[, 2:3]) )] <- fit$cluster

# plot(log(exo$period), log(exo$earth_radius), col = radii$type)
# plot(log(exo$mass), log(exo$dist), col = radii$type)

# Copy type column from radii over to exo and rename the factor labels
exo <- exo %>%
  mutate(type = factor(radii$type)) %>%
  mutate(type = recode(type, '1' = 'hot_jupiters',
                       '2' = 'cold_gas_giants',
                       '3' = 'others',
                       '4' = 'rocky')) 

# 8) Use a histogram and a violin plot to illustrate how these clusters relate to the log-mass of the exoplanet.
exo %>% drop_na(type) %T>%
{print(ggplot(., aes(x = mass %>% log, fill = type)) +
  geom_histogram(aes(colour = type), alpha = 0.2) +
    theme_pv())}

ggplot(filter(exo, !is.na(type)), aes(fill = type)) +
  geom_violin(aes(colour = type, y = mass %>% log, x = type), alpha = 0.8) +
  theme_pv()


# 9*) transform r_asc and decl into the equivalent values in seconds and use these as coordinates to represent a celestial map for the exoplanets.

# convert the time value in r_asc and decl to seconds
exo <- exo %>%
  mutate(r_asc = period_to_seconds(hms(r_asc)),
         decl = period_to_seconds(hms(decl)))

# scatterplot with r_asc and decl
ggplot(exo, aes(x = period_to_seconds(hms(r_asc)), y = period_to_seconds(hms(decl)))) +
  geom_point(alpha = 0.3) +
  theme_pv()


# 10) create an animated time series where multiple lines illustrate the evolution over time of the total number of exoplanets discovered for each method up to that year.


# get counts and cumulative counts of discovered exoplanets per year and per method
discovery <- exo %>% 
  group_by(year, meth, .drop = FALSE) %>% 
  count() %>% 
  group_by(meth) %>%
  mutate(cum = cumsum(n))

# arrange the data by year and the number of planet discovered by each method
discovery <- discovery %>%
  arrange(year, cum) %>%
  ungroup()

# # add order column to indicate relative order of each method for each year  
# discovery <-  discovery %>%
#   ungroup() %>%
#   mutate(order = rep(seq(1:5), nrow(discovery) / 5))

# get the log values in for better visualisation
discovery <- discovery %>%
  mutate(log_cum = cum %>% log)

# replace the infinity values for cum == 0
discovery <- discovery %>%
  mutate(log_cum = replace(log_cum, log_cum < 0, 0))


 
# line plot with animation
ggplot(data = discovery %>% drop_na(), aes(x = year, y = (cum %>% log), colour = meth)) +
  geom_line() +
  geom_segment(aes(xend = 2018, yend = (cum %>% log)), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = 2025, label = meth), hjust = 0) + 
  transition_reveal(year) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'Discovery of exoplanets', y = 'Number of exoplanets discovered') + 
  theme_pv() +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5),
        legend.position = 'none')


# bar chart race 

# getting the data ready

# reorder factors in meth for easier plotting
discovery$meth <- factor(discovery$meth, 
                         levels = c("timing", "imaging", "microlensing",
                                    'RV', 'transit'))


# add missing year 1993 (it will have the same counts as 1992, but the animation will not skip it)
discovery <- discovery %>% 
  add_row(year = rep(1993, 5), 
          meth = discovery$meth[1:5],
          n = rep(0, 5),
          cum = discovery$cum[1:5],
          log_cum = discovery$log_cum[1:5], .after = 5)

# add missing year label for 2019
discovery$year[is.na(discovery$year)] <- 2019

# plot
ggplot(data = discovery, aes(x = meth, group = meth)) +
  geom_tile(aes(y = log_cum / 2, 
                height = log_cum,
                width = 0.9, fill = meth), alpha = 0.7) +
  geom_text(aes(y = log_cum, label = as.character(cum)), 
            size = 7, nudge_y = 1.1, family = 'mono') +
  geom_text(aes(x = 1.5, y = (log(20000)), label = paste0(year)), size = 8, color = 'gray45', family = 'mono') +
  coord_flip() +
  labs(title = 'Discovery of exoplanets',
       caption = 'plot by @frenkxs',
       x = '',
       y = '') +
  transition_states(year,
                    transition_length = 12, state_length = 10) +
  ease_aes('cubic-in-out') +
  theme_pv() +
  scale_fill_viridis_d(option = 'A') +
  scale_y_continuous(breaks = c(log(1), log(10), log(50), 
                                log(300), log(2000)),
                     label = c('1', '10', '50','300','2000'),
                     limits = c(log(1), log(30000))) +
  theme(legend.position = 'none',
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = rel(1.3)),
        plot.title = element_text(size = 20))



# 11*) create an interactive plot with Shiny where you can select the year (slider widget, with values >= 2009) and exoplanet type. Exoplanets appear as points on a scatterplot (log-mass vs log-distance coloured by method) only if they have already been discovered. If type is equal to "all" all types are plotted together.
#

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
