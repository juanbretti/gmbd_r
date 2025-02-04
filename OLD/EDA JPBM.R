# Preparation ----

## Load libraries ----

# General porpuse
library(tidyverse)
library(data.table)
library(lubridate)

# Descriptive
library(skimr)

# Visualization
library(ggplot2)
library(PerformanceAnalytics)
library(corrplot)

# Mapping
library(leaflet)
library(leaflet.extras)

# Calculations
library(forecast)
library(caret)
library(mice)

# Parallel
library(foreach)
library(doParallel)

## Helper functions ----

# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## Load data ----

path_ <- '~/gmbd_r'

data_solar <- readRDS(file = file.path(path_, 'data', 'solar_dataset.RData'))
data_station <- fread(file = file.path(path_, 'data', 'station_info.csv'))
data_add <- readRDS(file = file.path(path_, 'data', 'additional_variables.RData'))

## Transform data ----

# Source dataset
data_solar <- data_solar[j = Date2 := as.Date(x = Date, format = "%Y%m%d")]

# Add date conversions
data_solar <- data_solar %>% 
  mutate(Year = year(Date2),
         Month = month(Date2, label = TRUE),
         Day = day(Date2),
         Day_Of_Year = yday(Date2),
         Day_Of_Week = wday(Date2, label = TRUE, week_start = 1)) %>% 
  as.data.table(.)

# Columns defined from the enunciate
data_solar_col_produ <- colnames(data_solar)[2:99]
data_solar_col_predi <- colnames(data_solar)[100:456]
data_solar_col_dates <- setdiff(colnames(data_solar), c(data_solar_col_produ, data_solar_col_predi))

# Additional data to be considered
data_add <- data_add[j = Date2 := as.Date(x = Date, format = "%Y%m%d")]

# Add date conversions
data_add <- data_add %>% 
  mutate(Year = year(Date2),
         Month = month(Date2, label = TRUE),
         Day = day(Date2),
         Day_Of_Year = yday(Date2),
         Day_Of_Week = wday(Date2, label = TRUE, week_start = 1)) %>% 
  as.data.table(.)

# Columns defined from the enunciate
data_add_col <- colnames(data_add)[2:101]
data_add_col_dates <- setdiff(colnames(data_add), data_add_col)

data_solar_train <- data_solar[i = 1:5113]
data_solar_test <- data_solar[i = 5114:nrow(data_solar), j = .SD, .SDcols = c(data_solar_col_dates, data_solar_col_predi)]

# Overview: Data structure and content ----

## General overview ----

### NA check

f_check_na <- function(x) {
  count_na <- sum(is.na(x))
  count_total <- dim(x)[1]*dim(x)[2]
  variable_name <- deparse(substitute(x))
  if(count_na == 0) out <- sprintf("No NA in the dataset '%s'", variable_name)
  else out <- sprintf("There are %3.1f%% NA in the dataset '%s'", count_na/count_total*100, variable_name)
  return(out)
}

f_check_na(data_solar_train)
f_check_na(data_solar_test)
f_check_na(data_station)
f_check_na(data_add)

## Solar dataset ----

### Training

skim(data_solar_train)
glimpse(data_solar_train)

### Testing

skim(data_solar_test)
glimpse(data_solar_test)

## Statios informaiton

skim(data_station)
glimpse(data_station)

## Additional information

skim(data_add)
glimpse(data_add)

# Descriptive plots ----

## Principal weather stations ----
# Weather stations sorted by volume

principal_weather_station <- data_solar_train %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  group_by(WeatherStation) %>% 
  summarise(ValueSum = sum(as.numeric(Value))) %>% 
  arrange(desc(ValueSum)) %>% 
  select(WeatherStation) %>% 
  pull()

top_ <- 7

p_top <- data_solar_train %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  filter(WeatherStation %in% principal_weather_station[1:top_]) %>% 
  group_by(WeatherStation) %>% 
  summarise(ValueSum = sum(as.numeric(Value))) %>% 
  mutate(WeatherStation_f = factor(WeatherStation, levels = rev(principal_weather_station))) %>% 
  ggplot(aes(x = WeatherStation_f, y = ValueSum/1e6, fill = WeatherStation_f)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = 'Weather Station', y = 'Production in million', title = 'Top')

p_bottom <- data_solar_train %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  filter(WeatherStation %in% principal_weather_station[(length(principal_weather_station)-top_):length(principal_weather_station)]) %>% 
  group_by(WeatherStation) %>% 
  summarise(ValueSum = sum(as.numeric(Value))) %>% 
  mutate(WeatherStation_f = factor(WeatherStation, levels = rev(principal_weather_station))) %>% 
  ggplot(aes(x = WeatherStation_f, y = ValueSum/1e6, fill = WeatherStation_f)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = 'Weather Station', y = 'Production in million', title = 'Bottom')

multiplot(p_top, p_bottom)

## Rank position change over time ----

top_ <- 10

p_rank <- data_solar_train %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  filter(WeatherStation %in% principal_weather_station[1:top_]) %>% 
  mutate(WeatherStation_f = factor(WeatherStation, levels=principal_weather_station)) %>% 
  group_by(Year, WeatherStation_f) %>% 
  summarise(ValueSum = sum(as.numeric(Value))) %>% 
  group_by(Year) %>%
  mutate(Rank = rank(-ValueSum, ties.method = "first")) %>% 
  ungroup() %>% 
  ggplot(aes(x = Year, y = Rank, color = WeatherStation_f)) +
  geom_line() +
  scale_y_reverse(breaks=1:top_) +
  labs(x = 'Weather Station', y = 'Rank', color = 'Weather Station')

p_rank

## All the datapoint ----
# Descriptive plot to show the production recorded in millions

p_all <- data_solar_train %>%
  select(all_of(c(data_solar_col_dates, data_solar_col_produ))) %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  ggplot(aes(x = Date2, y = Value/1e6)) +
  geom_smooth() +
  labs(x = 'Date', y = 'Production in million')

p_year <- data_solar_train %>%
  select(all_of(c(data_solar_col_dates, data_solar_col_produ))) %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  ggplot(aes(x = Year, y = Value/1e6, group = Year)) +
  geom_boxplot() +
  labs(x = 'Year', y = 'Production in million')

p_month <- data_solar_train %>%
  select(all_of(c(data_solar_col_dates, data_solar_col_produ))) %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  ggplot(aes(x = Month, y = Value/1e6)) +
  geom_boxplot() +
  labs(x = 'Month', y = '')

p_Day_Of_Week <- data_solar_train %>%
  select(all_of(c(data_solar_col_dates, data_solar_col_produ))) %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  ggplot(aes(x = Day_Of_Week, y = Value/1e6)) +
  geom_boxplot() +
  labs(x = 'Day of the week', y = '')

layout <- matrix(c(1,1,1,2,3,4),2,3, byrow=TRUE)
multiplot(p_all, p_year, p_month, p_Day_Of_Week, layout=layout)

## Evolution of principal weather stations ----
# Plot for the top highest weather stations.

top_ <- 5

p_all <- data_solar_train %>%
  select(all_of(c(data_solar_col_dates, data_solar_col_produ))) %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  filter(WeatherStation %in% principal_weather_station[1:top_]) %>% 
  ggplot(aes(x = Date2, y = Value/1e6, color = WeatherStation)) +
  geom_smooth() +
  labs(x = 'Date', y = 'Production in million') +
  theme(legend.position = "none")

p_month <- data_solar_train %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  filter(WeatherStation %in% principal_weather_station[1:top_]) %>% 
  ggplot(aes(x = Month, y = Value, color = WeatherStation)) +
  geom_boxplot() +
  labs(x = 'Month', y = 'Production in million')

multiplot(p_all, p_month)

## Training and test ----

p_train_test <- data_solar %>% 
  mutate(DataSet = ifelse(row_number()<=5113, 'Train', 'Test')) %>%
  group_by(Year, DataSet) %>% 
  summarise(N = n()) %>% 
  ggplot(aes(x = Year, y = N, fill = DataSet)) + 
  geom_col() +
  coord_flip() +
  scale_x_reverse() +
  labs(x = 'Year', y = 'Number of days')

p_train_test

## Seasonability ----
# https://towardsdatascience.com/forecasting-with-r-trends-and-seasonality-def24280e71f

data <- data_solar_train %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  group_by(Date2) %>% 
  summarise(ValueMean = mean(as.numeric(Value)))

# range(data$Date2)
data2 <- ts(data = data$ValueMean/1e6, frequency = 365, start = c(1994, 1, 01), end = c(2007, 12, 31))
plot(decompose(data2))
# plot(decompose(data2)$trend)

### Trend for the principal weather stations ----

top_ <- 5

ts_ <- function(x) {
  a <- ts(data = x, frequency = 365, start = c(1994, 1, 01), end = c(2007, 12, 31))
  a <- decompose(a)
  return(a)
}
  
data <- data_solar_train %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  group_by(WeatherStation) %>% 
  group_map(~ ts_(.x$Value)) %>% 
  setNames(data_solar_col_produ)

# bb <- lapply(principal_weather_station[1:top_], function(x) plot(data[[x]]))
lapply(principal_weather_station[1:top_], function(x) plot(data[[x]]$trend, main = x, ylab = 'Value'))

# Geolocalization ----

## Positions

data <- data_solar_train %>% 
  select(-data_solar_col_predi) %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  group_by(WeatherStation) %>%
  summarise(ValueMean = mean(Value)) %>% 
  left_join(data_station, by = c('WeatherStation' = 'stid'))

m1 <- leaflet(data = data) %>%
  addTiles() %>%
  addMarkers(lng=~elon, lat=~nlat,
             popup = ~paste(round(ValueMean/1e6, 0), "Million"), label = ~WeatherStation,
             clusterOptions = markerClusterOptions())

m1

## Heatmap

# https://rstudio.github.io/leaflet/
# https://github.com/r-spatial/leafpop
# https://stackoverflow.com/questions/29034863/apply-a-ggplot-function-per-group-with-dplyr-and-set-title-per-group
# http://environmentalinformatics-marburg.github.io/mapview/popups/html/popups.html#popupgraph
# https://rpubs.com/bhaskarvk/leaflet-heat
# https://bhaskarvk.github.io/leaflet.extras/reference/heatmap.html

data <- data_solar_train %>% 
  select(-data_solar_col_predi) %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  group_by(WeatherStation) %>%
  summarise(ValueMean = mean(Value)) %>% 
  left_join(data_station, by = c('WeatherStation' = 'stid'))

m1 <- leaflet(data = data) %>%
  addTiles() %>%
  addCircleMarkers(lng=~elon, lat=~nlat, group = 'data_solar') %>% 
  addHeatmap(lng = ~elon, lat = ~nlat, intensity = ~ValueMean, blur = 90, max = 0.05, radius = 60)

m1

## Seasonal decomposition 
# For the top weather stations

# https://rstudio.github.io/leaflet/
# https://github.com/r-spatial/leafpop
# https://stackoverflow.com/questions/29034863/apply-a-ggplot-function-per-group-with-dplyr-and-set-title-per-group
# http://environmentalinformatics-marburg.github.io/mapview/popups/html/popups.html#popupgraph

top_ <- 5

data <- data_solar_train %>% 
  dplyr::select(-data_solar_col_predi) %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  filter(WeatherStation %in% principal_weather_station[1:top_]) %>%
  group_by(WeatherStation) %>%
  summarise(ValueMean = mean(Value)) %>% 
  left_join(data_station, by = c('WeatherStation' = 'stid'))

data1 <- data_solar_train %>%
  dplyr::select(-data_solar_col_predi) %>%
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>%
  filter(WeatherStation %in% principal_weather_station[1:top_]) %>%
  group_by(WeatherStation) %>%
  do(
    plots = forecast::autoplot(decompose(ts(data = .$Value, frequency = 365, start = c(1994, 1, 01), end = c(2007, 12, 31))))
    )

m1 <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = data, lng=~elon, lat=~nlat, group = 'data_solar') %>% 
  leafpop::addPopupGraphs(data1$plots, group = 'data_solar', width = 300, height = 400)

m1

# Correlations ----

# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

top_ <- 5
top_pc <- 10

data <- data_solar_train %>% 
  dplyr::select(principal_weather_station[1:top_], data_solar_col_predi[1:top_pc])

chart.Correlation(data, histogram=TRUE) #, pch=19

# Histograms, density kernel and boxplot ----

# black: histogram
# blue: density kernel distribution

# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
# https://stackoverflow.com/questions/14262497/fixing-the-order-of-facets-in-ggplot

## General production

data <- data_solar_train %>% 
  dplyr::select(data_solar_col_produ) %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value')

p_histogram_density <- ggplot(data = data, aes(x = Value/1e6)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="blue") +
  labs(x = '', y = 'Density')

p_boxplot <- ggplot(data = data, aes(x = Value/1e6)) +
  geom_boxplot() +
  stat_boxplot(coef = 1.5, outlier.colour = 'red', outlier.alpha = 0.1) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = 'Production in million', y = '')

layout <- matrix(c(1,1,1,2),4,1, byrow=TRUE)
multiplot(p_histogram_density, p_boxplot, layout = layout)

## Weather stations

# No outliers detected, considering 1.5
# https://en.wikipedia.org/wiki/Interquartile_range

data <- data_solar_train %>% 
  dplyr::select(data_solar_col_produ) %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  mutate(WeatherStation_f = factor(WeatherStation, levels=principal_weather_station))

p_histogram_density <- ggplot(data = data, aes(x = Value/1e6)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="blue") +
  facet_wrap(vars(WeatherStation_f))

p_boxplot <- ggplot(data = data, aes(x = Value/1e6)) +
  geom_boxplot() +
  stat_boxplot(coef = 1.5, outlier.colour = 'red', outlier.alpha = 0.1) +
  facet_wrap(vars(WeatherStation_f)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p_histogram_density
p_boxplot

## Weather predictors
# It's logical there are more possible "outliers" in the PC, because the definition is to have the biggest dispersion/range in the data when doing "principal component analysis".

data <- data_solar_train %>% 
  dplyr::select(data_solar_col_predi) %>% 
  pivot_longer(cols = all_of(data_solar_col_predi), names_to = 'PC', values_to = 'Value') %>% 
  filter(PC %in% data_solar_col_predi[1:50]) %>% 
  mutate(PC_f = factor(PC, levels=data_solar_col_predi))

ggplot(data = data, aes(x = Value)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="blue") +
  facet_wrap(vars(PC_f))

ggplot(data = data, aes(x = Value)) +
  geom_boxplot() +
  stat_boxplot(coef = 1.5, outlier.colour = 'red', outlier.alpha = 0.1) +
  facet_wrap(vars(PC_f)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# Variable importance ----
## Using standard 'lapply'

# Using the formula from the forum:
# C:\Users\juanb\OneDrive\GMBD\STATISTICAL PROGRAMMING - R (MBD-EN-BL2020J-1_32R369_316435)\Session 12 - Forum\ex9.R

top_ <- 5

select_important<-function(dat, n_vars, y){
  varimp <- filterVarImp(x = dat, y=y, nonpara=TRUE)
  varimp <- data.table(variable=rownames(varimp),imp=varimp[, 1])
  varimp <- varimp[order(-imp)]
  selected <- varimp$variable[1:n_vars]
  return(selected)
}

# x <- data_solar_train[, data_solar_col_predi, with = FALSE]
# y <- data_solar_train$ACME
# table(is.na(y))
# varimp <- filterVarImp(x = x, y=y, nonpara=TRUE)
# select_important(dat = x, n_vars = 10, y = y)

# data_solar_importance <- lapply(principal_weather_station[1:top_], function(x) select_important(dat = data_solar_train[, data_solar_col_predi, with = FALSE], 
#                                                                                              n_vars = 10, 
#                                                                                              y = data_solar_train[[x]]))

# saveRDS(data_solar_importance, file.path(path_, 'storage', 'data_solar_importance.rds'))
# data_solar_importance <- readRDS(file.path(path_, 'storage', 'data_solar_importance.rds'))
# names(data_solar_importance) <- principal_weather_station[1:top_]

## Using parallel computing

top_ <- 98

cl<-makeCluster(detectCores())
registerDoParallel(cl)

select_important<-function(dat, n_vars, y){
  varimp <- filterVarImp(x = dat, y=y, nonpara=TRUE)
  varimp <- data.table(variable=rownames(varimp),imp=varimp[, 1])
  varimp <- varimp[order(-imp)]
  selected <- varimp$variable[1:n_vars]
  return(selected)
}

# time_importance <- system.time({
# data_solar_importance <- foreach (x = principal_weather_station[1:top_],
#                                   .inorder=FALSE, .verbose=FALSE, .errorhandling="remove",
#                                   .packages=(.packages()), .export=ls(envir=globalenv())) %dopar% {
#                                     select_important(dat = data_solar_train[, data_solar_col_predi, with = FALSE], 
#                                                      n_vars = 50,
#                                                      y = data_solar_train[[x]])
#                                   }
# })
print(time_importance) #1459.36
stopCluster(cl)

# saveRDS(data_solar_importance, file.path(path_, 'storage', 'data_solar_importance_parallel.rds'))
data_solar_importance <- readRDS(file.path(path_, 'storage', 'data_solar_importance_parallel.rds'))

names(data_solar_importance) <- principal_weather_station[1:top_]

# Additional information ----

## Feature visualization ----

data <- data_add %>% 
  pivot_longer(cols = all_of(data_add_col), names_to = 'Variables', values_to = 'Value')

p_histogram_density <- ggplot(data = data, aes(x = Value/1e6)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="blue") +
  labs(x = '', y = 'Density')

p_boxplot <- ggplot(data = data, aes(x = Value/1e6)) +
  geom_boxplot() +
  stat_boxplot(coef = 1.5, outlier.colour = 'red', outlier.alpha = 0.1) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = 'Production in million', y = '')

layout <- matrix(c(1,1,1,2),4,1, byrow=TRUE)
multiplot(p_histogram_density, p_boxplot, layout = layout)

mean_ <- data_add %>% 
  dplyr::select(all_of(data_add_col)) %>% 
  summarise_all(~mean(., na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(), names_to = 'Variables', values_to = 'Mean')

median_ <- data_add %>% 
  dplyr::select(all_of(data_add_col)) %>% 
  summarise_all(~median(., na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(), names_to = 'Variables', values_to = 'Median')

sd_ <- data_add %>% 
  dplyr::select(all_of(data_add_col)) %>% 
  summarise_all(~sd(., na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(), names_to = 'Variables', values_to = 'SD')

na_ <- data_add %>% 
  dplyr::select(all_of(data_add_col)) %>% 
  summarise_all(~sum(is.na(.))/length(.)) %>% 
  pivot_longer(cols = everything(), names_to = 'Variables', values_to = 'NA_')

stats_ <- mean_ %>% 
  left_join(median_, by = 'Variables') %>% 
  left_join(sd_, by = 'Variables') %>% 
  left_join(na_, by = 'Variables')

p_mean <- stats_ %>% 
  ggplot(aes(Mean)) +
  geom_histogram(fill = 'blue') +
  labs(x = 'Feature Mean', y = 'Count')

p_median <- stats_ %>% 
  ggplot(aes(Median)) +
  geom_histogram(fill = 'red') +
  labs(x = 'Feature Median', y = 'Count')

p_sd <- stats_ %>% 
  ggplot(aes(SD)) +
  geom_histogram(fill = 'green') +
  labs(x = 'Feature Standard Deviation', y = 'Count')

p_na <- stats_ %>% 
  ggplot(aes(NA_)) +
  geom_histogram(fill = 'orange') +
  labs(x = 'Percentage of NA', y = 'Count')

p_mean_sd <- stats_ %>% 
  ggplot(aes(x = Mean, y = SD)) +
  geom_point(color = 'darkgreen') +
  labs(x = 'Mean', y = 'Standard Deviation')

layout <- matrix(c(1,2,3,4,5,5),3,2, byrow=TRUE)
multiplot(p_mean, p_median, p_sd, p_na, p_mean_sd, layout = layout)

## Correlation between additional information ----

data <- data_add[, ..data_add_col]

# https://stackoverflow.com/questions/17079637/correlation-matrix-in-r-returning-na-values
cor_ <- cor(data, use="pairwise.complete.obs")

# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(data)

dat <- data.frame(cor = matrix(cor_))
ggplot(dat, aes(x = cor)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="blue") +
  labs(x = 'Paerson correlation', y = 'Density')

p_corrplot <- corrplot(cor_, type="upper", order="hclust", p.mat = p.mat, sig.level = 0.01, insig = "blank")

## Complete data ----
# Using library 'mice'.
# Considering the number of missing values, we propose the use of 'mice' to completete the dataset.

# https://ipub.com/dev-corner/apps/r-package-downloads/
# https://campus.ie.edu/webapps/discussionboard/do/message?action=list_messages&course_id=_114320331_1&nav=discussion_board&conf_id=_251223_1&forum_id=_112929_1&message_id=_4663169_1
# C:\Users\juanb\OneDrive\GMBD\STATISTICAL PROGRAMMING - R (MBD-EN-BL2020J-1_32R369_316435)\Session 12 - Forum\EXERCISE 7.R
# https://www.r-bloggers.com/how-to-save-and-load-datasets-in-r-an-overview/

data <- data_add[, ..data_add_col]

m_ <- 5
# df2 <- mice(data, m=m_, maxit=10, meth='pmm', seed=500)
# saveRDS(df2, file.path(path_, 'storage', 'data_add_mice.rds'))
df2 <- readRDS(file.path(path_, 'storage', 'data_add_mice.rds'))
# summary(df2)

# Average of all the Multivariate Imputation
df3 <- 0
for (i in 1:m_) df3 <- df3 + complete(df2, i)
df3 <- df3/m_

## Principal component analysis ----
### Using 'caret'

# https://topepo.github.io/caret/pre-processing.html

pca_threshold <- 0.90
model_ <- preProcess(df3, method = c("pca"), thresh = pca_threshold)
df4 <- predict(model_, df3)

data_add_col_pca <- colnames(df4)
data_add_pca <- bind_cols(data_add[, ..data_add_col_dates], df4)

### Histograms, density kernel and boxplot

data <- data_add_pca %>% 
  pivot_longer(cols = all_of(data_add_col_pca), names_to = 'PC', values_to = 'Value') %>% 
  mutate(PC_ = factor(PC, levels = data_add_col_pca))

p_histogram_density <- ggplot(data = data, aes(x = Value)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="blue") +
  facet_wrap(vars(PC_))

p_boxplot <- ggplot(data = data, aes(x = Value)) +
  geom_boxplot() +
  stat_boxplot(coef = 1.5, outlier.colour = 'red', outlier.alpha = 0.1) +
  facet_wrap(vars(PC_)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p_histogram_density
p_boxplot