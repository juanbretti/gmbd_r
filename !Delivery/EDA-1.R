# GMBD 2020 Intake
# Group E
#   Juan Pedro Bretti Mandarano
#   Nicolas Greull
#   Zhaoxue Li
#   Gauchet van Antwerpen
#   Asad Umar

################################ [0] LIBRARIES #####################################

# General purpose
library(tidyverse)
library(lubridate)
library(data.table)
library(outliers)

# Descriptive
library(ggplot2)
library(ggpubr)
library(skimr)
library(DataExplorer)
library(forecast)
library(PerformanceAnalytics)
library(corrplot)

# Mapping
library(leaflet)
library(leaflet.extras)
library(sf)
library(ggmap)

# Calculations
library(caret)
library(mice)

# Parallel
library(doParallel)
library(foreach)

################################ [1] LOAD DATA & TRANSFORM #############################

data_solar <- readRDS(file = file.path('data', 'solar_dataset.RData'))
data_station <- fread(file = file.path('data', 'station_info.csv'))
data_add <- readRDS(file = file.path('data', 'additional_variables.RData'))

# Source dataset
data_solar <- data_solar[j = Date2 := as.Date(x = Date, format = "%Y%m%d")]

# Add date conversions
data_solar <- data_solar %>% 
  mutate(Year = year(Date2),
         Month = month(Date2),
         Day = day(Date2),
         Day_Of_Year = yday(Date2),
         Day_Of_Week = wday(Date2)) %>% 
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
         Month = month(Date2),#Error in month(Date2, label = TRUE) : unused argument (label = TRUE)
         Day = day(Date2),
         Day_Of_Year = yday(Date2),
         Day_Of_Week = wday(Date2)) %>% #Error in wday(Date2, label = TRUE, week_start = 1) : unused arguments (label = TRUE, week_start = 1)
  as.data.table(.)

# Columns defined from the enunciate
data_add_col <- colnames(data_add)[2:101]
data_add_col_dates <- setdiff(colnames(data_add), data_add_col)

# Split train and prediction
data_solar_train <- data_solar[i = 1:5113]
data_solar_test <- data_solar[i = 5114:nrow(data_solar), j = .SD, .SDcols = c(data_solar_col_dates, data_solar_col_predi)]

################################# [2] Overview #####################################
################################# [2.1] Check for NAs #####################################
f_check_na <- function(x) {
  count_na <- sum(is.na(x))
  count_total <- dim(x)[1]*dim(x)[2]
  variable_name <- deparse(substitute(x))
  if(count_na == 0) {
    text <- sprintf("No NA in the dataset '%s'", variable_name)
    number <- count <- 0
  } else {
    number <- round(count_na/count_total*100, 1)
    text <- sprintf("There are %3.1f%% NA in the dataset '%s'", number, variable_name)
    count <- count_na
  }
  return(list(text = text,
              number = number,
              count = count))
}

f_check_na(data_solar_train)$text
f_check_na(data_solar_test)$text
f_check_na(data_station)$text
f_check_na(data_add)$text

############################# [2.2] Check for Constant Variables ##########################

# Returns the unique value of each column of x
check_unique <- function(x){
  return(length(unique(x)));
}

check_constants <- function(in_dat, criteria = 1){
  sapply(in_dat, check_unique);
}

# Function to detect constant variables within a given criteria
criteria_variables <- function(in_dat, criteria = 1){
  
  n_unique_values <- sapply(in_dat, check_unique);
  criteria_variables <- names(n_unique_values)[n_unique_values == criteria];
  if(length(criteria_variables) > 0) out <- sprintf(variable_name, "is a constant variable.")
  else out <- sprintf("There is no constant variable.")
  return(out);
}

criteria_variables(data_solar_train)
criteria_variables(data_solar_test)
criteria_variables(data_station)
criteria_variables(data_add)

#################################### [2.3] Outliers #########################################

##Based on:
##https://www.kaggle.com/rtatman/data-cleaning-challenge-outliers
##https://cran.r-project.org/web/packages/outliers/outliers.pdf

#################################### [2.3.1] Outliers In Solar Production ###################

data_solar_produ_scores <- lapply(data_solar_col_produ, function(x) scores(data_solar_train[[x]], type = 'z'))
names(data_solar_produ_scores) <- data_solar_col_produ
solar_data_outlier_skim <- skim(data_solar_produ_scores)

solar_data_outlier_table <- 
table(
  abs(unlist(data_solar_produ_scores))>=3
)

################################ [3] Solar Production Dataset Overview ##############################
########################################## [3.1] Training set #######################################

skim_train <- skim(data_solar_train)
glimpse_train <- glimpse(data_solar_train)

###########################################3 [3.2] Test set ########################################
skim_test <- skim(data_solar_test)
glimpse_test <- glimpse(data_solar_test)

################################# [4] Solar Station Dataset Overview ###############################

skim_station <- skim(data_station)
glimpse_station <- glimpse(data_station)

################################# [4] Additional information Dataset Overview #############################

skim_additional <- skim(data_add)
flimpse_additional <- glimpse(data_add)

################################ [5]Descriptive Plots   ##############################################

################################ [5.1] Principal Weather Stations   ###############################

## Ranking stations by the solar energy production volume

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
  labs(x = 'Weather Station', y = 'Cumulative production in million', title = 'Top')

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
  labs(x = 'Weather Station', y = 'Cumulative production in million', title = 'Bottom')

 principal_station_plot <- ggarrange(p_top, p_bottom, nrow = 2)

################################ [5.2] RANK (IN TERMS OF PRODUCTION) CHANGE OVER TIME  ################################

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
  labs(x = 'Date', y = 'Rank', color = 'Weather Station')

p_rank

####################################### [5.3] All production data ################################

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

total_production_p <- ggarrange(p_all, 
                                ggarrange(p_year, p_month, p_Day_Of_Week, ncol = 3), 
                                nrow = 2)

################################# [5.4] DEVELOPMENT OF PRODUCTION VOLUME OVER TIME FOR THE 5 HIGHEST PRODUCING WEATHER STATIONS  #####################################

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

p_top_five_ts <- ggarrange(p_all, p_month, nrow = 2)

#################################### [5.5]Training vs Test  ###################################
p_train_test <- data_solar %>% 
  mutate(DataSet = ifelse(row_number()<=5113, 'Train', 'Test')) %>%
  group_by(Year, DataSet) %>% 
  summarise(N = n()) %>% 
  ggplot(aes(x = Year, y = N, fill = DataSet)) + 
  geom_col() +
  coord_flip() +
  scale_x_reverse() +
  labs(x = 'Year', y = 'Number of days')

####################################### [6] SEASONALITY ########################################
########################### [6.1] SEASONALITY - HOW DOES THE PRODUCTION OF ALL WEATHER STATIONS CHANGE OVER TIME AND HOW MUCH OF THE PRODUCTION IS EXPLAINED / TREND / RANDOM  #####################################

data <- data_solar_train %>%
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>%
  group_by(Date2) %>%
  summarise(ValueMean = mean(as.numeric(Value)))

# range(data$Date2)
data2 <- ts(data = data$ValueMean/1e6, frequency = 365, start = c(1994, 1, 01), end = c(2007, 12, 31))

p_seasonality_all <- plot(decompose(data2))

# ################################ [6.2] TREND VISUALIZATION FOR THE TOP 5 WEATHEER STATIONS  #####################################

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

p_seasonality_top_5 <- lapply(principal_weather_station[1:top_], function(x) plot(data[[x]]$trend, main = x, ylab = 'Value'))

######################################## [7] GEOGRAPHY ######################################
###################################### [7.1] POSITIONS #####################################

data <- data_solar_train %>%
  select(-all_of(data_solar_col_predi)) %>%
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>%
  group_by(WeatherStation) %>%
  summarise(ValueMean = mean(Value)) %>%
  left_join(data_station, by = c('WeatherStation' = 'stid'))

map_stations <- leaflet(data = data) %>%
  addTiles() %>%
  addMarkers(lng=~elon, lat=~nlat,
             popup = ~paste("Station", WeatherStation, "has", round(ValueMean/1e6, 0), "million of production"), 
             label = ~WeatherStation,
             clusterOptions = markerClusterOptions())

##################################### [7.2] HEATMAP #####################################

data <- data_solar_train %>% 
  select(-all_of(data_solar_col_predi)) %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  group_by(WeatherStation) %>%
  summarise(ValueMean = mean(Value)) %>% 
  left_join(data_station, by = c('WeatherStation' = 'stid'))

map_production <- leaflet(data = data) %>%
  addTiles() %>%
  addCircleMarkers(lng=~elon, lat=~nlat, group = 'data_solar',
                   popup = ~paste("Station", WeatherStation, "has", round(ValueMean/1e6, 0), "million of production"), 
                   label = ~WeatherStation) %>% 
  addHeatmap(lng = ~elon, lat = ~nlat, intensity = ~ValueMean, blur = 90, max = 0.05, radius = 60)

##################################### [7.3] SEASONAL DECOMPOSITION #####################################
# Deactivated, to speed up the code execution
# A copy of this plot it's being used in the Shyny app

# top_ <- 5
# 
# data <- data_solar_train %>%
#   dplyr::select(-all_of(data_solar_col_predi)) %>%
#   pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>%
#   filter(WeatherStation %in% principal_weather_station[1:top_]) %>%
#   group_by(WeatherStation) %>%
#   summarise(ValueMean = mean(Value)) %>%
#   left_join(data_station, by = c('WeatherStation' = 'stid'))
# 
# data1 <- data_solar_train %>%
#   dplyr::select(-all_of(data_solar_col_predi)) %>%
#   pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>%
#   filter(WeatherStation %in% principal_weather_station[1:top_]) %>%
#   group_by(WeatherStation) %>%
#   do(
#     plots = forecast::autoplot(decompose(ts(data = .$Value, frequency = 365, start = c(1994, 1, 01), end = c(2007, 12, 31))))
#   )
# 
# m1 <- leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data = data, lng=~elon, lat=~nlat, group = 'data_solar') %>%
#   leafpop::addPopupGraphs(data1$plots, group = 'data_solar', width = 300, height = 400)

################################ [8] COMPUTE CORRELATIONS BETWEEN TOP 5 WEATHER STATIONS (IN TERMS OF PRODUCTION) & TOP 10 PREDICTORS #####################################

top_ <- 5
top_pc <- 10

data <- data_solar_train %>%
  dplyr::select(all_of(c(principal_weather_station[1:top_], data_solar_col_predi[1:top_pc])))

p_corr <- chart.Correlation(data, histogram=TRUE) #, pch=19

################################# [9] HISTOGRAM, DENSITY KERNEL AND BOXPLOT ########################
##################################[9.1] FOR GENERAL PRODUCTION #####################################

data <- data_solar_train %>%
  select(all_of(data_solar_col_produ)) %>%
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

p_distri_prod <- ggarrange(p_histogram_density, p_boxplot, nrow = 2, heights = c(4,1))

# ################################ [9.2] FOR EACH WEATHER STATION AND PREDICTORS #####################################

# WEATHER STATION

data <- data_solar_train %>%
  select(all_of(data_solar_col_produ)) %>%
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>%
  mutate(WeatherStation_f = factor(WeatherStation, levels=principal_weather_station))

p_histogram_density <- ggplot(data = data, aes(x = Value/1e6)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="blue") +
  facet_wrap(vars(WeatherStation_f)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p_boxplot <- ggplot(data = data, aes(x = Value/1e6)) +
  geom_boxplot() +
  stat_boxplot(coef = 1.5, outlier.colour = 'red', outlier.alpha = 0.1) +
  facet_wrap(vars(WeatherStation_f)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#PREDICTORS

data <- data_solar_train %>%
  select(all_of(data_solar_col_predi)) %>%
  pivot_longer(cols = all_of(data_solar_col_predi), names_to = 'PC', values_to = 'Value') %>%
  filter(PC %in% data_solar_col_predi[1:50]) %>%
  mutate(PC_f = factor(PC, levels=data_solar_col_predi))

predictor_density <- ggplot(data = data, aes(x = Value)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="blue") +
  facet_wrap(vars(PC_f)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p_predictor_boxplot <- ggplot(data = data, aes(x = Value)) +
  geom_boxplot() +
  stat_boxplot(coef = 1.5, outlier.colour = 'red', outlier.alpha = 0.1) +
  facet_wrap(vars(PC_f)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

##################################### [10]VARIABLE IMPORTANCE #####################################
# Deactivated, to speed up the code execution
# The output of this chunk it's being sourced.

# top_ <- 98
# 
# cl<-makeCluster(detectCores())
# registerDoParallel(cl)

# top_ <- 98 #Selecting all the Weather Stations
# 
# cl<-makeCluster(detectCores())
# registerDoParallel(cl)
# 
# 
# select_important<-function(dat, n_vars, y){
#   varimp <- filterVarImp(x = dat, y=y, nonpara=TRUE)
#   varimp <- data.table(variable=rownames(varimp),imp=varimp[, 1])
#   varimp <- varimp[order(-imp)]
#   selected <- varimp$variable[1:n_vars]
#   return(selected)
# }

# time_importance <- system.time({
# data_solar_importance <- foreach (x = principal_weather_station[1:top_],
#                                   .inorder=FALSE, .verbose=FALSE, .errorhandling="remove",
#                                   .packages=(.packages()), .export=ls(envir=globalenv())) %dopar% {
#                                     select_important(dat = data_solar_train[, data_solar_col_predi, with = FALSE],
#                                                      n_vars = 50,
#                                                      y = data_solar_train[[x]])
#                                   }
# })
#print(time_importance) #1459.36
#stopCluster(cl)

# saveRDS(data_solar_importance, file.path('storage', 'data_solar_importance_parallel.rds'))
data_solar_importance <- readRDS(file.path('storage', 'data_solar_importance_parallel.rds'))

names(data_solar_importance) <- principal_weather_station[1:top_]

################################# [11] ADDITIONAL DATASET FEATURE VISUALIZATION ########################
# ################################ [11.1] DISTRIBUTION OF ADDITIONAL DATASET VALUES #####################################

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

p_add_histogtam_boxplot <- ggarrange(p_histogram_density, p_boxplot, nrow = 2)

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

p_additional <- ggarrange(ggarrange(p_mean, p_median, p_sd, p_na, ncol =2 , nrow = 2), p_mean_sd, nrow = 2, heights = c(2, 1))

# ##################################### [11.2] CORRELATION BETWEEN ADDITIONAL INFORMATION #####################################

##################################### [10.2] CORRELATION BETWEEN ADDITIONAL INFORMATION #####################################

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

corrplot(cor_, type="upper", order="hclust", p.mat = p.mat, sig.level = 0.01, insig = "blank", tl.pos	= 'n')

################################# [11.3] USE 'MICE' TO COMLETE 'ADDITIONAL DATA' VALUES #####################################
# Deactivated, to speed up the code execution

data <- data_add[, ..data_add_col]

m_ <- 5
# df2 <- mice(data, m=m_, maxit=10, meth='pmm', seed=500)
# saveRDS(df2, file.path('storage', 'data_add_mice.rds'))
df2 <- readRDS(file.path('storage', 'data_add_mice.rds'))
# summary(df2)

# Average of all the Multivariate Imputation
df3 <- 0
for (i in 1:m_) df3 <- df3 + complete(df2, i)

complete_mice <- df3/m_

################################ [11.4] OUTLIERS IN ADDITIONAL DATASET ####################

f_scores <- function(x) {
  data <- data_add[[x]]
  data <- data[!is.na(data)]
  res <- outliers::scores(data, type = 'z')
  return(res)
}

data_add_scores <- lapply(data_add_col, f_scores)
names(data_add_scores) <- data_add_col

add_outlier_hist <- hist(unlist(data_add_scores))

add_outlier_table <- table(
  abs(unlist(data_add_scores))>=3
)

############################### [11.5] PCA FOR ADDITIONAL DATASET ############################

############################## [10.5] PCA FOR ADDITIONAL DATASET ############################

pca_threshold <- 0.90
model_ <- preProcess(df3, method = c("pca"), thresh = pca_threshold)
df4 <- predict(model_, df3)

data_add_col_pca <- colnames(df4)
data_add_pca <- bind_cols(data_add[, ..data_add_col_dates], df4)

############################## [11.5.1] HISTOGRAM DENSITY KERNEL AND BOXPLOT #######################

data <- data_add_pca %>%
  pivot_longer(cols = all_of(data_add_col_pca), names_to = 'PC', values_to = 'Value') %>%
  mutate(PC_ = factor(PC, levels = data_add_col_pca))

pca_histogram_density <- ggplot(data = data, aes(x = Value)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="blue") +
  facet_wrap(vars(PC_))

pca_boxplot <- ggplot(data = data, aes(x = Value)) +
  geom_boxplot() +
  stat_boxplot(coef = 1.5, outlier.colour = 'red', outlier.alpha = 0.1) +
  facet_wrap(vars(PC_)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
