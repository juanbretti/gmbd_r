
################################ [0] LIBRARIES #####################################

library(dplyr)
library(lubridate)
library(data.table)
library(mice)
library(ggplot2)
library(purrr)
library(tidyverse)
library(ggmap)
library(PerformanceAnalytics)
library(skimr)
library(corrplot)
library(doParallel);
library(foreach)
library(DataExplorer)
library(forecast)
library(leaflet)
library(leaflet.extras)
library(caret)

################################ [1] FILE PATH #####################################

#testing

file_path <- 'C:/Users/pleaz/Desktop/IE/Term 1/R/Group Assignment/'

# Real values of solar production recorded in 98 different weather stations (rows defined below) 
# (each row coresponds to a date between 1994-01-01 and 2012-11-30)
# After column 99, values are missing or NA and need to be predicted
dat_solar <- readRDS(file = file.path(file_path,'solar_dataset.RData'))
# Name, latitude, longitude and elevation of each of the 98 stations
dat_station <- fread(file = file.path(file_path, 'station_info.csv'))
#additional data with Numerical Weather Prediction (NWP) values corresponding to a particular day
dat_add <- readRDS(file = file.path(file_path, 'additional_variables.RData'))

#assign date fromat
dat_solar <- dat_solar[j = Date2 := as.Date(x = Date, format = "%Y%m%d")]
dat_add <- dat_add[j = Date2 := as.Date(x = Date, format = "%Y%m%d")]

# Divide into the columns defined by the assignment

# solar production values known (also Training?)
dat_solar_target <- dat_solar[1:5113,2:99]

# solar production variables created from different weather predictors resulting from PCA over the original Data

dat_solar_predictors <- dat_solar[1:5113,100:456]

################################ JPBM #####################################

data_solar <- readRDS(file = file.path('data', 'solar_dataset.RData'))
data_station <- fread(file = file.path('data', 'station_info.csv'))
data_add <- readRDS(file = file.path('data', 'additional_variables.RData'))

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

################################ GVA #####################################

folder_path <- "/Users/gva/Documents/01 Pers/02 Loopbaan/01 MBD/02 Subjects/01 Statistical Prog R/03 Opdragte/03 GA01/r_shiny/files";

# get data files
read_data <- function(folder_path){
  dat_solar <- readRDS(file.path(folder_path, "solar_dataset.RData"));
  dat_variables <- readRDS(file.path(folder_path, "additional_variables.RData"));
  dat_station <- fread(file.path(folder_path, "station_info.csv"))};

fp_solar_data <- "/Users/gva/Documents/01 Pers/02 Loopbaan/01 MBD/02 Subjects/01 Statistical Prog R/03 Opdragte/03 GA01/r_shiny/files";
fp_local <- "/Users/gva/Documents/01 Pers/02 Loopbaan/01 MBD/02 Subjects/01 Statistical Prog R/03 Opdragte/03 GA01/r_shiny"
source(file.path(fp_local,"data_prep.R"))
list_dat <- read_data(fp_solar_data);

fp_geo_func <- "/Users/gva/Documents/01 Pers/05 Development/02 R/03 Projekte/01 GeoMaps"

################################ LI #####################################

ds <- as.data.table(readRDS("C:/Users/pleaz/Desktop/IE/Term 1/R/Group Assignment/solar_dataset.RData"))

da <- as.data.table(readRDS("c:/Users/pleaz/Desktop/IE/Term 1/R/Group Assignment/additional_variables.RData"))

################################ [1.1] LI: PREPARE TRAINING SET #####################################

ds$Date <- as.Date(strptime(ds$Date,"%Y%m%d"), format="%Y-%m-%d")
ds <- ds[Date<= "2007-12-31"]

ds_train <- cbind(ds[,1],ds[,2],ds[,100:length(colnames(ds))]) # example of one station

################################ [1.2] GVA: UNIQUE VALUES & CONSTANT VARIABLES #####################################

# Returns the unique value of each column of x
check_unique <- function(x){
  return(length(unique(x)));
}

check_duplicates <- function(){
  
}

# in_dat <- dat_station;

check_constants <- function(in_dat, criteria = 1){
  
  # Call function for each column
  sapply(in_dat, check_unique); # company is constant!!
  
}

# Function to detect constant variables within a given criteria 
criteria_variables <- function(in_dat, criteria = 1){
  
  n_unique_values <- sapply(in_dat, check_unique);
  criteria_variables <- names(n_unique_values)[n_unique_values == criteria];
  return(criteria_variables);
}


remove_constants <- function(in_dat, criteria){
  
  # Function call to detect constant variables in_dat within criteria
  constant_var <- criteria_variables(in_dat, criteria);
  # constant_var;
  
  # Remove these variables from the dataset
  # ncol(in_dat);
  in_dat <- in_dat[, setdiff(colnames(in_dat), constant_var)];
  # ncol(in_dat);
  # colnames(in_dat);
  return(in_dat)
}

################################ [1.3] GVA MOST FREQUENT VALUES #####################################

top_n_frequencies <- function(x, n = 5){
  return(names(sort(table(x), decreasing = TRUE)[1:n]));
}

################################ [2] COMPUTE STATISTICS OF EACH COLUMN #####################################

skim(dat_solar_target)

skim(dat_solar_predictors)

skim(dat_station)

skim(dat_add)

################################ [3] FIND AND REPLACE MISSING VALUES #####################################

# Missing values

list_all_df <- list(dat_solar_target, dat_solar_predictors, dat_station, dat_add)

check_missing <- function(x) {
  count_na <- sum(is.na(x))
  # print(names(list_all_df))
  return(count_na)
}

sapply(list_all_df,check_missing)

#only dat_add has missing values

#Replace Missing values with median value

fill_missing_with_median <- function(x){ 
  
  x <- as.numeric(x);   
  
  x[is.na(x)] <- median(x, na.rm = TRUE);   
  
  return(x);
  
}

sapply(dat_add,function(x){sum(is.na(x))})

dat_add<- as.data.frame(sapply(dat_add, fill_missing_with_median));

sum(is.na(dat_add));

################################ [3.1] LI: MISSING VALUES & REDUNDANT VARIABLES #####################################

cv <- sapply(ds_train, function(x){length(unique(x))});
constant_columns <- names(cv)[cv ==1]
constant_columns

# CALCULATING CORRELATION BETWEEN PREDICTORS

correlation_matrix <- abs(cor(ds_train[,3:ncol(ds_train)]))
hc <- findCorrelation(correlation_matrix,cutoff = 0.75)
hc

# FINDING MISSING VALUES FOR ADDITIONAL DATA

missing_value <- sapply(da, function(x){(sum(is.na(x))/length(x))*100});
sort(missing_value)
columns_with_missing <- names(missing_value)[missing_value > 0]

# The highest percentage of missing values is 7%, we can impute missing values using mice

library("mice")
imputed_d <- mice(da, m=1)
da <- complete(imputed_d,1)

################################ [3.2] JPBM: NA CHECK #####################################

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

################################ [3.3] JPBM: USE 'MICE' TO COMLETE 'ADDITIONAL DATA' VALUES #####################################

data <- data_add[, ..data_add_col]

m_ <- 5
# df2 <- mice(data, m=m_, maxit=10, meth='pmm', seed=500)
# saveRDS(df2, file.path('storage', 'data_add_mice.rds'))
df2 <- readRDS(file.path('storage', 'data_add_mice.rds'))
# summary(df2)

# Average of all the Multivariate Imputation
df3 <- 0
for (i in 1:m_) df3 <- df3 + complete(df2, i)
df3 <- df3/m_

#PCA 

pca_threshold <- 0.90
model_ <- preProcess(df3, method = c("pca"), thresh = pca_threshold)
df4 <- predict(model_, df3)

data_add_col_pca <- colnames(df4)
data_add_pca <- bind_cols(data_add[, ..data_add_col_dates], df4)

#HISTOGRAM DENSITY KERNEL AND BOXPLOT

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

################################ [4] COMPUTE CORRELATIONS #####################################

corela_target_pred <- as.data.frame(foreach(1, combine = cbind) %do% cor(dat_solar_target, dat_solar_predictors))

corela_target_pred;

################################ [4.1] LI: COMPUTE CORRELATIONS #####################################

correlation_matrix <- abs(cor(da[,2:ncol(da)]))
hc <- findCorrelation(correlation_matrix,cutoff = 0.9)
redundant_cols <- colnames(da[,hc+1])
length(redundant_cols)

# plot informative variables

da_in <- da[,-(hc+1)]
plot(x=da_in$Date,y=da_in$V2089, type='l')

################################ [4.2] JPBM: COMPUTE CORRELATIONS BETWEEN TOP 5 WEATHER STATIONS (IN TERMS OF PRODUCTION) & TOP 10 PREDICTORS #####################################

top_ <- 5
top_pc <- 10

data <- data_solar_train %>% 
  dplyr::select(principal_weather_station[1:top_], data_solar_col_predi[1:top_pc])

chart.Correlation(data, histogram=TRUE) #, pch=19

################################ [4.3] JPBM: VARIABLE IMPORTANCE #####################################

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

# saveRDS(data_solar_importance, file.path('storage', 'data_solar_importance_parallel.rds'))
data_solar_importance <- readRDS(file.path('storage', 'data_solar_importance_parallel.rds'))

names(data_solar_importance) <- principal_weather_station[1:top_]

################################ [5] OUTLIER DETECTION / REMOVAL / CORRECTION #####################################
# #Outlier detection/removal/correction.
# install.packages('outliers')
# library(outliers)
# 
# #visualize time series
# 
# dat %>% 
#   filter(Date > '2012-01-01') %>%
#     plot(Date ~ ACME, data = dat, type = '0');
# 
# boxplot(dat$ACME)
# hist(dat$ACME, breaks = 6)
# grubbs.test(dat$ACME) #since p-value closer to 1 - evidence for outlier is small
# 
# ##mod <- lm(dat~ACME, data=as.data.frame(dat))
# #cooksd <- cooks.distance(mod)
# 
# # plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
# # abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
# # text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

################################ [6] DATA SCALING #####################################

tipify <- function(x){   
  
  mu <- mean(x, na.rm = TRUE);   
  
  s <- sd(x, na.rm = TRUE);       
  
  # Special condition for constant variables   
  
  s[s == 0] <- 1;       
  
  # Tipify   
  
  x <- (x - mu) / s;
  
}

sapply(dat, mean, na.rm = TRUE);
sapply(dat, sd, na.rm = TRUE);

dat <- as.data.frame(sapply(dat, tipify));


################################ [7] JPBM:  PLOTS FOR EACH WEATHER STATION  #####################################

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

################################ [7.1] JPBM:  RANK (IN TERMS OF PRODUCTION) CHANGE OVER TIME  #####################################

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


################################ [7.2] JPBM:  OVERALL PRODUCTION OVER TIME  #####################################

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

################################ [7.3] JPBM: DEVELOPMENT OF PRODUCTION VOLUME OVER TIME FOR THE 5 HIGHEST PRODUCING WEATHER STATIONS  #####################################

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


################################ [7.4] JPBM: SEASONABILITY - HOW DOES THE PRODUCTION OF ALL WEATHER STATIONS CHANGE OVER TIME AND HOW MUCH OF THE PRODUCTION IS EXPLAINED / TREND / RANDOM  #####################################

data <- data_solar_train %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  group_by(Date2) %>% 
  summarise(ValueMean = mean(as.numeric(Value)))

# range(data$Date2)
data2 <- ts(data = data$ValueMean/1e6, frequency = 365, start = c(1994, 1, 01), end = c(2007, 12, 31))
plot(decompose(data2))

################################ [7.5] JPBM: TREND VISUALIZATION FOR THE TOP 5 WEATHEER STATIONS  #####################################

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

################################ [7.6] VISUALIZATION OF COLUMN VALUES AND DISTRIBUTION FOR ACME STATION #####################################

dat_solar_target$ACME

ggplot(dat_solar_target, aes(x=ACME, y = dat_solar$Date[1:5113], color=dat_solar_target$ACME)) + geom_boxplot(width=0.1) + theme_bw() + coord_flip()

#histogram
hist(dat_solar_target$ACME, breaks = 10)

#stip plot
ggplot(dat_solar_target, aes(x=ACME, y=dat_solar$Date[1:5113], color=dat_solar_target$ACME, alpha=0.8)) + geom_jitter(position=position_jitter(0.3)) + theme_bw()

# violin plot with inseet boxplot
# ggplot(dat_solar_target, aes(x=ACME, y=dat_solar$Date[1:5113], color=dat_solar_target$ACME, alpha=0.8)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1) + theme_bw();

################################ [7.7] JPBM: HISTOGRAM, DENSITY KERNEL AND BOXPLOT FOR GENERAL PRODUCTION #####################################

data <- data_solar_train %>% 
  dplyr::select(data_solar_col_produ) %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value')

## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(data_solar_col_produ)` instead of `data_solar_col_produ` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.

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


################################ [7.8] JPBM: HISTOGRAM AND BOXPLOT FOR EACH WEATHER STATION AND PREDICTORS #####################################


# WEATHER STATION

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

#PREDICTORS

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

################################ [7.9] JPBM: DISTRIBUTION OF ADDITIONAL DATASET VALUES #####################################

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

################################ [7.10] LI: VISUALIZATION OF SOLAR PRODUCTION COLUMN, VARIABLE DISTRIBUTION, TIME SERIES #####################################

plot(x=ds_train$Date, y=ds_train$ACME, type="l")

plot(density(da$V6409))
plot(density(da$V3529))

timeseries
da$Date <- as.Date(strptime(da$Date,"%Y%m%d"), format="%Y-%m-%d")
plot(x=da$Date, y=da$V6409, type='l')

################################ [8] VISUALIZATION OF CORRELATIONS #####################################

# Correlation between target values

corela_target <- cor(dat_solar_target)

corrplot(corela_target, method = 'color')

#Correlation between target values and predictors (example target values from "ACME")

corela_visu <- cor(x = dat_solar_target$ACME, y = dat_solar_predictors)

corrplot(corela_visu, method = "color", tl.cex= 0.5)

################################ [8.1] LI: VISUALIZATION OF CORRELATIONS #####################################

library("corrplot")
corrplot(cor(ds_train[,2:15]))

################################ [8.2] JPBM: CORRELATION BETWEEN ADDITIONAL INFORMATION #####################################

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

################################ [9] VISUALIZATION ON MAPS #####################################

#Visualization on a map using leaflet or similar. (https://rstudio.github.io/leaflet/)

################################ [9.1] VISUALIZATION OF STATIONS #####################################

station_map <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng=dat_station$elon, lat=dat_station$nlat, popup = dat_station$stid)


station_map;


################################ [9.2] JPBM: POSITIONS #####################################

data <- data_solar_train %>% 
  select(-data_solar_col_predi) %>% 
  pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
  group_by(WeatherStation) %>%
  summarise(ValueMean = mean(Value)) %>% 
  left_join(data_station, by = c('WeatherStation' = 'stid'))

## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(data_solar_col_predi)` instead of `data_solar_col_predi` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.

m1 <- leaflet(data = data) %>%
  addTiles() %>%
  addMarkers(lng=~elon, lat=~nlat,
             popup = ~paste(round(ValueMean/1e6, 0), "Million"), label = ~WeatherStation,
             clusterOptions = markerClusterOptions())

m1

################################ [9.3] JPBM: SEASONAL DECOMPOSITION #####################################

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


################################ [9.4] GVA:  GEOMETRICS #####################################

#Returns the geometrics for the group and for the MAIN map for station_geo
get_stat_geom <- function(dat_station_geo){
  
  #plt_lat = 35.57, plt_long = -97.22
  df <- data.frame(stid = c("MAIN"), lon = c(-97.22), lat = c(35.5), elev = c(0.0)); # The USA information
  sol_stat <- as.data.frame(dat_station_geo);
  #sol_stat <- as.data.frame(list_dat$dat_station);
  sol_stat$lon <- sol_stat$elon;
  sol_stat$lat <- sol_stat$nlat;
  sol_stat$elon <- NULL;
  sol_stat$nlat <- NULL;
  sol_stat <- bind_rows(union(sol_stat, df));
  #merged_map <- merge(world_map, list_dat$dat_station, by.x = "long", by.y = "elon")
  return(sol_stat);
}

################################ [9.5] GVA: CAPTURE LOCATION DATA & PLOT TERRAIN  #####################################

fp_geo_func <- "/Users/gva/Documents/01 Pers/05 Development/02 R/03 Projekte/01 GeoMaps"

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
from_to_adrr_gps <- function (df){
  geocoded <- data.frame(stringsAsFactors = FALSE);
  #df <- dat_act_cust;
  df$lon <- 0;
  df$lat <- 0;
  df$geoAddress <- "";
  str(df)
  
  for(i in 1:nrow(df))
  {
    result <- geocode(df$`Bill to`[i], output = "latlona", source = "google")
    if (!is.na(result)){
      df$lon[i] <- as.numeric(result[1])
      df$lat[i] <- as.numeric(result[2])
      df$geoAddress[i] <- as.character(result[3])
      
    } else {
      df$lon[i] <- 0
      df$lat[i] <- 0
      df$geoAddress[i] <- "error"
    }
    
    
  }
  return (df)
}

##Give a plot of a terrain map with a layer of data ontop
plot_map <- function(plt_clong, plt_clat, plt_long, plt_lat, plt_zoom, plt_data, plt_size, plt_col = "red"){
  p <- ggmap(get_googlemap(center = c(lon = plt_clong, lat = plt_clat),
                           zoom = plt_zoom, scale = 2,
                           maptype ='terrain',
                           color = 'color'));
  p + geom_point(aes(x = plt_data$lon, y = plt_data$lat, color = plt_col), data = plt_data, size = plt_size) + 
    theme(legend.position="bottom");
  
  #return(p)
}

#Anything that comes to your mind and makes sense. Creativity will be rewarded.