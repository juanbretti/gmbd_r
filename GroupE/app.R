## Libraries ----

# General purpose
library(tidyverse)
library(data.table)
library(lubridate)

# Shiny
library(shiny)

# Visualization
library(ggplot2)
library(ggpubr)
library(ggpubr)
library(PerformanceAnalytics)
library(corrplot)

# Mapping
library(leaflet)
library(leaflet.extras)
library(leafpop)
library(sf)
library(gdtools)

## Load data ----

data_solar <- readRDS(file = file.path('data', 'solar_dataset.RData'))
data_station <- fread(file = file.path('data', 'station_info.csv'))

## Transform data ----

# Source dataset
data_solar <- data_solar[j = Date2 := as.Date(x = Date, format = "%Y%m%d")]

# Add date conversions
data_solar <- data_solar %>% 
    mutate(Year = year(Date2),
           Month = month(Date2, label = TRUE),
           Day = day(Date2),
           Day_Of_Year = yday(Date2),
           Day_Of_Week = wday(Date2, label = TRUE, week_start = 1),
           Weekend = ifelse(Day_Of_Week %in% c('Sat', 'Sun'), 'Weekend', 'Workday'),
           Days_Since_Origin = time_length(interval(origin, Date2), unit = 'day')) %>% 
    as.data.table(.)

# Columns defined from the enunciate
data_solar_col_produ <- colnames(data_solar)[2:99]
data_solar_col_predi <- colnames(data_solar)[100:456]
data_solar_col_dates <- setdiff(colnames(data_solar), c(data_solar_col_produ, data_solar_col_predi))

# Split train and test set
data_solar_train <- data_solar[i = 1:5113]
# data_solar_test <- data_solar[i = 5114:nrow(data_solar), j = .SD, .SDcols = c(data_solar_col_dates, data_solar_col_predi)]

## Transform data ----

# Position for map
data_position <- data_solar_train %>% 
    select(-all_of(data_solar_col_predi)) %>% 
    pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
    group_by(WeatherStation) %>%
    summarise(ValueMean = mean(Value)) %>% 
    left_join(data_station, by = c('WeatherStation' = 'stid'))

# Plots for map
# https://www.datanovia.com/en/lessons/combine-multiple-ggplots-into-a-figure/
weatherstation_plot <- function(data){
    p_all <- data %>%
        ggplot() +
        geom_smooth(aes(x = Date2, y = Value/1e6)) +
        labs(x = 'Date', y = 'Production in million')
    
    p_year <- data %>%
        ggplot(aes(x = Year, y = Value/1e6, group = Year)) +
        geom_boxplot() +
        labs(x = 'Year', y = 'Production in million')

    p_month <- data %>%
        ggplot(aes(x = Month, y = Value/1e6)) +
        geom_boxplot() +
        labs(x = 'Month', y = '')

    p_Day_Of_Week <- data %>%
        ggplot(aes(x = Day_Of_Week, y = Value/1e6)) +
        geom_boxplot() +
        labs(x = 'Day of the week', y = '')

    plot <- ggarrange(
        p_all,
        ggarrange(p_year, p_month, p_Day_Of_Week, ncol = 3),
        nrow = 2
    )
    
    return(plot)
}

# Create all the plots, takes some time
# data_plot <- data_solar_train %>%
#     dplyr::select(all_of(c(data_solar_col_dates, data_solar_col_produ))) %>%
#     pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>%
#     # filter(WeatherStation %in% principal_weather_station[1:top_]) %>%
#     group_by(WeatherStation) %>%
#     do(
#         plots = weatherstation_plot(.)
#     )

# Save and load the plots, to improve speed of App starting
# saveRDS(data_plot, file.path('GroupE', 'storage', 'data_plot.rds'))
# data_plot <- readRDS(file.path('storage', 'data_plot.rds'))

## App ----

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("Weather stations", id = "nav",
    tabPanel("Map",
        div(class = "outer", tags$head(includeCSS("styles.css")),
            leafletOutput("map", width = "100%", height = "100%")
        )
    ),
    tabPanel("Data")
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$map <- renderLeaflet({
        leaflet(data = data_position) %>%
            addTiles() %>%
            addCircleMarkers(
                lng=~elon, lat=~nlat,
                radius = ~ValueMean/1e6, 
                label = ~WeatherStation,
                group = 'data_solar') %>% 
            addHeatmap(
                lng = ~elon, lat = ~nlat,
                intensity = ~ValueMean,
                blur = 90, max = 1, radius = 60, minOpacity = 0.5)
            # leafpop::addPopupGraphs(
            #     data_plot$plots,
            #     group = 'data_solar',
            #     width = 300, height = 400)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)