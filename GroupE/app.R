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
library(PerformanceAnalytics)
library(corrplot)

# Mapping
library(leaflet)
library(leaflet.extras)
library(leafpop)
library(gdtools)

# Spatial
library(sf)
library(sp)
library(rgdal)
library(rgeos)

## Load data ----

data_solar <- readRDS(file = file.path('data', 'solar_dataset.RData'))
data_station <- fread(file = file.path('data', 'station_info.csv'))

## Transform data ----

# Source dataset ----
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

# Positions for map ----
data_position <- data_solar_train %>% 
    select(-all_of(data_solar_col_predi)) %>% 
    pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
    group_by(WeatherStation) %>%
    summarise(ValueMean = mean(Value)) %>% 
    left_join(data_station, by = c('WeatherStation' = 'stid'))

# Plots for map ----
# https://www.datanovia.com/en/lessons/combine-multiple-ggplots-into-a-figure/
weatherstation_plot <- function(data){
    p_all <- data %>%
        ggplot() +
        geom_smooth(aes(x = Date2, y = Value/1e6)) +
        labs(x = 'Date', y = 'Production in million')
    
    p_year <- data %>%
        ggplot(aes(x = Year, y = Value/1e6, group = Year)) +
        geom_boxplot() +
        labs(x = 'Year', y = 'Production in million') +
        ggpubr::rotate_x_text()

    p_month <- data %>%
        ggplot(aes(x = Month, y = Value/1e6)) +
        geom_boxplot() +
        labs(x = 'Month', y = '') +
        ggpubr::rotate_x_text()

    p_Day_Of_Week <- data %>%
        ggplot(aes(x = Day_Of_Week, y = Value/1e6, fill = Weekend)) +
        geom_boxplot() +
        labs(x = 'Day of the week', y = '') +
        theme(legend.position = "none") +
        scale_fill_manual(values=c("gray80", "white")) +
        ggpubr::rotate_x_text()

    plot <- ggarrange(
        p_all,
        ggarrange(p_year, p_month, p_Day_Of_Week, ncol = 3),
        nrow = 2
    )
    plot <- annotate_figure(plot, top = text_grob(unique(data$WeatherStation), face = "bold", size = 14))
    
    return(plot)
}

# Create all the plots, takes some time ----
# data_plot <- data_solar_train %>%
#     dplyr::select(all_of(c(data_solar_col_dates, data_solar_col_produ))) %>%
#     pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>%
#     # filter(WeatherStation %in% data_solar_col_produ[1:2]) %>%
#     group_by(WeatherStation) %>%
#     do(
#         plots = weatherstation_plot(.)
#     )
# data_plot[[2]]

# Save and load the plots, to improve speed of App starting
# saveRDS(data_plot, file.path('GroupE', 'storage', 'data_plot.rds'))
data_plot <- readRDS(file.path('storage', 'data_plot.rds'))

# Creating the spatial dataset ----
#CRS
CRSLatLon<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ") #"+init=epsg:4326"
#http://spatialreference.org/ref/sr-org/7483/, WGS84 Web Mercator (Auxiliary Sphere) (Google, Spotfire)
CRSProj<-CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")

# Create the SpatialPointsDataFrame
WeatherStation_point<-SpatialPointsDataFrame(coords = data_station[, c('elon', 'nlat')], data = data_station, proj4string=CRSLatLon) %>% 
    spTransform(CRSProj)

# Calculate the closest weather stations
ws_distance <- function(spdf, ws, distance) {
    # Add distance to 'HOOK'
    spdf@data$Distance <- as.numeric(gDistance(spgeom1 = subset(spdf, stid == ws), spgeom2 = spdf, byid=TRUE))
    # List of neighbors
    ws_neighbors_spdf <- subset(spdf, Distance != 0 & Distance <= distance)
    # Replicate the source of point
    ws_origin <- subset(spdf, stid == ws)
    ws_origin <- as.data.frame(ws_origin@coords)
    ws_origin <- bind_rows(replicate(length(ws_neighbors_spdf), ws_origin, simplify = FALSE))
    # Coords from the neighbors
    ws_destination <- ws_neighbors_spdf@coords
    # https://stackoverflow.com/questions/29287237/connect-xy-points-with-spatial-lines
    # Number of rows
    ws_number <- length(ws_neighbors_spdf)
    # If I have neighbors
    if (ws_number>0) {
        # Creation of the lines
        ws_lines <- vector("list", ws_number)
        for (i in 1:ws_number) {
            ws_lines[[i]] <- Lines(list(Line(rbind(ws_origin[i, ], ws_destination[i,]))), as.character(i))
        }
        ws_lines <- SpatialLines(ws_lines, proj4string = CRSProj) %>% 
            spTransform(CRSLatLon)
        # List of neighbors
        neig_stid <- ws_neighbors_spdf@data$stid
        neig_data <- ws_neighbors_spdf@data
    } else {
        ws_lines <- neig_stid <- neig_data <- NULL
    }
    
    return(list(
        neig_number = ws_number,
        lines = ws_lines,
        neig_stid = neig_stid,
        neig_data = neig_data
    ))
}

## App ----

## UI ----
# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("Weather stations", id = "nav",
    tabPanel("Map",
            tags$head(includeCSS("styles.css")),
            div(class = "outer", 
            leafletOutput("map", width = "100%", height = "50%"),
            absolutePanel(id = "controls", class = "panel panel-default", 
                          top = 60, right = 20, width = 330, fixed = TRUE, draggable = TRUE, bottom = "auto", height = "auto", left = "auto",
                          sliderInput(inputId = "distance", label = "Distance [km]", min = 10, max = 500, value = 100, step = 10, ticks = FALSE)),
            DT::dataTableOutput("neighbors_table"),
            tags$div(id="cite", 'IE, GMBD, Intake 2020, Group E')
        )
    ),
    tabPanel("Data")
))


## Server ----
# Create the server
server <- function( input, output, session ){
    
    # First draw
    output$map <- renderLeaflet({
        leaflet(data = data_position) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addCircleMarkers(
                lng=~elon, lat=~nlat,
                radius = ~ValueMean/1e6, 
                label = ~WeatherStation,
                layerId = ~WeatherStation,
                group = 'data_solar') %>% 
            addHeatmap(
                lng = ~elon, lat = ~nlat,
                intensity = ~ValueMean,
                layerId = 'Heat',
                blur = 90, max = 1, radius = 60, minOpacity = 0.5) %>% 
            leafpop::addPopupGraphs(
                data_plot$plots,
                group = 'data_solar', #Has to be the same group as 'addCircleMarkers'
                width = 500, height = 400)
    }) 

    # Check events over the map
    observeEvent(c(input$map_marker_click, input$distance), ignoreNULL = FALSE, ignoreInit = TRUE, {
        map_ <- input$map_marker_click
        dist_ <- input$distance
        # If there is any input
        if (!is.null(map_) & !is.null(dist_)) {
            # Calculate neighbors
            ws_ <- ws_distance(WeatherStation_point, map_$id, dist_*1e3)
            # Plot the lines, only if there are neighbors
            if (ws_$neig_number>0) {
                leaflet::leafletProxy(mapId = "map") %>%
                    clearGroup('lines') %>% 
                    clearGroup('DT_selected') %>% 
                    addPolygons(
                        data = ws_$lines,
                        opacity = 0.5,
                        group = 'lines')
                # Table with the list of neighbors
                output$neighbors_table <- DT::renderDataTable(ws_$neig_data, rownames = FALSE, width = 0.9)
            }
        }
    })
    
    # Check events over the Data Table
    observeEvent(input$neighbors_table_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
        map_ <- input$map_marker_click
        dist_ <- input$distance
        row_ <- input$neighbors_table_rows_selected
        if (!is.null(row_)) {
            ws_ <- ws_distance(WeatherStation_point, map_$id, dist_*1e3)
            leaflet::leafletProxy(mapId = "map") %>%
                clearGroup('DT_selected') %>% 
                addCircleMarkers(
                    lng=ws_$neig_data$elon[row_], lat=ws_$neig_data$nlat[row_],
                    label = ws_$neig_data$stid[row_],
                    color = 'red',
                    group = 'DT_selected')
        } else {
            leaflet::leafletProxy(mapId = "map") %>%
                clearGroup('DT_selected')
        }
    })
} 

# Run the application 
shinyApp(ui = ui, server = server)