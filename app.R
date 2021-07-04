library(shiny)
library(shiny.semantic)
library(modules)

library(DT)  
library(readr)
library(dplyr)
library(magrittr)
library(data.table)
library(vroom)
library(lubridate)

library(leaflet.mapboxgl)
library(leaflet)
options(mapbox.accessToken = NA)

library(ggplot2)
library(plotly)

source('R/modules.r')
source('R/compute_helper.r')
shipData <- vroom('extdata/ships_select_cols.csv')
#shipData <- as_tibble(fread('extdata/ships.csv', integer64 = "double"))
#library(bit64)
#shipData <- read_csv('extdata/ships.csv')

# data processing
shipData %<>% mutate(DATETIME = lubridate::as_datetime(DATETIME)) %>% 
    arrange(DATETIME) %>% group_by(SHIP_ID) %>%
    group_modify(~mutate(., METERSFROMLAST = calcDistances(select(., LAT, LON))))

shipData.summary <- shipData %>% 
    group_by(SHIPNAME) %>% 
    summarize(thisShipsLongestMove = last(
        which(METERSFROMLAST == max(METERSFROMLAST, na.rm = TRUE))))

shipTypes <- sort(unique(shipData$ship_type))  # improve naming conventions

shipsApp <- function(shipData, shipData.summary, shipTypes) {
    ui <- semanticPage(
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")),
        title = "Shiniest ships",
        shipsUI("shipsInfo", shipTypes),
        calendar("date", type = "date", placeholder = "Select", 
                 min = min(shipData$date), max = max(shipData$date)),
        plotlyOutput("plot"),
        semantic_DTOutput("table")
        
    )
    
    server <- function(input, output, session) {
        shipsServer("shipsInfo", shipData, shipData.summary)
        output$table <- DT::renderDataTable(
           semantic_DT(rbind(shipData[sample(1:nrow(shipData), 20),],
                             tail(shipData[shipData$date == input$date,]),
                             tail(shipData)))  # just experimenting
        )
        output$plot <- renderPlotly({
            plotly::ggplotly(ggplot(shipData[shipData$date == input$date,]) +
                                 geom_bar(aes(x = FLAG)))
        })
    }
    shinyApp(ui, server)
}

shipsApp(shipData, shipData.summary, shipTypes)
