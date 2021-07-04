#' explicit namespacing
#' @export
shipsUI <- function(id, shipTypes) {
  tagList(
    dropdown_input(NS(id, "shipType_dropD"), choices = shipTypes,
                   type = "search selection multiple", default_text = "Vessel Type"),
    dropdown_input(NS(id, "shipName_dropD"), choices = shipTypes,
                   type = "clearable selection", default_text = "Vessel Name"),
    div("Ship info: "),
    textOutput(NS(id, "ship_info")),
    actionButton(NS(id, "gl_button"), label = "Feeling lucky"),
    leafletOutput(NS(id, "view_map"), height = 500)
  )
}

#' Dropdown list module server-side processing
#'
#' This module produces items from ship dataset for user to choose from (vessel types and names)
#' @param input Shiny app input object
#' @param output Shiny app output object
#' @param session Shiny app session object 
#' implicit namespacing 
#' @export
shipsServer <- function(id, shipData, shipData.summary) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$shipType_dropD, {
      update_dropdown_input(session, "shipName_dropD",
                            choices = sort(unique(filter(shipData, ship_type %in% 
                                                           input$shipType_dropD)$SHIPNAME)))
    })
    
    observeEvent(input$shipName_dropD, {
      distnc <- 0  # pre-caution
      perShipRecordNum <- shipData.summary %>% filter(SHIPNAME == input$shipName_dropD) %>%
        select(thisShipsLongestMove) %>% as.numeric()
      srcNdest <- shipData %>% 
        filter(SHIPNAME == input$shipName_dropD) %>% 
        filter(row_number() %in% c(perShipRecordNum - 1, perShipRecordNum)) %>%
        select(LON, LAT)
      
      output$view_map <- renderLeaflet({ leaflet() %>% addTiles() %>%
          addMapboxGL(style = "mapbox://styles/mapbox/outdoors-v11") %>%
          setView(lng = 18.6, lat = 54.3, zoom = 3)  # Gdansk, default view
      })
      
      if (nrow(srcNdest) > 1) {
        distnc <- filter(shipData, SHIPNAME == input$shipName_dropD)$METERSFROMLAST[perShipRecordNum] %>% na.omit()
        timeInt<- filter(shipData, SHIPNAME == input$shipName_dropD)$DATETIME[perShipRecordNum] - 
          filter(shipData, SHIPNAME == input$shipName_dropD)$DATETIME[perShipRecordNum - 1]
        output$view_map <- renderLeaflet({ leaflet() %>% addTiles() %>%
            addMapboxGL(style = "mapbox://styles/mapbox/outdoors-v11") %>%
            setView(lat = srcNdest$LAT[2], lng = srcNdest$LON[2], zoom = 5) %>% 
            addAwesomeMarkers(lat = srcNdest$LAT, lng = srcNdest$LON, 
                              icon = awesomeIcons(markerColor = c('gray', 'red')), options = list(opacity = 0.5), 
                              popup = c(paste("Distance (m):", round(distnc, 2)), 
                                        paste(capture.output(na.omit(timeInt)), collapse = ": ")), label = 
                                c(paste('Start:', paste(srcNdest[1,], collapse = ", ")),
                                  paste('End:', paste(srcNdest[2,], collapse = ", ")))[1:2])
        })
      }
      
      output$ship_info <- renderText(c(paste(collapse = ", ", getShipLatestInfo(input[["shipName_dropD"]], shipData)),
                                       paste("Distance (m):", round(distnc, 2))))
    })
    
    observeEvent(input$gl_button, {
      update_dropdown_input(session, "shipName_dropD", choices = unique(shipData$SHIPNAME),
                            value = sample(unique(shipData$SHIPNAME), 1))  
    })    
  })
}

