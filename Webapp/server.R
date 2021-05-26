library(shiny)
library(leaflet)
library(mongolite)
library(dplyr)
library(DT)
library(shinyWidgets)
library(lubridate)


##### Shiny server #####
Shinyserver <- function(input, output, session) {
    
    user <<- "LaurenOconnor" #as.character(Sys.getenv("user"))
    pass <<- "DataMuster" #as.character(Sys.getenv("pass"))
    
    url <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin",
                    'LaurenOconnor', 'DataMuster')
    db <- mongolite::mongo(collection = "RabbitMQLocationTest", db = "DMIoT", url = url, verbose = T)
    df <- db$find()
    timer <- df %>%
        slice(which.max(date))
    timer <<- timer
    datatabledownload <- df[,c(1:3)]
    colnames(datatabledownload) <- c("Timestamp", "Latitude", "Longitude")
    
    
    ##### Modal #####
    sorrymodal <<- modalDialog(
        title = "Sorry!",
        "There is no GPS data available for that date.",
        footer = actionButton("cancel", "Close", style = "color: #6d6e71"), easyClose = TRUE)
    observeEvent(input$cancel, {
        removeModal() 
    })
    
    
    ##### Download as .csv button #####
    output$download <- downloadHandler(
        filename = function() {
            paste0(strftime(Sys.time(), format = "%Y%m%d"), ".csv")
        },
        content = function(file) {
            write.csv(datatabledownload, file, row.names = FALSE)
        }
    )
    
    
    ##### Data table #####
    df <- df[order(df$date, decreasing = TRUE),]
    df$time <- paste(df$date)
    
    output$datatable <- renderDataTable(datatable(tibble(
        Timestamp = df$time,
        Latitude = round(df$Lat, digits = 3),
        Longitude = round(df$Long, digits = 5)),
        rownames = FALSE, 
        options = list(columnDefs = list(list(className = 'dt-center', targets = 0:2)))
        ))
    
    ##### Map #####
    observeEvent(input$datechoice, {
        datetime <<- input$datechoice
        
        latest <- df %>%
            mutate(date = round_date(date, unit = "minute")) %>%
            filter(date == input$datechoice) %>%
            # filter(date == as.POSIXct("2021-05-26 15:16:00")) %>%
            slice_head(n = 1)
        latest <<- latest
        
        if(nrow(latest) == 0){
            showModal(sorrymodal)
            output$map <- renderLeaflet({
                leaflet() %>%
                    setView(lng = 133.416667, lat = -24.25, zoom = 5) %>% 
                    addProviderTiles(providers$OpenStreetMap)
            })
        } else {
            output$map <- renderLeaflet({
                leaflet(latest) %>%
                    addProviderTiles(providers$OpenStreetMap) %>%
                    addCircleMarkers(radius = 6, stroke = FALSE, fillOpacity = 0.7,
                                     fillColor = "#d52a2e",
                                     popup = paste(paste("Latitude:", round(latest$Lat, digits = 5)),
                                                   paste("Longitude:", round(latest$Long, digits = 5)),
                                                   sep = "</br>"))
            })
        }
    })
    
    
    ##### Formatting #####
    ## Sidebar ##
    output$body <- renderUI({
        div(
            tags$head(
                tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Montserrat:700');
          .sidebar {color: #f7f8f8; position: sticky; width: 220px; white-space: nowrap; overflow: visible;}
         # .box {-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}
          .skin-blue .sidebar-menu {color: #808080;}
          .selectize-input {color: #6d6e71;}
          .btn-success.btn {color: #ffc40c; background-color: #fff; border: 2px rgb(255,255,255) solid;}
          .btn-success.btn:hover {color:#ffc40c; background-color: rgb(255,255,255);}
          .btn-success.active {color: rgb(190,30,45); background-color: rgb(255,255,255); border-color:rgb(255,255,255);}
          .btn-file {background-color:red;}
          hr {border-top: 2px solid #FCAF17;}
          .dataTables_wrapper .dataTables_length, 
          .dataTables_wrapper .dataTables_filter, 
          .dataTables_wrapper .dataTables_info, 
          .dataTables_wrapper .dataTables_processing, 
          .dataTables_wrapper .dataTables_paginate {color: #6d6e70 !important;}
          thead {color: #6d6e70 !important;}
          tbody {color: #6d6e70 !important;}
          .dt-button.buttons-copy {color: #6d6e70;}
          .dt-button.buttons-collection {color: #6d6e70;}
          ))),
                        "))),
            fluidRow(leafletOutput("map", height = 950, width = "97%"))
        )
    })
    
    ##### Sidebar #####
    output$sidebar <- renderUI({
        div(width = 200, height = 1, shinyjs::useShinyjs(), id="side-panel",
            fluidRow(column(width = 12, h3("Calf48h data visualisation tool", color = "#6e6d71"), align = "center"), style = "font-size: 100% ; width: 235%",
                     column(width = 12, align = "center", airDatepickerInput("datechoice", label = "Date for visualisation:",
                                                                             addon = 'none', width ='50%',
                                                                             # autoClose = TRUE,
                                                                             dateFormat = "dd M yyyy",
                                                                             value = round_date(timer$date, unit = "minute"),
                                                                             # value = as.POSIXct(timed, format = "%Y-%m-%d %H:%M:%S"),
                                                                             update_on = "close",
                                                                             timepicker = TRUE, timepickerOpts = timepickerOptions(hoursStep = "1",
                                                                                                                                   minutesStep = "1")))),
            fluidRow(column(width = 12, align = "center", downloadButton("download", label = "Download data as csv", style = "color: #6d6e71")), style = "font-size: 100% ; width: 220%"),
            fluidRow(column(width = 12, div(style = "height:20px"))),
            fluidRow(column(width = 12, div(DTOutput("datatable", height = 480), style = "font-size: 100% ; width: 220%"))),
            fluidRow(column(width = 12, div(style = "height:20px"))),
            fluidRow(column(width = 12, align = "center", tags$a(tags$img(src = "logos.png", height = 120, width = 430))), style = "font-size: 100% ; width: 235%")
            )
    })
    
    
    ##### Body #####
    # output$body <- renderUI({
    #     div(
    #         tags$head(
    #             tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Montserrat:700');
    #       .sidebar {color: #f7f8f8; position: sticky; width: 220px; white-space: nowrap; overflow: visible;}
    #      # .box {-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}
    #       .skin-blue .sidebar-menu {color: #808080;}
    #       .selectize-input {color: #6d6e71;}
    #       .btn-success.btn {color: #ffc40c; background-color: #fff; border: 2px rgb(255,255,255) solid;}
    #       .btn-success.btn:hover {color:#ffc40c; background-color: rgb(255,255,255);}
    #       .btn-success.active {color: rgb(190,30,45); background-color: rgb(255,255,255); border-color:rgb(255,255,255);}
    #       .btn-file {background-color:red;}
    #       hr {border-top: 2px solid #FCAF17;}
    #       .dataTables_wrapper .dataTables_length, 
    #       .dataTables_wrapper .dataTables_filter, 
    #       .dataTables_wrapper .dataTables_info, 
    #       .dataTables_wrapper .dataTables_processing, 
    #       .dataTables_wrapper .dataTables_paginate {color: #6d6e70 !important;}
    #       thead {color: #6d6e70 !important;}
    #       tbody {color: #6d6e70 !important;}
    #       .dt-button.buttons-copy {color: #6d6e70;}
    #       .dt-button.buttons-collection {color: #6d6e70;}
    #       ))),
    #                     "))),
    #         fluidRow(leafletOutput("map", height = 450, width = "97%")),
    #         fluidRow(wellPanel(height = 540,
    #                           fluidRow(column(width = 2, downloadButton("download", label = "Download as .csv", style = "color: #6d6e71"))),
    #                           fluidRow(column(width = 12, div(style = "height:10px"))),
    #                           fluidRow(column(width = 12, div(DTOutput("datatable", height = 480), style = "font-size: 100% ; width: 99%")))
    #         ))
    #         )
    # })
    
    
}