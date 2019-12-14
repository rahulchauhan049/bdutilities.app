# Module UI

#' @title   mod_add_data_ui and mod_add_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_add_data
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList
#' @importFrom leaflet leafletOutput
mod_add_data_ui <- function(id, label = "Add Occurrence Data"){
    ns <- NS(id)
    tagList(
        column(
            12,
            h1("Add Occurrence Data"),
            column(
                3,
                class = "upload_side",
                # ------------- DB Module -------------------
                tabsetPanel(
                    type = "tabs",
                    tabPanel(
                        "Download Data",
                        div(class = "secondaryHeaders", h3("Option 01: From Online Database")),
                        textInput(
                            ns("scientificName"),
                            label = h3("Scientific Name:"),
                            value = "Puma concolor"
                        ),
                        
                        numericInput(
                            ns("recordSize"),
                            label = h3("Record Size:"),
                            value = 500
                        ),
                        
                        selectInput(
                            ns("hasCoords"),
                            label = h3("Records Filter:"),
                            choices = list(
                                "With Coordinates" = "1",
                                "Without Coordinates" = "2",
                                "No Filter" = "3"
                            ),
                            selected = 3
                        ),
                        
                        radioButtons(
                            ns("queryDB"),
                            label = h3("Online Database:"),
                            choices = list(
                                "GBIF (Global Biodiversity Information Facility)" = "gbif",
                                "iDigBio (Integrated Digitized Biocollections)" = "idigbio",
                                "EcoEngine (Berkeley Ecoinformatics Engine)" = "ecoengine",
                                "Vertnet (Vertebrate Network)" = "vertnet",
                                "BISON (Biodiversity Information Serving Our Nation)" = "bison",
                                "iNaturalist" = "inat",
                                "ALA (Atlas of Living Australia)" = "ala"
                                # "OBIS (Ocean Biogeographic Information System)" = "obis",
                                # "AntWeb" = "antweb"
                            ),
                            selected = "gbif"
                        ),
                        
                        
                        div(
                            id = ns("queryDatabaseDiv"),
                            class = "activeButton",
                            actionButton(ns("queryDatabase"), "Query Database", icon("download"))
                        )
                    ),
                    
                    # ------------- End of DB Module -------------------
                    
                    # ------------- Local Disk Module -------------------
                    tabPanel(
                        "Upload Data",
                        div(class = "secondaryHeaders", h3("Option 02: From Local Disk")),
                        div(
                            id = ns("inputFileDiv"),
                            class = "activeButton",
                            fileInput(
                                ns("inputFile"),
                                label = h3("CSV / DWCA ZIP / R RDS file input"),
                                accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv",
                                    ".zip",
                                    "application/zip",
                                    ".rds",
                                    ".RDS"
                                )
                            )
                        )
                    )
                    
                    # ------------- End of Local Disk Module -------------------
                    
                    
                )
            ),
            
            # ------------- Map / Table Module -------------------
            column(9,
                   class = "upload_main",
                   tabsetPanel(
                       type = "tabs",
                       tabPanel(
                           "Map View",
                           leafletOutput(ns("mymap"), height = "700"),
                           absolutePanel(
                               top = 60,
                               right = 20,
                               selectInput(
                                   ns("mapTexture"),
                                   "Map Texture",
                                   choices = list(
                                       "OpenStreetMap.Mapnik" = "OpenStreetMap.Mapnik",
                                       "OpenStreetMap.BlackAndWhite" = "OpenStreetMap.BlackAndWhite",
                                       "Stamen.Toner" = "Stamen.Toner",
                                       "CartoDB.Positron" = "CartoDB.Positron",
                                       "Esri.NatGeoWorldMap" = "Esri.NatGeoWorldMap",
                                       "Stamen.Watercolor" = "Stamen.Watercolor",
                                       "Stamen.Terrain" = "Stamen.Terrain",
                                       "Esri.WorldImagery" = "Esri.WorldImagery",
                                       "Esri.WorldTerrain" = "Esri.WorldTerrain"
                                   ),
                                   selected = "CartoDB.Positron"
                               ),
                               selectInput(
                                   ns("mapColor"),
                                   "Points Color",
                                   choices = list(
                                       "Red" = 'red',
                                       "Green" = "green",
                                       "Blue" = "blue",
                                       "Black" = "black"
                                   )
                               )
                           )
                       ),
                       tabPanel("Table View",
                                DT::dataTableOutput(ns("inputDataTable")))
                   ))
            
            # ------------- End of Map/Table Module -------------------
        )
    )
}

# Module Server

#' @rdname mod_add_data
#' @export
#' @keywords internal
#' @importFrom rgbif occ_search 
#' @importFrom spocc occ
#' @importFrom finch dwca_read 
#' @importFrom data.table fread
#' @importFrom shinyjs runjs
#' @import DT leaflet
mod_add_data_server <- function(input, output, session, next_button_id = "dataToConfigureDiv"){
    ns <- session$ns
    
    returnData <- data.frame()
    mapData <- data.frame()
    map <- leafletProxy(ns("mymap"))
    
    # ----------------
    
    observeEvent(input$queryDatabase, {
        withProgress(message = paste("Querying", input$queryDB, "..."), {
            if (input$queryDB == "gbif") {
                data <-
                    rgbif::occ_search(
                        scientificName = input$scientificName,
                        limit = input$recordSize,
                        hasCoordinate = switch(
                            input$hasCoords,
                            "1" = TRUE,
                            "2" = FALSE,
                            "3" = NULL
                        )
                    )
                returnData <<- data$data
                mapData <<- returnData
                
            } else {
                warnings <- capture.output(
                    data <-
                        spocc::occ(
                            query = input$scientificName,
                            from = input$queryDB,
                            limit = input$recordSize,
                            has_coords = switch(
                                input$hasCoords,
                                "1" = TRUE,
                                "2" = FALSE,
                                "3" = NULL
                            )
                        ),
                    type = "message"
                )
                
                if (length(warnings) > 0) {
                    showNotification(paste(warnings, collapse = " "),
                                     duration = 6)
                }
                
                tempData <- data[[input$queryDB]]$data[[1]]
                returnData <<- tempData
                mapData <<- returnData
            }
        })
        
        dataLoadedTask()
    })
    
    observeEvent(input$inputFile, {
        withProgress(message = paste("Reading", input$inputFile$name, "..."), {
            
            print(input$inputFile$type)
            
            if (is.null(input$inputFile))
                return("No data to view")
            
            if (grepl("zip", tolower(input$inputFile$type))) {
                message("Reading DWCA ZIP...")
                finchRead <-
                    finch::dwca_read(input$inputFile$datapath, read = T)
                returnData <<- finchRead$data[[1]]
                mapData <<- returnData
                
            } else if (grepl("rds", tolower(input$inputFile$type))) {
                message("Reading RDS...")
                returnData <<-
                    readRDS(input$inputFile$datapath)
                mapData <<- returnData
            } else  {
                message("Reading Tabular Data...")
                returnData <<-
                    data.table::fread(input$inputFile$datapath)
                mapData <<- returnData
            }
        })
        dataLoadedTask()
    })
    
    
    observeEvent(input$mapTexture, {
        if (length(returnData) == 0) {
            return(NULL)
        }
        leafletProxy(ns("mymap"), data = mapData) %>%
            clearShapes() %>%
            addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
    })
    
    observeEvent(input$mapColor, {
        if (length(returnData) == 0) {
            return(NULL)
        }
        leafletProxy(ns("mymap"), data = mapData) %>%
            clearShapes() %>%
            addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
    })
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(input$mapTexture) %>%
            setView(0, 0, zoom = 2)
    })
    
    dataLoadedTask <- function() {
        mapData <<- as.data.frame(mapData)
        if (length(mapData) == 0) {
            showNotification("Empty data returned! Try different setting.",
                             duration = 2)
            return()
        }
        
        if ("decimallatitude" %in% tolower(colnames(mapData))) {
            mapData$decimalLatitude <<-
                as.numeric(mapData[, which(tolower(colnames(mapData)) == "decimallatitude")])
            mapData$decimalLongitude <<-
                as.numeric(mapData[, which(tolower(colnames(mapData)) == "decimallongitude")])
        } else if ("latitude" %in% tolower(colnames(mapData))) {
            mapData$decimalLatitude <<-
                as.numeric(mapData[, which(tolower(colnames(mapData)) == "latitude")])
            mapData$decimalLongitude <<-
                as.numeric(mapData[, which(tolower(colnames(mapData)) == "longitude")])
        } else {
            return()
        }
        
        # ------------ End of Darwinizing Data -------------
        
        try(leafletProxy(ns("mymap"), data = mapData) %>%
                clearShapes() %>%
                addCircles(~ decimalLongitude, ~ decimalLatitude, color = input$mapColor))
        
        output$inputDataTable <- DT::renderDataTable(DT::datatable({
            summarizeDataframe(mapData)
        }, options = list(scrollX = TRUE)))
        
        
        shinyjs::runjs(code = paste('$("#', ns("queryDatabaseDiv"), '").addClass("readyButton");', sep = ""))
        shinyjs::runjs(code = paste('$("#', ns("queryDatabaseDiv"), '").removeClass("activeButton");', sep = ""))
        shinyjs::runjs(code = paste('$("#', ns("inputFileDiv"), '").addClass("readyButton");', sep = ""))
        shinyjs::runjs(code = paste('$("#', ns("inputFileDiv"), '").removeClass("activeButton");', sep = ""))
        shinyjs::runjs(code = paste('$("#', next_button_id, '").addClass("completedButton");', sep = ""))
        shinyjs::runjs(code = paste('$("#', next_button_id, '").removeClass("activeButton");', sep = ""))
        
        showNotification("Read Data Successfully", duration = 2)
    }
    
    returnDataReact <- reactive({
        # Input actions that need to trigger new dataframe return 
        input$inputFile
        input$queryDatabase
        
        returnData
    })
    
    
    return(returnDataReact)
}
