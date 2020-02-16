library(shiny)
library(shinyjs)
library(DT)
library(rhandsontable)

source("functions.R")

shinyServer(function(input, output, session) {
    path <- paste0(getwd(), "/yamls/data_check_edited.yaml")
    tests <- yaml_to_dataframe()
    
    if (file.exists(path)) {
        url <- path
    } else {
        url <- paste0(getwd(), "/yamls/data_check.yaml")
    }
    
    checks <-
        read_yaml(url)
    
    r_loc <- "~/RProjects/bdchecks/R"
    
    output$menu <- renderUI({
        menus <- list()
        
        for (i in 1:length(checks)) {
            menus[[i]] <-
                menuItem(
                    checks[[i]]$name,
                    tabName = checks[[i]]$name,
                    icon = icon("plus-circle")
                )
        }
        
        return(tagList(dashboardSidebar(sidebarMenu(id = "menuslo", menus))))
    })
    
    output$tab <- renderUI({
        observe({
            print(input[["menuslo"]])
        })
        
        tabs <- list()
        elem_placeholder <- list()
        
        create_layer <- function(listElems, prefix) {
            n <- names(listElems)
            
            for (index in 1:length(listElems)) {
                if (class(listElems[[index]]) == "list") {
                    elem_placeholder[[length(elem_placeholder) + 1]] <<-
                        h3(names(listElems)[[index]])
                    create_layer(listElems[[index]], paste0(prefix, "$", n[[index]]))
                    elem_placeholder[[length(elem_placeholder) + 1]] <<-
                        hr()
                    
                } else {
                    id <- paste0(prefix, "$", n[[index]])
                    elem_placeholder[[length(elem_placeholder) + 1]] <<-
                        textInput(id,
                                  label = names(listElems)[[index]],
                                  value = listElems[[index]])
                }
            }
            
            return(elem_placeholder)
        }
        
        names <- names(checks)
        for (i in 1:length(checks)) {
            elem_placeholder <- list()
            meta_input_fields <-
                create_layer(checks[[i]], paste0("`", names[[i]], "`"))
            
            # meta <- unlist(checks[[i]])
            # meta_input_fields <- list()
            
            # test <- unlist(tests[checks[[i]]$name])
            
            # for (j in 1:length(meta)) {
            #     meta_input_fields[[j]] <-
            #         textInput(names(meta[j]),
            #                   label = names(meta[j]),
            #                   value = meta[[j]])
            # }
            
            
            
            tabs[[i]] <-
                tabItem(checks[[i]]$name,
                        fluidRow(column(
                            12,
                            h1(paste0("Check ", i, ": ", checks[[i]]$name), class =
                                   "primaryHeader"),
                            
                            column(
                                12,
                                tabsetPanel(
                                    type = "tabs",
                                    id = paste0(checks[[i]]$name, "_table_tab"),
                                    tabPanel("Meta Data",
                                             column(
                                                 12,
                                                 div(class = "secondaryHeaders", h3("Edit Meta Data")),
                                                 
                                                 tagList(meta_input_fields)
                                             )),
                                    
                                    tabPanel(
                                        "Test Data",
                                        column(
                                            12,
                                            div(class = "secondaryHeaders", h3("Edit Test Data")),
                                            rHandsontableOutput(paste0(checks[[i]]$name, "_table"))
                                        )
                                    ),
                                    
                                    tabPanel("R Code",
                                             column(
                                                 12,
                                                 div(class = "secondaryHeaders", h3("Edit R Code")),
                                                 
                                                 textAreaInput(
                                                     paste0(checks[[i]]$name, "_rcode"),
                                                     label = "R Code",
                                                     value = paste(suppressWarnings(readLines(
                                                         paste0(r_loc, "/dc_", checks[[i]]$name, ".R")
                                                     ))
                                                     , collapse = "\n")
                                                 )
                                             ))
                                )
                            )
                        )))
        }
        
        
        
        return(dashboardBody(
            tags$head(
                tags$link(
                    rel = "stylesheet",
                    type = "text/css",
                    href = "style.css"
                )
            ),
            useShinyjs(),
            fluidRow(column(
                12,
                column(6,
                       tags$div(tagList(tabs), class = "tab-content", id = "sideTabs")),
                column(6,
                       div(id = "yaml",
                           fluidRow(
                               textAreaInput(
                                   "yaml",
                                   label = "YAML File",
                                   value = paste(as.yaml(checks), collapse = "\n")
                               )
                           )))
            ))
        ))
    })
    
    output$textWithNewlines <- renderUI({
        rawText <-
            readLines(paste0(getwd(), "/yamls/data_check_edited.yaml"))
        
        
        splitText <-
            stringi::stri_split(str = rawText, regex = '\\n')
        
        # wrap a paragraph tag around each element in the list
        replacedText <- lapply(splitText, p)
        
        return(replacedText)
    })
    
    
    
    getDTTests <- function(){
        elems <- reactiveValuesToList(input)
        print(elems[!grepl("DC", names(elems))])
        tests[[checks[[1]]$name]]
        print(input$menus)
    }
    
    lapply(1:length(checks), function(index){
        output[[paste0(checks[[index]]$name, "_table")]] <-
            renderRHandsontable({
                if(is.null(tests[[checks[[index]]$name]])){
                    rhandsontable(data.frame())
                }else {
                    rhandsontable(tests[[checks[[index]]$name]])
                }
            })
    })
    
    
    observe({
        lapply(1:length(checks), function(index){
            
            if(!is.null(input[[paste0(checks[[index]]$name, "_table")]])){
                DF <-  hot_to_r(input[[paste0(checks[[index]]$name, "_table")]])
                tests[[checks[[index]]$name]] <<- DF
            }
            
        })
        print(tests)
        dataframe_to_yaml(tests)
    })
    
    
    observe({
        elems <- reactiveValuesToList(input)
        
        for (index in 1:length(elems)) {
            name <- names(elems[index])
            
            
            if (!is.null(name) &&
                nchar(elems[[index]]) > 0 && grepl("`DC_", name)) {
                eval(parse(text = paste0(
                    "checks$", name, " <<-", "'", elems[index], "'"
                )))
            } else if (!is.null(name) &&
                       nchar(elems[[index]]) > 0 && grepl("_rcode", name)) {
                writeLines(elems[[index]],
                           paste0(r_loc, "/dc_", strsplit(name, "_rcode")[1], ".R"))
            }
        }
        
        updateTextAreaInput(session, "yaml", value = paste(as.yaml(checks), collapse = "\n"))
        write_yaml(checks,
                   paste0(getwd(), "/yamls/data_check_edited.yaml"))
    })
    
})
