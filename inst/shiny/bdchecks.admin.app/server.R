library(shiny)
library(shinyjs)

shinyServer(function(input, output) {
    checks <-
        read_yaml(
            "/Users/thiloshonnagarajah/RProjects/bdchecks/inst/extdata/data_check.yaml"
        )
    
    tests <-
        read_yaml("/Users/thiloshonnagarajah/RProjects/bdchecks/inst/extdata/data_test.yaml")
    
    r_loc <- "~/RProjects/bdchecks/R"
    
    output$menu <- renderUI({
        menus <- list()
        
        for (i in 1:length(checks)) {
            menus[[i]] <-
                menuItem(
                    checks[[i]]$name,
                    tabName = checks[[i]]$name,
                    icon = icon("plus-circle"),
                    selected = T
                )
        }
        
        return(tagList(dashboardSidebar(sidebarMenu(menus))))
    })
    
    output$tab <- renderUI({
        tabs <- list()
        
        
        create_layer <- function(listElems, prefix){
            n <- names(listElems)
            elem_placeholder <- list()
            for (index in 1 : length(listElems)) {
                if (length(listElems[[index]]) > 1){
                    create_layer(listElems[[index]], paste0(prefix, "$", n[[index]]))
                } else {
                    id <- paste0(prefix, "$", n[[index]])
                    print()
                    elem_placeholder[[length(elem_placeholder) + 1]] <-
                        textInput(id,
                                  label = "dsaf",
                                  value = checks[[id]]
                        )
                }
            }
            
            return(elem_placeholder)
        }
        
        names <- names(checks)
        for (i in 1:length(checks)) {
            print("----")
            meta_input_fields <- create_layer(checks[[i]], paste0("`", names[[i]],"`"))
            
            # meta <- unlist(checks[[i]])
            # 
            # 
            # test <- unlist(tests[checks[[i]]$name])
            # 
            # for (j in 1:length(meta)) {
            #     meta_input_fields[[j]] <-
            #         textInput("test",
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
                                    tabPanel(
                                        "Meta Data",
                                        column(
                                            12,
                                            div(class = "secondaryHeaders", h3("Edit Meta Data")),
                                            
                                            tagList(meta_input_fields),
                                            
                                            actionButton("tr", "Save Meta")
                                        )
                                    ),
                                    
                                    tabPanel("Test Data",
                                             column(
                                                 12,
                                                 div(class = "secondaryHeaders", h3("Edit Test Data"))
                                                 
                                             )),
                                    
                                    tabPanel(
                                        "R Code",
                                        column(
                                            12,
                                            div(class = "secondaryHeaders", h3("Edit R Code")),
                                            
                                            textAreaInput(
                                                "rgt",
                                                label = "R Code",
                                                value = paste(readLines(
                                                    paste0(r_loc, "/dc_", checks[[i]]$name, ".R")
                                                )
                                                , collapse = "\n")
                                            ),
                                            
                                            actionButton("tr", "Save R Code")
                                        )
                                    )
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
            tags$div(tagList(tabs), class = "tab-content")
        ))
    })
    
})
