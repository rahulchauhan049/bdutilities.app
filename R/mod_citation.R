# Module UI
  
#' @title   mod_citation_ui and mod_citation_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_citation
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_citation_ui <- function(id, package){
  ns <- NS(id)
  tagList(
    column(12,
           h1(paste0("Cite ", package, "!")),
           column(12, 
                  radioButtons(
                    ns("citation_level"),
                    label = h3("Citation Level"),
                    choices = list(
                      "Core - Citation for just base R and this package" = 1,
                      "Dependency - Citations for packages used by this package directly" = 2,
                      "Full - Citations for every single package this package depends on directly and indirectly" = 3
                    ),
                    selected = 1
                  ),
                  
                  downloadButton(ns("download_bib"), label = "Download Bibtext file for current citation"),
                  
                  uiOutput(ns("citationsUI"))))
    
  )
}
    
# Module Server
    
#' @rdname mod_citation
#' @export
#' @keywords internal
#' @import knitr gtools 
mod_citation_server <- function(input, output, session, package){
  ns <- session$ns
  
  output$citationsUI <- renderUI({
    components <- list()
    components[[1]] <- tagList(h3("R"),
                               p(suppressWarnings(format(citation(), style = "text")[1])))
    components[[2]] <- tagList(h3(package),
                               p(suppressWarnings(format(citation(package), style = "text")[1])))
    
    if (input$citation_level == 1) {
      
    } else if (input$citation_level == 2) {
      print("---------DT--------")
      dep <- gtools::getDependencies(package)
      dep <- rev(dep)
      
      for (ind in 1:15) {
        components[[ind + 2]] <- tagList(h3(dep[ind]),
                                         p(suppressWarnings(format(citation(dep[ind]), style = "text")[1])))
        
        print(dep[ind])
        print(components[[ind + 2]])
      }
    } else {
      dep <- gtools::getDependencies(package)
      dep <- rev(dep)
      
      for (ind in 1:length(dep)) {
        components[[ind + 2]] <- tagList(h3(dep[ind]),
                                         p(suppressWarnings(format(citation(dep[ind]), style = "text")[1])))
      }
    }
    
    print("--------HT---------")
    
    print(components)
    print("--------MT---------")
    return(components)
  })
  
  output$download_bib <- downloadHandler(
    filename = function() {
      paste("citation-", Sys.Date(), ".bib", sep = "")
    },
    content = function(con) {
      if (input$citation_level == 1) {
        cont <- c("base", package)
      } else if (input$citation_level == 2) {
        cont <- c("base", package, rev(gtools::getDependencies(package))[1:15])
      } else {
        cont <- c("base", package, rev(gtools::getDependencies(package)))
      }
      
      knitr::write_bib(cont, con)
    }
  )
}
    
## To be copied in the UI
# mod_citation_ui("citation_ui_1")
    
## To be copied in the server
# callModule(mod_citation_server, "citation_ui_1")
 
