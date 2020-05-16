#' @title module to add darwinization ability
#' @description  UI to add darwinization funtion.
#'
#' @param id shiny id
#'
#' @rdname mod_darwinize
#'
#' @keywords internal
#' @export 
#' @import shiny
mod_darwinize_ui <- function(id) {
  ns <- NS(id)
  
  tagList(column(
    12,
    id = ns("darwinControl"),
    br(),
    
    div(
      class = "darwinControlDiv",
      h2("Clean Headers"),
      p("Use darwinization to standardize headers"),
      div(
        class = "darwinControlInner",
        actionButton(ns("darwinizeButton"), "Perform Header Cleaning"),
        
        helpText(
          "To manually edit or clean headers, use ",
          a("bdDwC", href = "https://cran.r-project.org/web/packages/bdDwC/index.html"),
          " package. Launch bdDwC shiny app with the command 'bdDwC::run_dwc()' in R console,  or "
        ),
        
        actionButton(ns("launch_bddwc"), "Launch bddwc Shiny App Now"),
        helpText("(Requires RStudio 1.2 and above.)"),
        br()
      )
    )
  ))
}


# Module Server

#' @rdname mod_darwinize
#' @export
#' @keywords internal
#' @importFrom bdutilities return_core
#' @import shiny data.table bdDwC
mod_darwinize_server <-
  function(input, output, session, dat) {
    
    ns <- session$ns
    returnState <- data.frame()
    
    observeEvent(input$darwinizeButton, {
      data <- as.data.frame(bdutilities::return_core(dat))
      returnState <<- data
      
      showNotification("Cleaning Headers", duration = 4)
      
      darwinizer = tryCatch({
        bdDwC::darwinize_names(as.data.frame(data), bdDwC:::data_darwin_cloud$data)
      }, error = function(e) {
        print(e)
        showNotification("Darwinizing Erred")
      })
      
      if (class(darwinizer) != "character") {
        fixed <-
          darwinizer[darwinizer$match_type == "Darwinized", ]
        
        if (nrow(fixed) > 0) {
          tidyData <- bdDwC::rename_user_data(data, darwinizer)
          
          showNotification(paste("Converted Columns:",
                                 paste(
                                   paste(fixed[, 1], collapse = ", "),
                                   paste(fixed[, 2], collapse = ", "),
                                   sep = " -> "
                                 )),
                           duration = 7)
          returnState <<- tidyData
        } else {
          showNotification("No headers required darwinization", duration = 4)
        }
      }
    })
    
    returnDataReact <- reactive({
      # Input actions that need to trigger new dataframe return
      input$darwinizeButton
      return(returnState)
    })
    return(returnDataReact)
  }