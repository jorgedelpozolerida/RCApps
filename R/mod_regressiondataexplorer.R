## ---------------------------
##
## Script name:  MODULE FOR EXPLORING IMPORTED DATA FROM REGRESSION
##
## Purpose of script:
##
## Author: jorgedelpozolerida
##
## Date Created: 2020-12-11
##
## Email: jorge.delpozo@qiagen.com / jorgedelpozolerida@gmail.com
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------
##
#' TO DO:
#'
#' Reactivity, where to put and where not to


# Create variables -------------------------------------------------------------



# User Interface ---------------------------------------------------------------

mod_regressiondataexplorerUI <- function(id) {
  
  ns <- NS(id)

  tagList(
    DT::dataTableOutput(outputId = ns("table"))
  )
}

# Server -----------------------------------------------------------------------

mod_regressiondataexplorerServer <- function(id, regression_data_r) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$table <- DT::renderDT(
        {
          regression_data_r()
        },
        filter="top",
        class = 'cell-border stripe',
        options = list(pageLength = 10)
      )
      return()
    }
  )
}
