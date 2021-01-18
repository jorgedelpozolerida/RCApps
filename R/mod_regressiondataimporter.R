## ---------------------------
##
## Script name:  MODULE FOR SELECTING DATA FROM REGRESSION
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




mod_regressiondataimporterUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      fileInput(
        label = "Select Dataset",
        inputId = ns("input_dataset"),
        accept = c(".zip"),
        multiple = FALSE
      ),
      fileInput(
        label = "Select configuration file",
        inputId = ns("input_config"),
        accept = ".xml"
      )
    )
  )
}


mod_regressiondataimporterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      # Load regression datazip and return list with messages and targets
      zipdata_r <- reactive({
        if (is.null(input$input_dataset)) {
          return(NULL)
        } else {
          func_dataextractorfromzipfile(input$input_dataset$datapath)
        }
      })

      # Load config file data
      conf_xml_r <- reactive({
        func_dataextractorfromconfigfile(input$input_config$datapath)
      })


      return(list(
        targets_r = reactive({
          zipdata_r()$targets
        }),
        messages_r = reactive({
          zipdata_r()$messages
        }),
        conf_r = conf_xml_r,
        zipdatapath_r = reactive({input$input_dataset$datapath})
        
      ))
    }
  )
}
