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
#'




mod_regressiondataimporterUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      fileInput(label = 'Select Dataset',
                inputId = ns('input_dataset'),
                accept = c('.zip'),
                multiple = FALSE),
      fileInput(label = 'Select configuration file',
                inputId = ns('input_config'),
                accept = '.xml')
    )
    )
}


mod_regressiondataimporterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Load regression datazip and return list with messages and targets
      zipdata <- reactive ({
        func_dataextractorfromzipfile(input$input_dataset)
      })
        
      messages <-zipdata()$messages 
        
      targets <-zipdata()$targets 
        
      
      # Load config file data
      cfg_path <- reactive ({ input$input_config })
      conf_xml <- func_dataextractorfromconfigfile(cfg_path()$datapath)

      
      
      return(list(
        targets =  reactive ({ targets }),
        messages = reactive ({ messages}),
        conf = reactive ({ conf_xml})
      )) 
    }
  )
}
