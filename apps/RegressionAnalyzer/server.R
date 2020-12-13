## ---------------------------
##
## Script name: RegressionAnalyzer Server
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
#'

server <- function(input, output) {
  
  # ----------------------- DATA IMPORT ----------------------------------------
  
  regression_data <- reactive ({
    # Get reactive list with messages, targets and configuration data
    mod_regressiondataimporterServer("regressiondata1") 
    })
  messages <- reactive ({
    regression_data()$messages
    })
  targets <- reactive ({ regression_data()$targets
  })
  conf_xml <- reactive ({ regression_data()$conf
  })
  
  # ----------------------- DATA SELECTION -------------------------------------
  
  mod_chamberSelectorServer("chambers1", showxtalk=FALSE, showfaceting=FALSE)
  

  # ---------------------- OUTPUTS ---------------------------------------------
  
  output$text <- renderText({ 
length(regression_data)
  })
  output$regressiondata <- DT::renderDataTable({
    DT::datatable({
      messages()()
      
    })
  })
  
}