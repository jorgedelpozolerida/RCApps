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
#' Control only selecting one combination of Rc and CH


server <- function(input, output) {
  
  # ----------------------- DATA IMPORT ----------------------------------------
 
  # Get list with messages, targets and configuration
  regression_data <- mod_regressiondataimporterServer("regressiondata1") 
  # Get reactive elements from list  
  messages_r <- regression_data$messages_r
  targets_r <-regression_data$targets_r
  conf_xml_r <- regression_data$conf_r

  
  # ---------------------------- DATA SELECTION --------------------------------
  
  selectedchambers_r <- mod_chamberSelectorServer("chambers1", showxtalk=FALSE, 
                                                showfaceting=FALSE)
  # current_testID_r <- mod_cartridgeSelectorServer("testID_1")$testID
  
  
  # ----------------------- DATA USAGE AND PROCESSING --------------------------
  
  mod_regressionscatterplotterServer('exclusion1', conf_xml_r, targets_r, 
                                     messages_r, selectedchambers_r)

  # --------------------------------- OUTPUTS ----------------------------------
  
  output$text <- renderText({
    # length(regression_data)
  })
  output$regressiondata <- DT::renderDataTable({
    DT::datatable({
      targets_r()
    })
  })

  
}