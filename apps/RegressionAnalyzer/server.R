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
#' Save sleected cartridge on plot or sidebar along all tabs?



server <- function(input, output) {
  
  # ----------------------- DATA IMPORT ----------------------------------------
 
  # Get list with messages, targets and configuration
  regression_data <- mod_regressiondataimporterServer("regressiondata1") 
  # Get reactive elements from list  
  messages_r <- reactive ({ regression_data$messages_r()  })
  targets_r <-regression_data$targets_r
  conf_xml_r <- regression_data$conf_r
  zipdatapath_r <- regression_data$zipdatapath_r
  
  # ---------------------------- DATA SELECTION --------------------------------
  
  selectedchambersforscatter_r <- mod_chamberSelectorServer("chambers1", showxtalk=FALSE, 
                                                showfaceting=FALSE)

  choicelist_r <- reactive ({
    unique(messages_r()$id)
  })
  current_testID_r <- mod_cartridgeSelectorServer('selectedtest1', 
                                                  fromcsv = FALSE,
                                                  choicelist = choicelist_r )$testID
  selecteddatafordisplay_r <- reactive ({
    selected <- regression_data[[as.character(input$dataset)]]()
    if (input$dataset != 'conf_r' & !is.null(current_testID_r())){
      selected %>% 
        filter(id %in% current_testID_r())
    } else{
      selected
    }
  })
  
  # ----------------------- DATA USAGE AND PROCESSING --------------------------
  
  mod_regressionscatterplotterServer('exclusion1', conf_xml_r, targets_r, 
                                     messages_r, selectedchambersforscatter_r, 
                                     zipdatapath_r)

  # --------------------------------- OUTPUTS ----------------------------------
  
  output$text <- renderText({
    input$dataset
  })
  
  mod_regressiondataexplorerServer('displayedregressiondata1', 
                                   selecteddatafordisplay_r)
  
  
  output$regressiondata <- DT::renderDataTable({
    DT::datatable({
      targets_r()
    })
  })
  


  
}