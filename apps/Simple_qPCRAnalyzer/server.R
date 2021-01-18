## ---------------------------
##
## Script name: SIMPLE_QPCRANALYZER SERVER
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


sapply(list.files("../../R", full.names = TRUE), source)




server <- function(input, output) {
  
  
  # ----------------------- DATA SELECTION -------------------------------------
  
  # Get selection data list, containing reactive values inside
  selected_chambers <- mod_chamberSelectorServer("chambers1", showxtalk=TRUE)
  
  # Get TestID reactive
  current_testID_r <- mod_cartridgeSelectorServer("testID_1")$testID 
  # Load raw data data frame into reactive
  rawdata_r <- reactive({
    func_dataImporter(current_testID_r(), fromIn2Data = FALSE, typeofdata = "raw")
  })
  # Load metadata data frame into reactive
  metadata_r <- reactive({
    func_dataImporter(current_testID_r(), fromIn2Data = TRUE, typeofdata = "metadata")
  })
  # Load analyte data dataframe into reactive
  analytedata_r <- reactive({
    func_dataImporter(current_testID_r(), fromIn2Data = TRUE, typeofdata = "analyte")
  })
  
  # ---------------- DATA USAGE AND PROCESSING ---------------------------------
  
  # Amplification plot Server part inputting raw data and selection data
  mod_amplificationPlotterServer("plot1", rawdata_r, selected_chambers, current_testID_r())
  
  
  # ---------------------- OUTPUTS ---------------------------------------------
  
  # For troubleshooting displayed text
  output$text <- renderText({ # selected_chambers$allplots()
    #selected_chambers$CH_xtalk()
  })
  
  output$rawdata <- DT::renderDataTable({
    DT::datatable({
      rawdata_r()
    })
  })
}
