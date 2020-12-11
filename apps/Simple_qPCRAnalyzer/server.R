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


server <- function(input, output) {
  
  
  # ----------------------- DATA SELECTION -------------------------------------
  
  # Get TestID reactive
  current_testID <- mod_cartridgeSelectorServer("testID_1")$testID # cartridge selector Server
  # Load raw data data frame into reactive
  rawdata <- reactive({
    func_dataImporter(current_testID(), fromIn2Data = FALSE, typeofdata = "raw")
  })
  # Load metadata data frame into reactive
  metadata <- reactive({
    func_dataImporter(current_testID(), fromIn2Data = TRUE, typeofdata = "metadata")
  })
  # Load analyte data dataframe into reactive
  analytedata <- reactive({
    func_dataImporter(current_testID(), fromIn2Data = TRUE, typeofdata = "analyte")
  })
  
  # ---------------- DATA USAGE AND PROCESSING ---------------------------------
  
  # Get selection data list, containing reactive values inside
  selected_chambers <- mod_chamberSelectorServer("chambers1", showxtalk=TRUE)
  # For troubleshooting displayed text
  output$text <- renderText({ # selected_chambers$allplots()
    #selected_chambers$CH_xtalk()
  })
  # Amplification plot Server part inputting raw data and selection data
  mod_amplificationPlotterServer("plot1", rawdata, selected_chambers, current_testID())
  
  
  # ---------------------- OUTPUTS ---------------------------------------------
  
  output$rawdata <- DT::renderDataTable({
    DT::datatable({
      rawdata()
    })
  })
}
