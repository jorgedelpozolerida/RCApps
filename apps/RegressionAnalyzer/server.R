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
  mod_chamberSelectorServer("chambers1", showxtalk=FALSE)
  data <- reactive ({ mod_regressiondataimporterServer("regressiondata1") })

  # ---------------------- OUTPUTS ---------------------------------------------
  output$text <- renderText({ # selected_chambers$allplots()
    length(data)
  })
  output$regressiondata <- DT::renderDataTable({
    DT::datatable({
      tabledata <- data$data()
      data <- tabledata$dataset_data
      data1 <- data$targets()
    })
  })
  
}