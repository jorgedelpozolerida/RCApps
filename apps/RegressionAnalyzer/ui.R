## ---------------------------
##
## Script name: RegressionAnalyzer user interface
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


ui <- fluidPage(

  # Application title
  titlePanel("Regression Analyzer"),

  # Sidebar Layout: SiderBar + MainPanel
  sidebarLayout(
    # SideBar
    sidebarPanel(
      mod_regressiondataimporterUI("regressiondata1"),
      conditionalPanel(condition = "input.tabs == 'Scatter & SNR plots'",
        mod_chamberSelectorUI('chambers1')
        
      ),
      textOutput("text") # used for troubleshooting
      
    ),
    # Panel
    mainPanel(
      tabsetPanel(
        id = "tabs",
        type = "tabs", # TO OD :investigate "hidden" for controlling tabs other way
        tabPanel(
          "Raw Data Visualization",
          # mod_regressiondataexplorer("plot1") # amplification plot UI
          DT::dataTableOutput("regressiondata") # Displayed table
          
          ),
        tabPanel(
          "Result Individual",
          # mod_regressiondataexplorer()
        ),
        tabPanel(
          "Scatter & SNR plots",
          # mod_regressionplotter()
          mod_regressionscatterplotterUI('exclusion1')
        )
      )
    )
  )
)