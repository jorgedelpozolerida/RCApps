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

sapply(list.files("../../R", full.names = TRUE), source)



ui <- fluidPage(

  #Use shiny themes selector
  shinythemes::themeSelector(),
  
  # Enable shinyjs
  useShinyjs(),
  
  # Application title
  titlePanel("Regression Analyzer"),

  # Sidebar Layout: SiderBar + MainPanel
  sidebarLayout(
    # SideBar
    sidebarPanel(
      mod_regressiondataimporterUI("regressiondata1"),
      conditionalPanel(condition = "input.tabs == 'Scatter & SNR plots'",
        mod_chamberSelectorUI('chambers1'),
        mod_regressionplotfilterUI("scatterparameters1")
        
        
      ),
      conditionalPanel(condition = "input.tabs == 'Raw Data Visualization'",
                       # Select which dataframe to display
                       radioButtons(
                         inputId = "dataset",
                         label = "Data:",
                         choices = list(
                           "Targets" = "targets_r",
                           "Messages" = "messages_r",
                           "Configuration" = "conf_r"
                         ),
                         inline = TRUE
                       ),
                       mod_cartridgeSelectorUI('selectedtest1')
                       
                       
                       
                       #show controls here
      ),
      # textOutput("text"), # used for troubleshooting
      width = 2
    ),
    # Panel
    mainPanel(
      tabsetPanel(
        id = "tabs",
        type = "tabs", # TO OD :investigate "hidden" for controlling tabs other way
        tabPanel(
          "Raw Data Visualization",
          # mod_regressiondataexplorer("plot1") # amplification plot UI
          # DT::dataTableOutput("regressiondata") # Displayed table
          mod_regressiondataexplorerUI("displayedregressiondata1")
          
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