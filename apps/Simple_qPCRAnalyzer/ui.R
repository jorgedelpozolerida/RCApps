## ---------------------------
##
## Script name: SIMPLE_QPCRANALYZER USER INTERFACE
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
  titlePanel("Simple qPCR Analyzer"),
  
  # Sidebar Layout: SiderBar + MainPanel
  sidebarLayout(
    # SideBar
    sidebarPanel(
      mod_cartridgeSelectorUI("testID_1"), # select cartridge/test
      mod_chamberSelectorUI("chambers1"), # select RC, Sensor and others
      textOutput("text") # used for troubleshooting
    ),
    # Panel
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Amplification plot",
          mod_amplificationPlotterUI("plot1"), # amplification plot UI
        ),
        tabPanel(
          "Data Table",
          DT::dataTableOutput("rawdata") # Displayed table
        )
      )
    )
  )
)
