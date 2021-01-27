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
#' Reactivity, where to put and where not to


# Define necessary variables ---------------------------------------------------

pathtojsonfile <- "/home/shinyuser/RCApps/info/paths.json"

defaultzipfile_path <- file.path(
  "/home/shinyuser/RCApps/data/default_input/GI22_regression/GI22_regressionoutput.zip"
)

defaultconfigfile_path <- file.path(
  "/home/shinyuser/RCApps/data/default_input/GI22_regression/rca_gastro2_config_2.0.2.xml"
)



# User Interface ---------------------------------------------------------------

mod_regressiondataimporterUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      fileInput(
        label = "Select Dataset",
        inputId = ns("input_dataset"),
        accept = c(".zip"),
        multiple = FALSE
      ),
      fileInput(
        label = "Select configuration file",
        inputId = ns("input_config"),
        accept = ".xml"
      )
    )
  )
}


mod_regressiondataimporterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      # Load regression datazip and return list with messages and targets
      zipdata_r <- reactive({
        if (is.null(input$input_dataset)) {
          # # Default input file
          # func_dataextractorfromzipfile(defaultzipfile_path)
          # showModal(modalDialog(title = "Default regression zip file has been added", paste0(
          #   "A default regression zip file has been added. 
          #   Upload your own zip file for analysis. Path to file: ", "\n",
          #   defaultzipfile_path
          # )))
          return(NULL)
          
        } else {
          func_dataextractorfromzipfile(input$input_dataset$datapath)
        }
      })
      # zipdata_r <- reactive({
      #   if (is.null(input$input_dataset)) {
      #     return(NULL)
      #   } else {
      #     func_dataextractorfromzipfile(input$input_dataset$datapath)
      #   }
      # })

      
      # Load config file data
      conf_xml_r <- reactive({
        if (is.null(input$input_config)) {
          # Default input file
          # func_dataextractorfromconfigfile(defaultconfigfile_path)
          # showModal(modalDialog(title = "Default config file has been added", paste0(
          #   "A default configuration file has been added. Upload your own configuration file for analysis.
          #   Path to file: ", "\n",
          #   defaultconfigfile_path
          # )))
          return(NULL)
          
        } else {
          func_dataextractorfromconfigfile(input$input_config$datapath)
        }
      })
      # conf_xml_r <- reactive({
      #   func_dataextractorfromconfigfile(input$input_config$datapath)
      # })


      return(list(
        targets_r = reactive({
          zipdata_r()$targets
        }),
        messages_r = reactive({
          zipdata_r()$messages
        }),
        conf_r = conf_xml_r,
        zipdatapath_r = reactive({input$input_dataset$datapath})
        
      ))
    }
  )
}
