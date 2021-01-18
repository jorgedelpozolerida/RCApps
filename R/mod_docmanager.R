## ---------------------------
##
## Script Name:  MODULE FOR MANAGING RMARKDOWNS ON DATABASE
##
## Purpose of script:
##
## Author: jorgedelpozolerida
##
## Date Created: 2021-01-18
##
## Email: jorgedelpozolerida@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------
#' TO DO:
#'
#' Path to regressiondatra might change... manage that

# Necessary variables -------------------------------------------------------------

panel_list <- list(
  men = 'Meningitis',
  gastro= 'Gastroinstestinal',
  onco = 'Oncology',
  respis = 'Respiratory'
)



# User Interface ---------------------------------------------------------------

mod_docmanagerUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    prettyCheckbox(
      inputId = ns("manual"),
      value = FALSE,
      label = "Add manually",
      thick = TRUE,
      shape = "curve",
      animation = "pulse",
      status = "info",
      inline = TRUE
    ),
    # what to show if manual selected
    conditionalPanel(
      condition = paste0("input['", ns("manual"), "']"),
      fluidPage(
        column(6,
               selectInput(ns("panel"), "Select panel", choices=panel_list),
               textInput(ns('type'), "Document type", value = "", width = NULL, placeholder = "f.e. ROCcurves"),
                textInput(ns('author'), "Document type", value = "", width = NULL, placeholder = "f.e jdelpozo"),
                textInput(ns('datasetpath'), "Path to dataset", value = "", width = NULL, placeholder = NULL)),
        
        column(6,      textInput(ns('datasetname'), "Name of dataset", value = "", width = NULL, placeholder = NULL),
                       textInput(ns('comments'), "Comments", value = "", width = NULL, placeholder = "f.e. Done for development tasks of onco"),
                       fileInput(
                         label = "Upload html file",
                         inputId = ns("uploaded_html"),
                         accept = ".html"
                       ),
               actionBttn(
                 inputId = ns("addfile"),
                 label = "Add file",
                 color = "success",
                 style = "material-flat",
                 icon = icon("angle-double-right"),
                 block = TRUE
               )
        )
      )),
    # what to show if manual NOT selected
    conditionalPanel(
      condition = paste0("!","input['", ns("manual"), "']"),
      selectInput(ns("panel"), "Select panel", choices=panel_list)
      
      
    )
    
    
    
    
    )
}

# Server -----------------------------------------------------------------------

mod_docmanagerServer <- function(id, path_to_regressiondata="/var/RCAdata/regression",
                                 pathtohtmls="/home/shinyuser/RCA_rmarkdowns/data/htmlfiles") {
  moduleServer(
    id,
    function(input, output, session) {
      
      #datapathtofile <- input$uploaded_html$datapath
      
      dataset_info_r <- reactive({
        data.frame(filename=list.dirs(path_to_regressiondata, 
                   # pattern = ".RDS$", 
                   full.names = FALSE, 
                   recursive = FALSE),
                   filepath=list.files(path_to_regressiondata, 
                                      pattern = ".RDS$", 
                                      full.names = TRUE, 
                                      recursive = TRUE)) 
        })
      
      observeEvent(input$addfile, {
        # validate(
        #   need(input$panel, paste0(input$panel, " cannot be empty!")),
        #   need(input$author, paste0(input$author, " cannot be empty!")),
        #   need(input$type, paste0(input$type, " cannot be empty!")),
        #   need(input$datasetpath, paste0(input$datasetpath, " cannot be empty!")),
        #   need(input$datasetname, paste0(input$datasetname, " cannot be empty!")),
        #   need(input$comments, paste0(input$comments, " cannot be empty!")),
        #   need(input$addfile, paste0(input$addfile, " cannot be empty!"))
        # )
        if () {

        } else{

        }
      })
      
      
      return()
    }
  )
}
