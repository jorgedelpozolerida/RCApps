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
#' See if name conflicts between modules for manual or automatic...
#' Handle automatic generation using Rmarkdown or apps

# Necessary variables -------------------------------------------------------------

panel_list <- list(
  Meningitis = "men",
  Gastroinstestinal = "gastro",
  Oncology = "onco",
  Respiratory = "respi"
)

doctype_list <- list(
  ROCcurves_study = "ROCcurves",
  Scatterplots_Analysis = "Scatter"
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
        column(
          6,
          selectInput(ns("panel"), "Select panel", choices = panel_list),
          textInput(ns("type"), "Document type", value = "", width = NULL, placeholder = "f.e. ROCcurves"),
          textInput(ns("author"), "Author name", value = "", width = NULL, placeholder = "f.e jdelpozo"),
          textInput(ns("datasetpath"), "Path to dataset", value = "", width = NULL, placeholder = NULL)
        ),

        column(
          6, textInput(ns("datasetname"), "Name of dataset", value = "", width = NULL, placeholder = NULL),
          textInput(ns("comments"), "Comments", value = "", width = NULL, placeholder = "f.e. Done for development tasks of onco"),
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
      )
    ),
    # what to show if manual NOT selected
    conditionalPanel(
      condition = paste0("!", "input['", ns("manual"), "']"),
      fluidPage(
        column(
          6,
          selectInput(ns("autopanel"), "Select panel", choices = panel_list),
          selectInput(ns("autotype"), "Document type", choices = doctype_list),
          textInput(ns("author"), "Author name", value = "", width = NULL, placeholder = "f.e jdelpozo")
        ),
        # textInput(ns('datasetpath'), "Path to dataset", value = "", width = NULL, placeholder = NULL)),

        column(
          6,
          selectInput(ns("autodatasetname"), "Name of dataset", choices = NULL),
          textInput(ns("comments"), "Comments", value = "", width = NULL, placeholder = "f.e. Done for development tasks of onco"),
          actionBttn(
            inputId = ns("generatefile"),
            label = "Generate file",
            color = "success",
            style = "material-flat",
            icon = icon("angle-double-right"),
            block = TRUE
          )
        )
      )
    )
  )
}

# Server -----------------------------------------------------------------------

mod_docmanagerServer <- function(id, path_to_regressiondata = "/var/RCAdata/regression",
                                 pathtohtmls = "/home/shinyuser/RCA_rmarkdowns/data/htmlfiles") {
  moduleServer(
    id,
    function(input, output, session) {


      # Get list of available and selectable datasets and update SelectInput in UI
      dataset_info_r <- reactive({
        data.frame(
          filename = list.dirs(path_to_regressiondata,
            # pattern = ".RDS$",
            full.names = FALSE,
            recursive = FALSE
          ),
          filepath = list.files(path_to_regressiondata,
            pattern = ".RDS$",
            full.names = TRUE,
            recursive = TRUE
          )
        )
      })

      # ----------------------- MANUAL PROCESSING ------------------------------
      
      observeEvent(input$addfile, {
        
        if (all(nzchar(c(input$panel, input$author, input$type, input$datasetpath, 
                         input$datasetname, input$comments)), 
                !is.null(input$uploaded_html))) {
          
          # Generate all related files and get name
          file_name <- func_generatehtmlrelatedfiles(input$panel, input$author, input$type, input$datasetpath, 
                                        input$datasetname, input$comments, input$uploaded_html,
                                        pathtohtmls=pathtohtmls)


          # If succesfull, show message
          showModal(modalDialog(title = "File succesfully added!", paste0(
            "You're file has been added to the repository. Name of file: ", "\n",
            file_name,
            "Refresh page to see changes reflected"
          )))
        }
        else {
          showModal(modalDialog(title = "Warning!!!", "Please fill all the fields before you click the Add File button!!!"))
        }
      })

      # -------------------- AUTOMATIC GENERATION ------------------------------
      
      # When button "generatefile" is clicked, this happens
      observe(
        updateSelectInput(session,
                          "datasetname",
                          choices = unique(dataset_info_r()$filename),
                          selected = NULL
        )
      )
      
      return()
    }
  )
}
