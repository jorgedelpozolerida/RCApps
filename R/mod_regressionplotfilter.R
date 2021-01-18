## ---------------------------
##
## Script name: MODULE FOR SELECTING DATA FOR SCATTER PLOT
##
## Purpose of script:
##
## Author: jorgedelpozolerida
##
## Date Created: 2020-12-22
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



# Necessary variables ----------------------------------------------------------

Trace_Tunning_symbolchoices <- list(
  "Group" = "group",
  "Analyzer" = "Analyzer",
  "Sample" = "Sample"
)

Trace_Tunning_colorchoices <- list(
  "Result" = 'result',
  "Group" = 'group',
  "Analyzer" = 'Analyzer',
  "Sample" = 'Sample',
  "Assay Perf" = 'concordance'
)

# User Interface ---------------------------------------------------------------

mod_regressionplotfilterUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      selectizeInput(
        inputId = ns("study_checkbox"),
        label = "Select study",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      selectizeInput(
        inputId = ns("Trace_Tunning_symbol"),
        label = "Select symbol category",
        choices = Trace_Tunning_symbolchoices,
        selected = "group"
      ),
      selectizeInput(
        inputId = ns('Trace_Tunning_color'),
        label = "Select color category",
        choices = Trace_Tunning_colorchoices,
        selected = 'concordance'),
      numericInput(
        inputId = ns('th'),
        label = 'Threshold:',
        10,
        min = 1,
        max = 1e6)
      # downloadButton(ns('downloadScatter'),
      #                label = 'Download Scatter Plot')
    )
  )
}


# Server function --------------------------------------------------------------

mod_regressionplotfilterServer <- function(id, targets_r) {
  moduleServer(
    id,
    function(input, output, session) {
      
      studychoices_r <- reactive( {sort(unique(targets_r()[['group']])) })
      
      # Update selectize input 
      observe(
        updateSelectInput(session,
                          "study_checkbox",
                          choices = studychoices_r() ,
                          selected = studychoices_r() )
        
      )
      
      # output$downloadScatter <- downloadHandler(
      #   filename = function(){
      #     paste0('Scatter_', Sys.Date(), '.html')
      #   },
      #   content = function(file){
      #     htmlwidgets::saveWidget(scatter_plot(), file)
      #   }
      # )
      # 
      return(list(
        study_r = reactive({ input$study_checkbox }),
        TTsymbol_r = reactive({ input$Trace_Tunning_symbol }),
        TTcolor_r = reactive ({ input$Trace_Tunning_color }),
        threshold_r = reactive ({ input$th })
        
      ))
    }
  )
}
