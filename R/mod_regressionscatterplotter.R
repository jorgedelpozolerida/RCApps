## ---------------------------
##
## Script name: MODULE FOR PLOTTING REGRESSION SCATTER
##
## Purpose of script:
##
## Author: jorgedelpozolerida
##
## Date Created: 2020-12-14
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
#' Manage heigt of fluidpage and so...
#' Handle empty plot case with no rows as in theia


# User Interface --------------------------------------------------------------


mod_regressionscatterplotterUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      plotlyOutput(ns("scatter"), height = "875px") # TO DO: manage introducing column
    )
  )
}


# Server -----------------------------------------------------------------------

mod_regressionscatterplotterServer <- function(id, conf_xml_r, targets_r, 
                                               messages_r, selected_chambers_r) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # ---------------------- DATA PROCESSING ---------------------------------

      # Get selected RC and Ch for filtering data within modules
      RC_r <- selected_chambers_r$RC_r
      CH_r <- selected_chambers_r$CH_r #passed as "S"+"integer"
      

      # ----------------------  PLOTs CREATION ---------------------------------
      
      scatter_plot_r <- reactive({
        func_exclusionplot(conf_xml_r(), targets_r(), RC_r(), CH_r())
      })
      
      
      # --------------------------  OUTPUTS ------------------------------------
      
      output$scatter <- renderPlotly({
        
          scatter_plot_r()
          # subplot(scatter_plot_r(), SNR_plot(), titleX = TRUE, titleY = TRUE) %>%
          #   layout(plot_bgcolor = 'transparent') %>%
          #   layout(paper_bgcolor = 'transparent') 
          
        
          # plotly_empty()%>%
          #   layout(plot_bgcolor = 'transparent') %>%
          #   layout(paper_bgcolor = 'transparent')
        
      })
      
      return()
    }
  )
}
