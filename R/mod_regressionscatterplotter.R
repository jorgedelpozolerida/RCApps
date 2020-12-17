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
#' Hnalde CH given as number and not 's0', 's1'..

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
                                               messages_r, selected_chambers_r,
                                               zipdatapath_r) {
  moduleServer(
    id,
    function(input, output, session) {
      

      # ---------------------- DATA PROCESSING ---------------------------------

      rc_ch_r <- reactive({
        RC_r <- selected_chambers_r$RC_r
        CH_r <- selected_chambers_r$CH_r #passed as "S"+"integer"
        # Check length 1 for each
        shiny::validate( #important to explicitly put shiny
          need((length(RC_r())==1 & length(CH_r())==1), "Please select only one chamber and one sensor.
          \n If that is the case,
         please wait for data to load... \n If error persists please contact owner of app")
        )
        return(list(RC_r=RC_r,CH_r=CH_r))
      })
      # Get selected RC and Ch for filtering data within modules

      # ----------------------  PLOTs CREATION ---------------------------------
      
      scatterplot_r <- reactive({
        func_exclusionplot(conf_xml_r(), targets_r(), rc_ch_r()$RC_r(), rc_ch_r()$CH_r())
      })
      snrplot_r <- reactive({
        func_snrplot(targets_r(), rc_ch_r()$RC_r(), rc_ch_r()$CH_r())
        
      })
      amplificationplot <- reactive({
        selectedpoint <- event_data("plotly_click")
        func_amplificationfromregressionplot(targets_r(), selectedpoint, zipdatapath_r())
      })
      
      # --------------------------  OUTPUTS ------------------------------------
      
      output$scatter <- renderPlotly({
        subplot(scatterplot_r(), snrplot_r(), amplificationplot(), nrows = 2,
                titleX = TRUE, titleY = TRUE,
                shareY = FALSE) %>%
          layout(plot_bgcolor = 'transparent') %>%
          layout(paper_bgcolor = 'transparent')
          # subplot(scatterplot_r(), SNR_plot(), titleX = TRUE, titleY = TRUE) %>%
          #   layout(plot_bgcolor = 'transparent') %>%
          #   layout(paper_bgcolor = 'transparent') 
          
        
          # plotly_empty()%>%
          #   layout(plot_bgcolor = 'transparent') %>%
          #   layout(paper_bgcolor = 'transparent')
          
      })
      
      
    }
  )
}
