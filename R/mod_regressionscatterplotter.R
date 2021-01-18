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

mod_regressionscatterplotterServer <- function(id, 
                                               conf_xml_r, 
                                               targets_r, 
                                               messages_r, 
                                               selected_chambers_r,
                                               zipdatapath_r, 
                                               selectedparams) {
  moduleServer(
    id,
    function(input, output, session) {
      

      # ---------------------- DATA PROCESSING ---------------------------------

      # Ensure reactivity trigger by grouping all 
      params_r <- reactive({
        RC_r <- selected_chambers_r$RC_r
        CH_r <- selected_chambers_r$CH_r #passed as "S"+"integer"
        # Check length 1 for each
        shiny::validate( #important to explicitly put shiny
          need((length(RC_r())==1 & length(CH_r())==1), "Please select only one chamber and one sensor.
          \n If that is the case,
         please wait for data to load... \n If error persists please contact owner of app")
        )
        study_r = selectedparams$study_r
        TTsymbol_r = selectedparams$TTsymbol_r
        TTcolor_r = selectedparams$TTcolor_r
        threshold_r = selectedparams$threshold_r
        return(list(
          RC_r = RC_r,
          CH_r = CH_r, 
          study_r = study_r,
          TTsymbol_r = TTsymbol_r,
          TTcolor_r = TTcolor_r,
          threshold_r = threshold_r
          ))
      })
      

      # ----------------------  PLOTs CREATION ---------------------------------
      
      scatterplot_r <- reactive({
        func_exclusionplot(conf_xml_r(),
                           targets_r(),
                           params_r()$RC_r(), 
                           params_r()$CH_r(),
                           params_r()$study_r(), 
                           params_r()$TTsymbol_r(),  
                           params_r()$TTcolor_r(),
                           params_r()$threshold_r())
      })
      snrplot_r <- reactive({
        func_snrplot(targets_r(),
                     params_r()$RC_r(),
                     params_r()$CH_r(), 
                     params_r()$study_r(),
                     params_r()$TTsymbol_r(),  
                     params_r()$TTcolor_r(),
                     params_r()$threshold_r())
        
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
          
      })
      
      
    }
  )
}
