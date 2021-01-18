## ---------------------------
##
## Script name: MODULE FOR SELECTING DESIRED SENSOR, CHAMBER AND CROSSTALK CORRECTION.
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
#' Solve showing  




# Create variables -------------------------------------------------------------

RC_choices <- list(
  "RC 1" = 1,
  "RC 2" = 2,
  "RC 3" = 3,
  "RC 4" = 4,
  "RC 5" = 5,
  "RC 6" = 6,
  "RC 7" = 7,
  "RC 8" = 8
)

CH_choices <- list(
  "CH 0" = "S0",
  "CH 1" = "S1",
  "CH 2" = "S2",
  "CH 3" = "S3",
  "CH 4" = "S4",
  "CH 5" = "S5"
)

CH_xtalk_choices <- list(
  "CH 0 corrected" = "S0_xtalk",
  "CH 1 corrected" = "S1_xtalk",
  "CH 2 corrected" = "S2_xtalk",
  "CH 3 corrected" = "S3_xtalk",
  "CH 4 corrected" = "S4_xtalk",
  "CH 5 corrected" = "S5_xtalk"
)



# ui function --------------------------------------------------------------

mod_chamberSelectorUI <- function(id) {
  
  ns <- NS(id)
  
  # three columns, each containing one group of checkboxes
  tagList(
    fluidRow(
      column(
        4,
        checkboxGroupInput(
          ns("RC"),
          label = "Reaction chamber",
          choices = RC_choices,
          selected = 1
        )
      ),
      column(
        4,
        checkboxGroupInput(
          ns("CH"),
          label = "Channel",
          choices = CH_choices,
          selected = "S0"
        )
      ),
      column(
        4,
        conditionalPanel(
          condition = "output.showxtalk", # in javaScript
          checkboxGroupInput(
            ns("CH_xtalk"),
            label = "X-talk Correction",
            choices = CH_xtalk_choices,
            selected = NULL
          )
        )
      )
    ),
    # three columns, each containing 'check all' option for each group
    fluidRow(
      column(
        4,
        checkboxInput(ns("checkallRC"), "Check all")
      ),
      column(
        4,
        checkboxInput(ns("checkallCH"), "Check all")
      ),
      column(
        4,
        conditionalPanel(
          condition = "output.showxtalk", # in javaScript
          checkboxInput(ns("checkallxtalk"), "Check all")
        )
        
      )
    ),
    conditionalPanel(
      condition = "output.showfaceting", # in javaScript
      materialSwitch(ns("faceting"), "Faceting", FALSE,
                     right = TRUE,
                     status = "info"
      )
    )
    
  )
}


# Server Function --------------------------------------------------------------


#' Chamber selection module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#'
#' @return list with following components
#' \describe{
#'   \item{CH_xtalk}{reactive character vector with selected sensors where 
#'   to apply cross-talk}
#'   \item{CH}{reactive character vector with selected sensors}
#'   \item{RC}{reactive character vector with selected reaction chambers}
#'   \item{faceting}{reactive character string indicating whether to apply faceting}

#' }

mod_chamberSelectorServer <- function(id,
                                      showxtalk=FALSE,
                                      showfaceting=TRUE) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Create bool outputs for controlling what is shown
      output$showxtalk <- reactive({ showxtalk })
      output$showfaceting <- reactive({ showfaceting })
      
      # Update checkboxinputs 
      observe({
        updateCheckboxGroupInput(
          session, "RC",
          choices = RC_choices,
          selected = if (input$checkallRC) RC_choices else 1 # if TRUE select all options, same for the rest
        )
      })
      observe({
        updateCheckboxGroupInput(
          session, "CH",
          choices = CH_choices,
          selected = if (input$checkallCH) CH_choices else "S0"
        )
      })
      observe({
        updateCheckboxGroupInput(
          session, "CH_xtalk",
          choices = CH_xtalk_choices,
          selected = if (input$checkallxtalk) CH_xtalk_choices else NULL
        )
      })

      return(
        # return a list containing 4 reactive elements 
        list(
          CH_xtalk_r = reactive({
            input$CH_xtalk #reactive character vector
          }),
          CH_r = reactive({
            input$CH #reactive character vector
          }),
          RC_r = reactive({
            input$RC #reactive character vector
          }),
          faceting_r = reactive({
            input$faceting #reactive boolean/logical
          })
        )
      )
    }
  )
}
