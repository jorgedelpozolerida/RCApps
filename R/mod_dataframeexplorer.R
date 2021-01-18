## ---------------------------
##
## Script name:  MODULE FOR EXPLORING DATA FRAMES DYNAMICALLY
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
#' Input column width (now all equalk theoretically)


# Create variables -------------------------------------------------------------



# User Interface ---------------------------------------------------------------

mod_dataframeexplorerUI <- function(id) {
  
  ns <- NS(id)

  tagList(
    DT::dataTableOutput(outputId = ns("table"))
  )
}

# Server -----------------------------------------------------------------------

mod_dataframeexplorerServer <- function(id, dataframe_r) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Calculate width each column must have
      width_col <- reactive({ as.integer( 100/ncol(dataframe_r()) )})
      
      output$table <- DT::renderDT(
        {
          dataframe_r()
        },
        class = 'cell-border stripe',
        server = TRUE, # server-processing (when used some functions do not work like select extension to DT)
        filter = list(
          position = 'top', clear = FALSE
        ),
        # editable=TRUE,
        escape = FALSE, # to activate hyperlinks
        options = list(
          pageLength = 10, 
          autoWidth = TRUE, # otherwise not able to set col width
          dom = 'T<"clear">lfrtip',
          columnDefs = list(list(width = paste0(width_col(), "%") #otherwise columsn change size when searching
                                 # targets=list()
                                 )),
          deferRender=TRUE,
          scrollX=TRUE,
          scrollY=400,
          # scrollCollapse=TRUE,
          pageLength = 100, 
          lengthMenu = c(10,50,100,200)
          # select=list(sSwfPath = copySWF('www'), aButtons=c('copy','csv','print'))
        )
        # extensions = list(Scroller=NULL, TableTools=NULL, FixedColumns=list(leftColumns=2))
        # searching=TRUE,
        # autoWidth = TRUE,
        # columnDefs = list(list(width = '200px', targets = "_all"))
      )
      return(width_col)
    }
  )
}
