## ---------------------------
##
## Script name: CARTRIDGE SELECTOR MODULE
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
#' make clear why use label variable or set a label for each item in TagList
#' use read_csv instead?
#' observe or observeevent
#' select by default the last cartridge run!
#' take out default 180116166 seledcted and choices
#' control better csv file upload... 
#' Explore UpdateSelectixe option: server side, more options...etc
#' eXplain format of csv to upload for multiple cartridges
#' handle file input better: data type, size not limited, handle cases...etc




# Necessary variables ----------------------------------------------------------

csvlist_path <- file.path(
  read_json("./info/paths.json")$project_dir,
  read_json("./info/paths.json")$testIDlist_file # path to folder with csv
)

# User Interface ---------------------------------------------------------------


#' Cartridge Selector module user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param maxoptions, numeric integer setting number of displayed options in scroll down bar
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements


mod_cartridgeSelectorUI <- function(id,
                                    maxoptions = 8) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        8,
        conditionalPanel(
          condition = paste0("!","input['", ns("multi"), "']"), # in javaScript
          selectInput(ns("selected_cartridge"),
                      label = "Cartridge Selected",
                      choices = NULL,
                      selected = NULL, 
                      multiple = TRUE # TO DO: solve passing null...
                      # options = list(maxOptions = maxoptions)
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("multi"), "']"), # in javaScript
          fileInput(ns("listfile"), label = "Cartridge list file (.csv)")
        )
      ),
      column(
        4,
        checkboxInput(ns("multi"), "Import cartridge list"),
        
      )
    )
  )
}

# Server function --------------------------------------------------------------


#' Cartridge Selector module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#'
#' @return list with following components
#' \describe{
#'   \item{testID}{reactive character string indicating testID selected}
#' }

mod_cartridgeSelectorServer <- function(id, fromcsv=TRUE, choicelist_r=NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      # Get csv list to display options in UI
      cartridge_list <- read.csv(file.path(csvlist_path),
        header = TRUE,
        sep = ";"
      )
      # Update UI possible options using this list
      observe(
        updateSelectInput(session,
          "selected_cartridge",
          choices = if (fromcsv) cartridge_list$testID else choicelist_r(),
          selected = if (fromcsv) cartridge_list$testID[1] else NULL),
          # server:	whether to store choices
        # on the server side, and load the select
        # options dynamically on searching, instead
        # of writing all choices into the page at once
      )
      #

      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(
          need(input$listfile, message = FALSE),
          need(
            tools::file_ext(input$listfile$datapath) == "csv",
            "Please upload a csv file"
          )
        )
        input$listfile
      })

      return(
        list(
          testID = reactive({
            if (input$multi){
              selected <- read.csv(userFile$datapath, header=FALSE, col.names = c("testID")) %>% 
                as.list()
              selected$testID
            } else {
              input$selected_cartridge
            }
          })
        )
      )
    }
  )
}
