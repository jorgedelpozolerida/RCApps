# csvlist_path <- file.path(
#   read_json("../../info/paths.json")$project_dir,
#   read_json("../../info/paths.json")$testIDlist_file # path to folder with csv
# )

# User Interface ---------------------------------------------------------------


mod_uselessUI <- function(id,
                                    maxoptions = 8) {
  ns <- NS(id)
  
  tagList(
    textOutput(ns("text"))
  )
}


# Server function --------------------------------------------------------------
mod_uselessServer <- function(id, fromcsv=TRUE, choicelist_r=NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      workingdir_r <- reactive ({ getwd()})
      
      output$text <- renderText({
        paste0("Working directory inside module: ", workingdir_r())
      })
      
      return(
        workingdir_r
      )
    }
  )
}
