## ---------------------------
##
## Script name: REGRESSIONANALYZER APP
##
## Purpose of script: To run Server and UI for RegressionAnalyzer app.
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
#'


# Library Import ---------------------------------------------------------------

library(shiny)
# suppressMessages(library(shiny, quietly = TRUE))
sapply(list.files("./R", full.names = TRUE), source)


# App Directory ----------------------------------------------------------------

RegressionAnalyzer_path <- file.path(
  read_json("./info/paths.json")$project_dir,
  read_json("./info/paths.json")$RegressionAnalyzer 
)

# Run App ----------------------------------------------------------------------

runApp(file.path(RegressionAnalyzer_path))
