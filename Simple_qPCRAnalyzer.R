# SIMPLE QPCR ANALYZER
library(shiny)
sapply(list.files("./R", full.names = TRUE), source)

#' TO DO:
#' use suppressMessages(library(shiny, quietly = TRUE))?
#' Add name and date on top of each file.
#' In header of each file there should be an agreed header by a convention
#' Add version changes code --> Github


#' MUST:
#' Select Cartridge and allow search
#' Plot Raw Data
#' Plot Processed data
#' Tune plot
#' Present Data in Tabular way
#' separate elements from page (sidebar, mainpanel...etc each into a variable...)
#' decide whether to use sidebar layout or not



# ----------------------- App Directory ----------------------------------------

Simple_qPCRAnalyzer_path <- file.path(
  read_json("./info/paths.json")$project_dir,
  read_json("./info/paths.json")$Simple_qPCRAnalyzer 
)


# ----------------------- Run App ----------------------------------------

runApp(file.path(Simple_qPCRAnalyzer_path))