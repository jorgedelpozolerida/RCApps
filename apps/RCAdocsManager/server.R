## ---------------------------
##
## Script Name:
##
## Purpose of script: SERVER FOR REPORT GENERATOR APP
##
## Author: jorgedelpozolerida
##
## Date Created: 2021-01-15
##
## Email: jorgedelpozolerida@gmail.com
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------
#' TO DO:
#' Make sure it is necessary to source files twice or just in ui function
#' Make sure data table is changing reactively



# Define necessary variables  and import libraries------------------------------

sapply(list.files("../../R", full.names = TRUE), source)

pathsjson_dir <- file.path("/home/shinyuser/RCApps/info/paths.json")

project_dir <- jsonlite::read_json(pathsjson_dir)$RCApps_project

RCA_htmls_path <- file.path(
  jsonlite::read_json(pathsjson_dir)$RCA_htmls_absolute
)

RCA_rmarkdowns_path <- file.path(
  jsonlite::read_json(pathsjson_dir)$RCA_rmarkdowns_project
)

path_to_regressiondata <- file.path(
  jsonlite::read_json(pathsjson_dir)$rocregressiondataframes_absolute
)
  
RCA_htmls_url <- paste0(
  "http://",
  jsonlite::read_json(pathsjson_dir)$server_IP,
  jsonlite::read_json(pathsjson_dir)$RCA_htmls_shinyserverlocation
)

RCA_rmarkdowns_url <- paste0(
  "http://",
  jsonlite::read_json(pathsjson_dir)$server_IP,
  jsonlite::read_json(pathsjson_dir)$RCA_rmarkdowns_shinyserverlocation
)


RCA_rmarkdowns_githubrepo <-  file.path(
  jsonlite::read_json(pathsjson_dir)$RCA_rmarkdowns_githubrepo
)

RCA_htmls_githubrepo <- file.path(
  jsonlite::read_json(pathsjson_dir)$RCA_htmls_githubrepo
)

# Server function ------------------------------------------- ------------------

server <- function(input, output) {

  # ----------------------------- DATA IMPORTING ------------------------------------
  
  # HTMLs
  htmlsummarydata_r <- reactive({
    func_getdocssummary(RCA_htmls_path,
                        RCA_htmls_url,
                        docs_extension=".html")
  })
  htmldataframe_r <- reactive({ htmlsummarydata_r()$data })
  
  mod_dataframeexplorerServer("htmldataframe", htmldataframe_r)
  
  
  # Generate dataframe for rmarkdown docs and explore with module
  
  rmarkdownsummarydata_r <- reactive({
    func_getdocssummary(RCA_rmarkdowns_path,
                        RCA_rmarkdowns_url,
                        docs_extension=".Rmd")
  })
  rmarkdowndataframe_r <- reactive({ rmarkdownsummarydata_r()$data })
  
  mod_dataframeexplorerServer("rmarkdowndataframe", rmarkdowndataframe_r)
  
  
  # ----------------------------- DEBUGGING ------------------------------------
  
  output$text <- renderText({
    # t()
  })
  

  # -------------------------- OUTPUT INFO TAB ---------------------------------
  
  output$infotable <- DT::renderDT(
    {
      data.frame(SERVER_LOCATION=c(RCA_rmarkdowns_path, RCA_htmls_path), 
                 GITHUB_REPO=paste0("<a href='", 
                                    c(RCA_rmarkdowns_githubrepo, RCA_htmls_githubrepo),
                                    "'>", 
                                    c(RCA_rmarkdowns_githubrepo, RCA_htmls_githubrepo), 
                                    "</a>"),
                 row.names=c("Rmarkdowns","HTMLs"))
    },
    escape = FALSE, # to activate hyperlinks
    
  )
  
  
  
  # --------------------- OUTPUT INFO BOXES ---------------------------------

  # HTMLs
  output$n_html <- renderInfoBox({
    infoBox("Number of files",
      icon = icon("file", lib = "glyphicon"), color = "purple",
      value = as.character(htmlsummarydata_r()$n_doc)
    )
  })
  output$n_authors_html <- renderInfoBox({
    infoBox("Number of authors",
      icon = icon("user", lib = "glyphicon"), color = "blue",
      value = as.character(htmlsummarydata_r()$n_authors)
    )
  })
  # Rmarkdowns
  output$n_rmarkdown <- renderInfoBox({
    infoBox("Number of files",
            icon = icon("file", lib = "glyphicon"), color = "purple",
            value = as.character(rmarkdownsummarydata_r()$n_doc)
    )
  })
  output$n_authors_rmarkdowns <- renderInfoBox({
    infoBox("Number of authors",
            icon = icon("user", lib = "glyphicon"), color = "blue",
            value = as.character(rmarkdownsummarydata_r()$n_authors)
    )
  })
  
  # --------------------- OUTPUT ADD FILE TAB ---------------------------------
  
  mod_docmanagerServer("addfile",
                       path_to_regressiondata = path_to_regressiondata,
                       RCA_htmls_path = RCA_htmls_path,
                       RCA_htmls_url = RCA_htmls_url)
  
  

  # --------------------- OUTPUT DROPDOWN MENU---------------------------------

  # Messages
  messageData <- data.frame(from = c("App developer"), message = c("App under development"))

  output$messageMenu <- renderMenu({
    # messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
  })

  # Notifications
  notificationData <- data.frame(text = c("notification1"))
  output$notificationMenu <- renderMenu({
    # notificationData is a data frame with one columns 'text'
    notif <- apply(notificationData, 1, function(row) {
      notificationItem(text = row[["text"]])
    })
  })

  # Tasks
  taskData <- data.frame(text = c("task1"), value = c(90), color = c("green"))
  output$taskMenu <- renderMenu({
    # taskData is a data frame with three columns 'text', 'value', 'color'
    notif <- apply(taskData, 1, function(row) {
      taskItem(text = row[["text"]], color = row[["color"]], value = row[["value"]])
    })
  })
}

