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

pathsjson_dir <- file.path("/home/shinyuser/RCApps/info/paths.json")

project_dir <- jsonlite::read_json(pathsjson_dir)$RCApps_project

RCA_htmls_path <- file.path(
  jsonlite::read_json(pathsjson_dir)$RCA_htmls_absolute
)

path_to_regressiondata <- file.path(
  jsonlite::read_json(pathsjson_dir)$rocregressiondataframes_absolute
)
  
RCA_htmls_url <- file.path(
  jsonlite::read_json(pathsjson_dir)$RCA_htmls_url
)
  
pathtoRfiles <- file.path(
  jsonlite::read_json(pathsjson_dir)$RCApps_project,
  "/R"
)

sapply(list.files(pathtoRfiles, full.names = TRUE), source)



# Server function ------------------------------------------- ------------------

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  summarydata_r <- reactive({
    func_gethtmldocssummary(RCA_htmls_path)
  })
  htmldataframe_r <- reactive({
    summarydata_r()$data
  })

  t <- mod_dataframeexplorerServer("htmldataframe", htmldataframe_r)
  output$text <- renderText({
    t()
  })

  # --------------------- OUTPUT INFO TAB ---------------------------------
  
  output$infotable <- DT::renderDT(
    {
      data.frame(PATH=RCA_htmls_path, row.names=c("HTMLs"))
    },
 
  )
  
  
  
  # --------------------- OUTPUT INFO BOXES ---------------------------------

  output$n_html <- renderInfoBox({
    infoBox("Number of HTML files",
      icon = icon("file", lib = "glyphicon"), color = "purple",
      value = as.character(summarydata_r()$n_html)
    )
  })
  output$n_authors <- renderInfoBox({
    infoBox("Number of authors",
      icon = icon("user", lib = "glyphicon"), color = "blue",
      value = as.character(summarydata_r()$n_authors)
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "80%",
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
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

