## ---------------------------
##
## Script Name:
##
## Purpose of script: UI FOR REPORT GENERATOR APP
##
## Author: jorgedelpozolerida
##
## Date Created: 2021-01-15
##
## Email: jorgedelpozolerida@gmail.com
##
## ---------------------------
##
## Notes: Dashboardpage for ui is composed of three elements:
## -header
## -sidebar
## -body
##
##
## ---------------------------
#' TO DO:
#'
#'

# Define necessary variables  and import libraries------------------------------

# pathsjson_dir <- file.path("../../info/paths.json")
#
# project_dir <- jsonlite::read_json(pathsjson_dir)$RCA_rmarkdowns_project
#
# pathtohtmls <- file.path(
#   jsonlite::read_json(pathsjson_dir)$htmlfiles_absolute
# )
#
# pathtoRfiles <- file.path(
#   jsonlite::read_json(pathsjson_dir)$RCApps_project,
#   "/R"
# )

sapply(list.files("../../R", full.names = TRUE), source)



# Header ---------------------------------------------------------------------------


header <- dashboardHeader(
  title = "RCA markdowns Explorer",
  # title = shinyDashboardLogo(
  #   theme = "blue_gradient",
  #   boldText = "RCA",
  #   mainText = "markdowns Explorer",
  #   # badgeText = ""
  # ),
  dropdownMenu(dropdownMenuOutput("messageMenu"), type = "message"),
  dropdownMenu(dropdownMenuOutput("notificationMenu"), type = "notifications"),
  dropdownMenu(dropdownMenuOutput("taskMenu"), type = "tasks")
)


# Siderbar ------------------------------------------------------------------



sidebar <- dashboardSidebar(
  # id = "sidebar", # necessary in case one want to bookmark and restore seleted tab
  sidebarSearchForm(
    textId = "searchText", buttonId = "searchButton",
    label = "Search..."
  ),
  sidebarMenu(
    id = "idebarmenu",
    menuItem("Summary", tabName = "summary", icon = icon("binoculars")),
    menuItem("Info", tabName = "info", icon = icon("book")),
    menuItem("Add file", tabName = "addfile", icon = icon("plus"))
  )
)


# Body -------------------------------------------------------------------------


body <- dashboardBody(

  # Enable shinyjs
  shinyjs::useShinyjs(),

  #------------------------------ SET THEME ------------------------------------

  shinyDashboardThemes(
    theme = "blue_gradient"
  ),

  #------------------------------ TAB ITEMS ------------------------------------

  tabItems(
    # Dashboard tab
    tabItem(
      tabName = "summary",
      fillPage(
        fluidRow(
          # Dynamic infoBoxes
          infoBoxOutput("n_html"),
          infoBoxOutput("n_authors"),
          infoBoxOutput("approvalBox")
        ),
        fluidRow(
          box(
            title = "List of HTML files", status = "primary", # can also be success/warning/info/danger/primary
            solidHeader = TRUE, collapsible = TRUE, width = NULL,
            # background = "maroon",
            # module inside
            mod_dataframeexplorerUI("htmldataframe"),

            # tabBox(
            #   title = tagList(shiny::icon("gear"), "tabBox"),
            #   id='tabs',
            #   side = "right",
            #   height = "250px",
            #   selected = "Tab3",
            #   tabPanel("Tab1", "Tab content 1"),
            #   tabPanel("Tab2", "Tab content 2"),
            #   tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
            # ),
            # box(width = 4, actionButton("count", "Increment progress")
          )
        )
      )
    ),
    
    # Add file tab
    tabItem(
      tabName = "info",
      DT::dataTableOutput(outputId = "infotable")
    ),
    # Add file tab
    tabItem(
      tabName = "addfile",
      
      mod_docmanagerUI("addfile")
    )
  )
)






# UI function ------------------------------------------------------------------

ui <- dashboardPage(header, sidebar, body)
