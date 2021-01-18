sapply(list.files("../../R", full.names = TRUE), source)


# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("App for checking module functioning"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    mod_uselessUI('text2')
  ),
  
  # Main panel for displaying outputs ----
  mainPanel()
)