sapply(list.files("../../R", full.names = TRUE), source)


# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  mod_uselessServer('text2')
  
}