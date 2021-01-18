# Import of all necessary libraries
library(shiny)



# core tidyverse
library(tidyverse) #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats


# for import
library(DBI) #for relational databases
library(httr) #for web APIS (In2Data API here)
library(readxl) #for excel sheets (.xls or .xlsx) 
library(jsonlite) #for JSON
library(XML) #for XML
library(xml2) #for XML


# for wrangle
library(lubridate) #for date and date-times


# for programming challenges
library(magrittr)


# other data manipulation

library(reshape2) 
# library(esquisse) # to explore data quickly

# custom R package for interaction with In2Data
library(RCAutils)


# plotting functions
library(plotly)
#library(ggplot2)
library(plotROC)
library(gganimate) # animated plots
library(magick) #combine two gif
library(yardstick) # ROC curve helper
library(grid)
library(gridExtra)

# complementary shiny packages
options(shiny.maxRequestSize=3000*1024^2) # increase shiny maximum upload size
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(DT)
library(shinyWidgets)
library(htmlwidgets)

# others
library(kableExtra)
library(scales)
library(knitr)
library(rlang)
library(purrr)
library(janitor)
library(withr)
library(ggpubr) #TO DO: check exact functions
library(stringr)
