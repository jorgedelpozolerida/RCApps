# Import of all necessary libraries





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


# custom R package for interaction with In2Data
library(RCAutils)


# plotting functions
library(plotly)


# complementary shiny packages
library(shinyjs)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(ggpubr) #TO DO: check exact functions
library(stringr)
options(shiny.maxRequestSize=3000*1024^2) # increase shiny maximum upload size

# Others
library(withr)