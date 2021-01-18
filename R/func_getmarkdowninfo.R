## ---------------------------
##
## Script Name:  
##
## Purpose of script: FUNCTIONS TO GET INFO FROM CURRENT MARKDOWNS
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
#'
#' Do not realy on locally set variable to get url to html...



# Function to get list of Markdowns and its info

func_gethtmldocssummary <- function(pathtohtmls, 
                                    url_to_html = "http://10.156.1.64/RCA_html/") {
  
  dataout <- data.frame(jsonpath=list.files(pathtohtmls, 
                                full.names = TRUE, 
                                recursive = TRUE,
                                pattern = ".json$" ),
                       htmlpath=list.files(pathtohtmls,
                                full.names = TRUE, 
                                recursive = TRUE,
                                pattern = ".html$" )) %>%
    mutate(jsondata=(map(jsonpath, jsonlite::fromJSON,  
                         simplifyDataFrame  = TRUE))) %>% 
    mutate(jsondata=map(jsondata, as.data.frame)) %>% 
    unnest(jsondata) %>% 
    relocate(jsonpath, htmlpath, .after = last_col()) %>%  #organize column order
    mutate_at(c("id"), as.integer) %>% 
    mutate_at(c("filename"), as.character) %>%
    mutate(date = as.Date(date)) %>% # captured with Sys.Date() so 
    mutate_at(c( "author", "date", "type"), as.factor) %>% 
    mutate(url=paste0(url_to_html,
           as.character(filename), "/", as.character(filename), ".html")) %>% 
    mutate(filename=paste0("<a href='", url, "'>", filename, "</a>")) %>% 
    select(-jsonpath, -htmlpath, -url) %>% 
    arrange((date)) %>% # order by date 
    rename_with(toupper) # prittier column names

  n_html <- int(nrow(dataout))
  n_authors <- nrow(unique(select(dataout, AUTHOR)))
  
  return(list(
    data = dataout,
    n_html = n_html,
    n_authors = n_authors
  )
  )
}
