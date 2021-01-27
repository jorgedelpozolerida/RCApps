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
#' Make valid for other docs rather than rmarkdowns and html (func_getdocsummary)



# Function to get list of Markdowns and its info

func_getdocssummary <- function(docs_path = "/home/shinyuser/RCA_htmls",
                                docs_url = "http://10.156.1.64/RCA_htmls_development/",
                                docs_extension=".html") {
  dataout <- data.frame(
    jsonpath = list.files(docs_path,
      full.names = TRUE,
      recursive = TRUE,
      pattern = ".json$"
    ),
    docpath = list.files(docs_path,
      full.names = TRUE,
      recursive = TRUE,
      pattern = paste0(docs_extension, "$")
    )
  ) %>%
    mutate(jsondata = (map(jsonpath, jsonlite::fromJSON,
      simplifyDataFrame = TRUE
    ))) %>%
    mutate(jsondata = map(jsondata, as.data.frame)) %>%
    unnest(jsondata) %>% 
    relocate(jsonpath, docpath, .after = last_col()) %>% # organize column order
    mutate_at(c("id"), as.integer) %>%
    mutate_at(c("filename"), as.character) %>%
    mutate(date = as.Date(date)) %>% # captured with Sys.Date() so
    mutate_at(c("author"), as.factor) %>%
    mutate(url = paste0(
      docs_url, "/",
      as.character(filename))) %>% 
    # If html cerate full path to html, otherwise url goes to Rmarkdown
    mutate(url=ifelse(docs_extension == ".html", paste0(url, "/", as.character(filename), ".html"), url))%>%
    mutate(filename = paste0("<a href='", url, "'>", filename, "</a>")) %>%
    select(-jsonpath, -docpath, -url) %>%
    arrange((date)) %>% # order by date
    rename_with(toupper) # prittier column names

  n_doc <- int(nrow(dataout))
  n_authors <- nrow(unique(select(dataout, AUTHOR)))
  id_list <- dataout$ID

  return(list(
    data = dataout,
    n_doc = n_doc,
    n_authors = n_authors,
    id_list = id_list
  ))
}




func_generatehtmlrelatedfiles <- function(panel, author, type, datasetpath,
                                          datasetname, comments, uploaded_html,
                                          RCA_htmls_path = "/home/shinyuser/RCA_rmarkdowns/data/htmlfiles") {


  # ------------------------- GENERATE ID AND FILENAME -----------------------

  id_list <- data.frame(id = func_getdocssummary()$id_list) %>%
    mutate(id = as.numeric(id)) %>%
    arrange(desc(id))
  new_id <- as.character(id_list$id[1] + 1) # new id


  # Create new name for file folder, json file and html file. Also create dir.
  file_name <- paste0(c(
    new_id,
    panel,
    type,
    author
  ),
  collapse = "_"
  )
  newdir_name <- file.path(RCA_htmls_path, file_name)
  dir.create(newdir_name)

  # ------------------------------- GENERATE FILES ---------------------------

  # Create list with data for json file --> json file --> write to file
  jsonlist <- list(
    filename = file_name,
    id = new_id,
    author = author,
    date = Sys.Date(),
    type = type,
    panel = panel,
    datasetpath = datasetpath,
    datasetname = datasetname,
    comments = comments
  ) %>%
    # purrr::flatten() %>% # flatten
    # jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% #convert to json file
    jsonlite::write_json(file.path(newdir_name, paste0(file_name, ".json"))) # save list directly better

  # Read uploaded html and write to new dir with new name
  htmlfile <- xml2::read_html(uploaded_html$datapath) %>%
    xml2::write_html(file.path(newdir_name, paste0(file_name, ".html")))

  return(file_name)
}



