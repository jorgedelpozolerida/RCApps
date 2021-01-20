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

func_gethtmldocssummary <- function(pathtohtmls = "/home/shinyuser/RCA_rmarkdowns/data/htmlfiles",
                                    url_to_html = "http://10.156.1.64/RCA_html/") {
  dataout <- data.frame(
    jsonpath = list.files(pathtohtmls,
      full.names = TRUE,
      recursive = TRUE,
      pattern = ".json$"
    ),
    htmlpath = list.files(pathtohtmls,
      full.names = TRUE,
      recursive = TRUE,
      pattern = ".html$"
    )
  ) %>%
    mutate(jsondata = (map(jsonpath, jsonlite::fromJSON,
      simplifyDataFrame = TRUE
    ))) %>%
    mutate(jsondata = map(jsondata, as.data.frame)) %>%
    unnest(jsondata) %>%
    relocate(jsonpath, htmlpath, .after = last_col()) %>% # organize column order
    mutate_at(c("id"), as.integer) %>%
    mutate_at(c("filename"), as.character) %>%
    mutate(date = as.Date(date)) %>% # captured with Sys.Date() so
    mutate_at(c("author", "date", "type"), as.factor) %>%
    mutate(url = paste0(
      url_to_html,
      as.character(filename), "/", as.character(filename), ".html"
    )) %>%
    mutate(filename = paste0("<a href='", url, "'>", filename, "</a>")) %>%
    select(-jsonpath, -htmlpath, -url) %>%
    arrange((date)) %>% # order by date
    rename_with(toupper) # prittier column names

  n_html <- int(nrow(dataout))
  n_authors <- nrow(unique(select(dataout, AUTHOR)))
  id_list <- dataout$ID

  return(list(
    data = dataout,
    n_html = n_html,
    n_authors = n_authors,
    id_list = id_list
  ))
}




func_generatehtmlrelatedfiles <- function(panel, author, type, datasetpath,
                                          datasetname, comments, uploaded_html,
                                          pathtohtmls = "/home/shinyuser/RCA_rmarkdowns/data/htmlfiles") {


  # ------------------------- GENERATE ID AND FILENAME -----------------------

  id_list <- data.frame(id = func_gethtmldocssummary()$id_list) %>%
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
  newdir_name <- file.path(pathtohtmls, file_name)
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



