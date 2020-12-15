## ---------------------------
##
## Script name: FUNCTIONS TO EXTRACT AND PROCESS DATA FROM REGRESSION 
##
## Purpose of script:
##
## Author: jorgedelpozolerida
##
## Date Created: 2020-12-11
##
## Email: jorge.delpozo@qiagen.com / jorgedelpozolerida@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------
##
#' TO DO:
#'
#'Imprpve computing time for zip function


# Necessary variables --------------------------------------------------------------


attributes <- c(
  "FINAL_MIN_AMPLIFICATION_DELTA",
  "ABN_AMPLIFICATION_DELTA",
  "CYCLE_THRESHOLD_ABN_LOW",
  "CYCLE_THRESHOLD_ABN_HIGH",
  "CYCLE_THRESHOLD_MAX",
  "CYCLE_THRESHOLD_MIN"
)

attribute_name <- c(
  "delta.enpoint",
  "delta.enpoint.abn",
  "ct.low",
  "ct.high",
  "ct.threshold",
  "ct.threshold.min"
)


# Necessary functions --------------------------------------------------------------

func_extractattributefromconfigxml <- function(att,
                                         att_name,
                                         xmldoc) {
  # Get values
  tmp <- as.data.frame(
    # Apply a function to subnodes before they are returned to R (quicker approach)
    (xmlSApply(
      # Find matching nodes using XPath syntax
      getNodeSet(
        xmldoc,
        # Syntax: selects all the Parameter elements that have a "name" attribute
        # with a value of "attribute"
        paste0("//Parameter[@name='", att, "']")
      ),
      xmlValue # function: extract or set the contents of a leaf XML node
    )
    ),
    stringsAsFactors = FALSE
  ) %>%
    set_colnames(c(att_name))

  # Get RC and Sensor(CH) id and bind to previous
  cfg <- as.data.frame(t(
    xmlSApply(
      getNodeSet(
        xmldoc,
        paste0("//Parameter[@name='", att, "']")
      ),
      xmlAttrs # function: returns a named character vector giving the name-value
      # pairs of attributes of an XMLNode object
    )
  ),
  stringsAsFactors = FALSE # character vector converted to a factor
  ) %>%
    rename(rc = "reactionChamber", ch = "channel") %>% # rename columns of dataframe
    cbind(tmp) %>% # bind to values
    select(-name) # deselect name col

  # Return configuration info for that attribute
  cfg
}


# Core functions ---------------------------------------------------------------


func_dataextractorfromconfigfile <- function(configfilepath) {


  # ------------------------- LOAD XML FILE AND CONTROL EMPTY VALUES------------

  if (is.null(configfilepath)) {
    return(NULL)
  }

  # Generate R structure representing XML tree
  xmldoc <- xmlTreeParse(configfilepath, useInternalNodes = TRUE)

  # ------------------------- EXTRACT INFO FROM ALL ATRIBUTTES AND JOIN---------
  configurationdata <- map2(
    attributes, # variable defined in the beginning of the file
    attribute_name, # same
    func_extractattributefromconfigxml,
    xmldoc = xmldoc
  ) %>%
    reduce(left_join, by = c("rc", "ch")) %>%
    mutate_all(replace_na, replace = 0) %>%  # replace all na
    mutate_all(as.numeric) %>% # make numeric 
    mutate_at(c('rc','ch'), as.factor) # turn rc and ch character
  
  # Return config data frame
  configurationdata
}

func_dataextractorfromzipfile <- function(datasetpath) {


  # ------------------------- LOAD ZIP FILE AND READ ---------------------------
  
  con <- unz(datasetpath, filename = "df.RDS") # reads(only) single file within zip in binary mode
  con2 <- gzcon(con) # wraps the existing connection "con", and decompresses reads through "con"
  df <- readRDS(con2)
  close(con2)


  # ------------------------- EXTRACT MESSAGES AND TARGETS ---------------------
  targ <- df %>%
    select(id, targets, metadata) %>%
    unnest(metadata) %>%
    select(id, uid, group, targets) %>%
    unnest(targets) %>%
    mutate_at(c("id", "uid","rc","ch"), as.factor) %>% # TO DO: investigate implications of having these as charcter
    select(
      id, uid, rc, ch, ct, ep, ep_cp7,
      baseline_pre, cp8_dEP, cp8_snr, cp8_rmse, cp8_ct,
      alpha, beta, a, b, c0, result,
      expected, concordance, group
    ) %>%
    mutate(idx = paste0(id, rc, ch))


  mess <- df %>%
    select(id, messages) %>%
    unnest(messages) %>%
    filter(rc != "NA") %>%
    mutate(idx = paste0(id, rc, ch)) %>% 
    mutate_at(c("id","rc", "ch","cp","codeblock","result","idx"), as.factor) # TO DO: investigate implications of having these as charcter

    
  rm(df) # remove data frame not to slow down execution by overloadinf cache

  # Return list of two data frames: targets and messages.
  return(list(
    targets = targ,
    messages = mess
  ))
}
