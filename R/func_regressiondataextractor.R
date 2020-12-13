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
#'



func_dataextractorfromconfigfile <- function(configfilepath) {
  if (is.null(configfilepath)){
    return(NULL)
  }  
  doc <- xmlTreeParse(configfilepath, useInternalNodes = TRUE)
  
  
  
  cfg <- as.data.frame(t(
    xmlSApply(
      getNodeSet(
        doc,
        "//Parameter[@name='FINAL_MIN_AMPLIFICATION_DELTA']"
      ),
      xmlAttrs
    )
  ),
  stringsAsFactors = FALSE
  )
  
  names(cfg)[which(names(cfg) == "reactionChamber")] <- "rc"
  names(cfg)[which(names(cfg) == "channel")] <- "ch"
  tmp <- as.data.frame(
    (xmlSApply(
      getNodeSet(
        doc,
        "//Parameter[@name='FINAL_MIN_AMPLIFICATION_DELTA']"
      ),
      xmlValue
    )
    ),
    stringsAsFactors = FALSE
  )
  
  names(tmp) <- "delta.enpoint"
  cfg <- cbind(cfg, tmp)
  cfg <- cfg[, -which(names(cfg) == "name")]
  
  tmp <- cbind(
    as.data.frame(t(xmlSApply(
      getNodeSet(doc,
                 "//Parameter[@name='ABN_AMPLIFICATION_DELTA']"),
      xmlAttrs)), stringsAsFactors = FALSE),
    as.data.frame((
      xmlSApply(getNodeSet(doc,
                           "//Parameter[@name='ABN_AMPLIFICATION_DELTA']"),
                xmlValue)), stringsAsFactors = FALSE)
  )
  names(tmp)[which(names(tmp) == "reactionChamber")] <- "rc"
  names(tmp)[which(names(tmp) == "channel")] <- "ch"
  tmp <- tmp[, -which(names(tmp) == "name")]
  names(tmp)[ncol(tmp)] <- "delta.enpoint.abn"
  cfg <- merge(cfg, tmp, all.x = TRUE)
  
  tmp <- cbind(
    as.data.frame(t(
      xmlSApply(getNodeSet(doc,
                           "//Parameter[@name='CYCLE_THRESHOLD_ABN_LOW']"),
                xmlAttrs)), stringsAsFactors = FALSE),
    as.data.frame((
      xmlSApply(getNodeSet(doc,
                           "//Parameter[@name='CYCLE_THRESHOLD_ABN_LOW']"),
                xmlValue)), stringsAsFactors = FALSE)
  )
  names(tmp)[which(names(tmp) == "reactionChamber")] <- "rc"
  names(tmp)[which(names(tmp) == "channel")] <- "ch"
  tmp <- tmp[, -which(names(tmp) == "name")]
  names(tmp)[ncol(tmp)] <- "ct.low"
  cfg <- merge(cfg, tmp, all.x = TRUE)
  
  tmp <- cbind(
    as.data.frame(t(
      xmlSApply(getNodeSet(doc,
                           "//Parameter[@name='CYCLE_THRESHOLD_ABN_HIGH']"),
                xmlAttrs)), stringsAsFactors = FALSE),
    as.data.frame((
      xmlSApply(getNodeSet(doc,
                           "//Parameter[@name='CYCLE_THRESHOLD_ABN_HIGH']"),
                xmlValue)), stringsAsFactors = FALSE)
  )
  
  names(tmp)[which(names(tmp) == "reactionChamber")] <- "rc"
  names(tmp)[which(names(tmp) == "channel")] <- "ch"
  tmp <- tmp[, -which(names(tmp) == "name")]
  names(tmp)[ncol(tmp)] <- "ct.high"
  cfg <- merge(cfg, tmp, all.x = TRUE)
  
  tmp <- cbind(
    as.data.frame(t(
      xmlSApply(getNodeSet(doc,
                           "//Parameter[@name='CYCLE_THRESHOLD_MAX']"),
                xmlAttrs)), stringsAsFactors = FALSE),
    as.data.frame((
      xmlSApply(getNodeSet(doc,
                           "//Parameter[@name='CYCLE_THRESHOLD_MAX']"),
                xmlValue)), stringsAsFactors = FALSE)
  )
  
  names(tmp)[which(names(tmp) == "reactionChamber")] <- "rc"
  names(tmp)[which(names(tmp) == "channel")] <- "ch"
  tmp <- tmp[, -which(names(tmp) == "name")]
  names(tmp)[ncol(tmp)] <- "ct.threshold"
  cfg <- merge(cfg, tmp, all.x = TRUE)
  
  tmp <- cbind(
    as.data.frame(t(
      xmlSApply(getNodeSet(doc,
                           "//Parameter[@name='CYCLE_THRESHOLD_MIN']"),
                xmlAttrs)), stringsAsFactors = FALSE),
    as.data.frame((
      xmlSApply(getNodeSet(doc,
                           "//Parameter[@name='CYCLE_THRESHOLD_MIN']"),
                xmlValue)), stringsAsFactors = FALSE)
  )
  
  names(tmp)[which(names(tmp) == "reactionChamber")] <- "rc"
  names(tmp)[which(names(tmp) == "channel")] <- "ch"
  tmp <- tmp[, -which(names(tmp) == "name")]
  names(tmp)[ncol(tmp)] <- "ct.threshold.min"
  cfg <- merge(cfg, tmp, all.x = TRUE)
  
  
  
  cfg$delta.enpoint <- as.numeric(cfg$delta.enpoint)
  cfg$delta.enpoint.abn <- as.numeric(cfg$delta.enpoint.abn)
  cfg$ct.low <- as.numeric(cfg$ct.low)
  cfg$ct.high <- as.numeric(cfg$ct.high)
  cfg$ct.threshold <- as.numeric(cfg$ct.threshold)
  cfg$ct.threshold.min <- as.numeric(cfg$ct.threshold.min)
  
  cfg[is.na(cfg)] <- 0

  # Return config data frame
  return(cfg)
  
}


func_dataextractorfromzipfile <- function(datasetpath) {
  data_path <- datasetpath # get file path
  # return NULL if no zip file selected and exit execution
  if (is.null(data_path)){
    return(NULL)
  }
  
  unzip(data_path$datapath, # unzip file into temporary directory
        exdir = tempdir())   
  
  df <- readRDS(file.path(tempdir(), 'df.RDS')) # read form temporary dir
  
  targ <- df %>%
    select(id, targets, metadata) %>% 
    unnest(metadata) %>%
    select(id, uid, group, targets) %>% 
    unnest(targets) %>% 
    select(id, uid, rc, ch, ct, ep, ep_cp7,
           baseline_pre, cp8_dEP, cp8_snr, cp8_rmse, cp8_ct,
           alpha, beta, a, b, c0, result,
           expected, concordance, group) %>% 
    mutate(idx = paste0(id, rc, ch))
  
  
  mess <- df %>% 
    select(id, messages) %>% 
    unnest(messages) %>% 
    filter(rc != 'NA') %>% 
    mutate_at(vars(rc, ch), as.numeric) %>% 
    mutate(idx = paste0(id, rc, ch))
  
  rm(df) # remove data frame not to slow down execution 
  
  # Return list of two data frames: targets and messages.
  return(list(targets = targ,
              messages = mess ))
  
}

