## ---------------------------
##
## Script name: DATA ANALYSIS FUNCTIONS
##
## Purpose of script:
##
## Author: jorgedelpozolerida
##
## Date Created: 2021-01-08
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
#' Use mapping functions familty for calculating fractions for each fluorescence value
#' (or use cutpointr package or other optimized)
#'



# Helper Functions -------------------------------------------------------------


# Function to scale data to [0,1]

scale01 <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


# Functions to import regression data ------------------------------------------


# Function to get deata from test.zip file generated when running regression

func_rocdataextractor <- function(filepath, filename = "df.RDS",
                                  listofrcch = c("2_3", "4_0", "6_0", "7_4", "6_4", "8_0"),
                                  zipped = TRUE, extract_counts = FALSE,
                                  threshold_value = 5000) {
  if (zipped) {
    # Connection to  file within zip without opening zip
    con <- unz(filepath, filename = filename)
    con2 <- gzcon(con)
    datain <- readRDS(con2)
    close(con2)
  } else {
    datain <- readRDS(file.path(filepath, filename))
  }

  # Process data and extract concordance
  dataout <- datain %>%
    unnest(metadata) %>%
    select(id, group, targets) %>%
    unnest(targets) %>%
    unite("rc_ch", c(rc, ch), remove = FALSE) %>%
    filter(rc_ch %in% listofrcch) %>%
    select(id, group, rc_ch, ep_cp7, cp8_dEP, cp8_ct, result, expected, concordance) %>%
    rename(reference_system = expected, reference_RCA = result) %>%
    mutate(
      reference_th = replace(reference_system, ep_cp7 < threshold_value, "Neg"),
      reference_subs = replace(reference_system, concordance == "FN", "Neg")
    ) %>%
    # mutate(group = replace(group, group == 'SWAB RP manufacturing protocol', 'SWAB RP' )) %>%
    select(
      id, group, rc_ch, ep_cp7, cp8_dEP, cp8_ct, reference_system, reference_RCA,
      reference_th, reference_subs, concordance
    )
  if (extract_counts) {
    dataout %<>%
      group_by(concordance, rc_ch) %>%
      summarize(count = n()) %>%
      spread(key = concordance, value = count)
  }

  dataout # return
}



# Functions to calculate threshold automatically -------------------------------


# Function to add TPF, FPF columns to regression data

func_getscores <- function(filtereddata, threshold_logscale, weigths) {
  dataout <- filtereddata %>%
    mutate(TPF = NA, FPF = NA) %>%
    arrange(cp8_dEP) %>%
    filter(cp8_dEP > 0) # take out 0 and negative values if any (for logarithms...)

  # Calculate for each fluorescence value used as threshold the TPF and FPF
  for (i in 1:nrow(dataout)) {
    concordancedata <- (dataout$cp8_dEP > dataout$cp8_dEP[i]) %>%
      as.numeric() %>%
      as.data.frame() %>%
      rename(result = ".") %>%
      mutate(Dsys = as.numeric(dataout$Dsys)) %>%
      mutate(Dsubs = as.numeric(dataout$Dsubs)) %>%
      mutate(concordance = case_when(
        Dsubs == "1" & result == "1" ~ "TP",
        Dsubs == "1" & result == "0" ~ "FP",
        Dsubs == "0" & result == "1" ~ "FN",
        Dsubs == "0" & result == "0" ~ "TN"
      ))
    # Count number of True and False Negative, True and False Positive
    TP <- nrow(filter(concordancedata, concordance == "TP"))
    FN <- nrow(filter(concordancedata, concordance == "FN"))
    TN <- nrow(filter(concordancedata, concordance == "TN"))
    FP <- nrow(filter(concordancedata, concordance == "FP"))

    # Calculate True Positive Fraction and False Negative Fraction for each
    tpf <-  TP / (TP + FN)
    fpf <- FP / (TN + FP)
    # Avoid calculation errors
    dataout$TPF[i] <- ifelse(is.nan(tpf), 0, tpf)
    dataout$FPF[i] <- ifelse(is.nan(fpf), 0, fpf)
    
  }

  # return
  dataout %>%
    select(TPF, FPF, cp8_dEP) %>%
    mutate(log_cp8_dEP = log10(cp8_dEP)) %>%
    mutate(distance_log = abs(log_cp8_dEP - threshold_logscale) / threshold_logscale) %>% # distance to threshold_log
    mutate(score = TPF * weigths[1] - FPF * weigths[2] - distance_log * weigths[3]) %>%
    arrange(desc(score))
}


# Function to get distribution threshold and statistics for data


func_getstatsthreshold <- function(filtereddata) {

  # Get summary statistics for data
  summarystats <- filtereddata %>%
    mutate(log_cp8_dEP = log10(cp8_dEP)) %>% # log scale for fluorescence
    filter(cp8_dEP > 0) %>% # take out 0 and negative fluorescence values if any for logarithms.
    group_by(reference_RCA) %>% # reference to RCA call used
    summarise(
      mean = mean(cp8_dEP),
      sd = sd(cp8_dEP),
      median = median(cp8_dEP),
      log_mean = mean(log_cp8_dEP),
      log_sd = sd(log_cp8_dEP),
      log_median = median(log_cp8_dEP)
    )

  # Get stats separated for each case: neg and pos
  negstats <- summarystats %>%
    filter(reference_RCA == "Neg")
  posstats <- summarystats %>%
    filter(reference_RCA == "Pos")


  # Using raw fluorescence: based on median and sd.
  sumsd <- posstats$sd + negstats$sd
  fsdpos <- posstats$sd / sumsd
  fsdneg <- negstats$sd / sumsd

  threshold <- ((posstats$median) * fsdneg + (negstats$median) * fsdpos) # inversely weigthed average
  
  # Using fluorescence in logarithmic scale: based on median and sd
  sumsd_log <- posstats$log_sd + negstats$log_sd
  fsdpos_log <- posstats$log_sd / sumsd_log
  fsdneg_log <- negstats$log_sd / sumsd_log

  threshold_logscale <- ((posstats$log_median) * fsdneg_log + (negstats$log_median) * fsdpos_log)
  threshold_log <- 10^(threshold_logscale)

  # Return NA if empty
  threshold <- ifelse(length(threshold) == 0, NA, threshold)
  threshold_logscale <- ifelse(length(threshold) == 0, NA, threshold_logscale)
  threshold_log <- ifelse(length(threshold) == 0, NA, threshold_log)
  
  
  
  return(list(
    threshold = threshold,
    threshold_logscale = threshold_logscale,
    threshold_log = threshold_log,
    summarystats = summarystats
  ))
}


# Main Function -------------------------------------------------------------


func_getthreshold <- function(filtereddata, weigths = c(0.6, 0.2, 0.2)) {



  # -------------------------- DISPERSION CRITERIA -----------------------------

  # ----- Calculate threshold value based on weigthed average median using ------
  # ----- standard deviation  of Neg and Pos values given by RCA call ----------

  # Get stats and distribution threshold
  stats <- func_getstatsthreshold(filtereddata)
  summarystats <- stats$summarystats
  threshold <- stats$threshold
  threshold_logscale <- stats$threshold_logscale
  threshold_log <- stats$threshold_log

  # -------------------------------- TPF CRITERIA ------------------------------
  # -- Calculate threshold based on a score based on: --------------------------
  #--- TPF, FPF, distance to previous threshold --------------------------------

  data_withscore <- func_getscores(filtereddata, threshold_logscale, weigths)


  # Select value with highest score
  th_score <- 10^(data_withscore$log_cp8_dEP[1])

  # --------------------------- FINAL THRESHOLD --------------------------------

  # Average value between the two previous thresholds
  th_final <- 10^((threshold_logscale + data_withscore$log_cp8_dEP[1]) / 2)

  return(list(
    stats = summarystats,
    th = threshold,
    th_log = threshold_log,
    th_score = th_score,
    th_final = th_final,
    score_info = data_withscore[1, ]
  ))
}


# # Example of application
#
# source("func_ROC.R")
#
# pathtodf <- './dfs/'
#
# data <- func_rocdataextractor(pathtodf, filename='dfgastro.RDS', zipped=FALSE) %>%
#   mutate(Dsys = ifelse(reference_system == 'Pos', 1, 0)) %>%
#   mutate(concRCA = ifelse(concordance == "FN", "TN", concordance)) %>%
#   mutate(Dsubs = ifelse(reference_subs == 'Pos', 1, 0)) %>%
#   mutate(Dth = ifelse(reference_th == 'Pos', 1, 0)) %>%
#   mutate_all(~replace(., is.na(.), 0))
#
# rcch = "4_0"
# roccutoffs=NULL
#
# filtereddata <- data %>%
#   mutate(Group=group) %>%
#   filter(rc_ch == rcch) %>% # filter analyte
#   select(-rc_ch) %>%
#   filter(concordance != "Unknown") # take out unknowns (given by concordance)
#
# threshold <- func_getthreshold(filtereddata)
# threshold
