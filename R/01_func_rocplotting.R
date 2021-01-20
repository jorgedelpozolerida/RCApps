## ---------------------------
##
## Script name: Functions to generate plots for ROC study
##
## Purpose of script:
##
## Author: jorgedelpozolerida & ariadnamasot
##
## Date Created: 2020-11-20
##
## Email: jorge.delpozo@qiagen.com & Ariadna.Masot@qiagen.com
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



# Plotting functions -----------------------------------------------------------


# Function for adding vertical line in plotly plot

hline <- function(y = 0, color = "red") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color)
  )
}


# Function to generate all ROC related plots

func_generaterocplots <- function(data, rcch = "4_0", roccutoffs = NULL, th_value = TRUE,
                              prefix='a') {

  # ------------------------- DATA FILTERING -----------------------------------

  filtereddata <- data %>%
    mutate(Group = group) %>%
    filter(rc_ch == rcch) %>% # filter analyte
    select(-rc_ch) %>%
    filter(concordance != "Unknown") # take out unknowns (given by concordance)

  # ----------------- AUTOMATIC THRESHOLD GENERATION ---------------------------


  if (th_value) {
    threshold_value <- func_getthreshold(filtereddata)
  } else {
    threshold_value <- NULL
  }

  # ------------------------- PLOTS GENERATION ---------------------------------


  # ROC curve
  # Do this only when at least two observations...otherwise error
  if (nrow(filtereddata)>1){
    
    roc_plot <- data.frame(
      D = filtereddata$Dsubs, D.str = filtereddata$reference_subs,
      M1 = filtereddata$cp8_dEP, stringsAsFactors = FALSE
    ) %>%
      ggplot(aes(m = M1, d = D)) +
      geom_roc(labelround = 1, cutoffs.at = roccutoffs) +
      style_roc(theme = theme_minimal()) +
      xlab("1 - Specificity") +
      ylab("Sensitivity") +
      scale_y_continuous(
        breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
        labels = c("0", "0.2", "0.4", "0.6", "0.8", "1"),
        limits = c(-0.2, 1.2)
      ) +
      scale_x_continuous(
        breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
        labels = c("0", "0.2", "0.4", "0.6", "0.8", "1"),
        limits = c(-0.2, 1.2)
      )


      auc <- calc_auc(roc_plot)$AUC 
      roc_plot <- roc_plot + annotate("text", x = 0.5, y = 0.5, label = paste0(
        "AUC: ",
        round(auc,
              digits = 3
        )
      ))
      # Interactive print for html (if several on a doc they shoudk have different prefix)
      roc_plot_interactive <- export_interactive_roc(roc_plot,
                               hide.points = TRUE,
                               # add.cis = FALSE,
                               style = NULL,
                               prefix = prefix)
  } else {
    
    roc_plot <- NULL
    roc_plot_interactive <- NULL
    
  }



  # Scatter plot

  scatter_plot <- ggplotly(ggplot(filtereddata, aes(
    ID = id, Group = Group,
    x = cp8_ct, y = cp8_dEP,
    color = concordance
  )) +
    geom_point() +
    scale_y_log10(
      limits = c(1000, 1e6),
      breaks = c(1e1, 1e2, 1e3, 1e4, 1e5),
      labels = c("10", "100", "1k", "10k", "100k")
    ) +
    xlab("Cycle") +
    ylab("Endpoint fluorescence") +
    geom_hline(aes(
      yintercept = threshold_value$th_final,
      text = paste("threshold:", round(threshold_value$th_final,
        digits = 2
      ))
    ),
    color = "black", linetype = "dashed", size = 0.5
    ),
  tooltip = c("id", "Group", "cp8_ct", "cp8_dEP", "concRCA")
  )



  # Density plot

  density_plot <- ggplotly(
    ggplot(filter(filtereddata, cp8_dEP > 0), aes(x = cp8_dEP, fill = reference_RCA)) +
      geom_density(alpha = 0.4) +
      theme_grey() +
      scale_x_log10(
        limits = c(1e3, 1e7),
        breaks = c(1e1, 1e2, 1e3, 1e4, 1e5, 1e6),
        labels = c("10", "100", "1k", "10k", "100k", "1M")
      ) +
      xlab("Endpoint Fluorescence") +
      ylab("Density") +
      geom_vline(aes(
        xintercept = threshold_value$th_final,
        text = paste("threshold:", round(threshold_value$th_final,
          digits = 2
        ))
      ),
      color = "black", linetype = "dashed", size = 0.5
      ) +
      coord_flip()
  )



  # Histogram plot

  hist_plot <- ggplotly(ggplot(filtereddata, aes(
    x = cp8_dEP,
    fill = reference_RCA,
    color = reference_RCA
  )) +
    geom_histogram(alpha = 0.7) +
    geom_vline(aes(
      xintercept = threshold_value$th_final,
      text = paste("threshold:", round(threshold_value$th_final,
        digits = 2
      ))
    ),
    color = "black",
    linetype = "dashed",
    size = 0.5
    ) +
    scale_y_continuous("Counts", trans = "log10", limits = c(NA, 1e3)) +
    scale_x_log10(
      limits = c(1e3, 1e7),
      breaks = c(1e1, 1e2, 1e3, 1e4, 1e5, 1e6),
      labels = c("10", "100", "1k", "10k", "100k", "1M")
    ) +
    coord_flip(), tooltip = )



  # Frequency polygons plot
  freqp_plot <- ggplotly(ggplot(filtereddata, aes(x = cp8_dEP, fill = reference_subs, color = reference_subs, )) +
    geom_freqpoly())

  # Return
  list(roccurve = roc_plot, roccurve_interactive=roc_plot_interactive, density = density_plot, scatter = scatter_plot, hist = hist_plot, freqp = freqp_plot, th = threshold_value$th_final)
}




# Function to generate ROC table

func_makeroctable <- function(data, rcch, cutoffs = NULL, threshold = NULL) {

  # Data filtering
  filtereddata <- data %>%
    mutate(Group = group) %>%
    filter(rc_ch == rcch) %>% # filter analyte
    select(-rc_ch) %>%
    filter(concordance != "Unknown") # take out unknowns (given by concordance)

  # ----------------------- Table making ---------------------------------------
  if (is.null(cutoffs)) {
    filtereddata %>%
      count(Concordance = concRCA) %>%
      mutate("%" = n / sum(n) * 100) %>%
      kable() %>%
      kable_styling(full_width = F)
  }
  else {
    dades <- data.frame(C = c("TP", "TN", "FP", "FN"))
    cutoffs <- append(cutoffs, threshold)
    for (i in 1:length(cutoffs)) {
      filtereddata <- filtereddata %>%
        mutate(!!paste0("P", i) := ifelse(cp8_dEP >= cutoffs[i], "Pos", "Neg")) %>%
        mutate(!!paste0("C", i) := case_when(
          reference_RCA == "Pos" & !!sym(paste0("P", i)) == "Pos" ~ "TP",
          reference_RCA == "Pos" & !!sym(paste0("P", i)) == "Neg" ~ "FN",
          reference_RCA == "Neg" & !!sym(paste0("P", i)) == "Neg" ~ "TN",
          reference_RCA == "Neg" & !!sym(paste0("P", i)) == "Pos" ~ "FP",
          TRUE ~ "0"
        ))
      dades2 <- filtereddata %>%
        select(id, !!paste0("C", i), !!paste0("P", i)) %>%
        group_by(!!sym(paste0("P", i))) %>%
        count(!!sym(paste0("C", i))) %>%
        mutate(!!paste0("cut", i) := n) %>%
        select(-n)

      dades <- left_join(dades, dades2, by = c("C" = paste0("C", i)))
    }

    dades <- dades %>%
      select(-contains("P")) %>%
      t() %>%
      as.data.frame() %>%
      row_to_names(row_number = 1) %>%
      mutate_all(funs(replace_na(., 0))) %>%
      mutate_all(function(x) as.numeric(as.character(x))) %>%
      mutate("Sensitivity (%)" = TP / (TP + FN) * 100, "Specificity" = TN / (TN + FP) * 100, "1-Specificity (%)" = 100 - Specificity) %>%
      mutate("Threshold" = cutoffs, .before = TP) %>%
      rename("Specificity (%)" = Specificity) %>%
      mutate_all(function(x) as.numeric(as.character(x))) %>%
      mutate_at(c("Sensitivity (%)", "Specificity (%)", "1-Specificity (%)"), function(x) format(round(x, 2), nsmall = 2)) %>%
      arrange(Threshold)
  }
  return(dades)
}