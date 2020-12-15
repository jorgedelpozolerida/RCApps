## ---------------------------
##
## Script name: FUNCTIONS FOR CREATING DIFFERENT TYPES OF PLOTS
##
## Purpose of script:
##
## Author: jorgedelpozolerida
##
## Date Created: 2020-12-14
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
#' Use plotlyoutput 
#' Subset by group also
#' allow select grouping factor in scatter
#' manually set the symbols in scatter plots, shape pallete auto only 6, i have more
#' Valid symbols include:
#' 0', 'circle', '100', 'circle-open', '200', 'circle-dot', '300', 'circle-open-dot', '1', 'square', '101', 'square-open', '201', 'square-dot', '301', 'square-open-dot', '2', 'diamond', '102', 'diamond-open', '202', 'diamond-dot', '302', 'diamond-open-dot', '3', 'cross', '103', 'cross-open', '203', 'cross-dot', '303', 'cross-open-dot', '4', 'x', '104', 'x-open', '204', 'x-dot', '304', 'x-open-dot', '5', 'triangle-up', '105', 'triangle-up-open', '205', 'triangle-up-dot', '305', 'triangle-up-open-dot', '6', 'triangle-down', '106', 'triangle-down-open', '206', 'triangle-down-dot', '306', 'triangle-down-open-dot', '7', 'triangle-left', '107', 'triangle-left-open', '207', 'triangle-left-dot', '307', 'triangle-left-open-dot', '8', 'triangle-right', '108', 'triangle-right-open', '208', 'triangle-right-dot', '308', 'triangle-right-open-dot', '9', 'triangle-ne', '109', 'triangle-ne-open', '209', 'triangle-ne-dot', '309', 'trian [... truncated]
#' Too slow scatter plot..solve


# Helper functions ----------------------------------------------------------


vline <- function(x = 0, color = "blue") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color)
  )}

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

logistic <- function(c, alpha, beta, a, b, co){
  alpha * c + beta + (a / (1 + (c / co) ^ b))
}








# Amplification Plots ----------------------------------------------------------


#' Return ggplot for given data
#'
#' @param data Data frame containing RC|Cycle|S0:S5|qPCRTemp|
#' @param title
#' @param xlabel
#' @param ylabel
#' @param caption
#' @param faceting Logical value that controls whether to apply faceting or not
#'
#' @noRd

func_amplificationplot <- function(data, title, xlabel, ylabel, current_testID,
                                       caption = "", faceting = FALSE) {
  plotout <- data %>%
    mutate(RC = paste0("RC", as.character(RC))) %>%
    ggplot(aes(x = Cycle, y = Fluorescence)) +
    geom_point(size = 0.5) +
    labs(title = title, x = xlabel, y = ylabel, caption = caption) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      panel.background = element_rect(
        fill = "#d8e7ed", colour = "#ffffff",
        size = 2, linetype = "solid"
      )
    )
  
  if (faceting) {
    plotout <- plotout + facet_grid(Sensor ~ RC, scales = "free")
    
    if (length(current_testID > 1)) {
      plotout <- plotout + geom_line(aes(color = testID), size = 1)
    }
  } else {
    if (length(current_testID > 1)) {
      plotout <- plotout +
        geom_line(aes(color = ID_RC_Sensor), size = 1) # variable ID_RC_Sensor created in server
    } else {
      plotout <- plotout +
        geom_line(aes(color = RC_Sensor), size = 1) # variable RC_Sensor created in server
    }
  }
  
  return(plotout)
}




# Exclusion plot / Scatter Plot ------------------------------------------------

func_exclusionplot <- function(conf_xml, targets, RC, CH){
  
  CH <- gsub(".*?([0-9]+).*", "\\1", CH) #  extract numeric part of string
  tmp <- conf_xml %>% 
    mutate(rc=as.character(rc), ch=as.character(ch)) %>% 
    filter(rc == RC, ch == CH)
  shiny::validate( #important to explicitly put shiny
    need((nrow(tmp)==1), "Please select only one chamber and one sensor, 
         or a valid combination that contains data. \n If that is the case,
         please wait for data to load...")
  )
  if (!(nrow(tmp)==1)) {
    message('Please select only one chamber and one sensor')
    return(NULL)
  }
  # Get values in separate vectors: must be of length 1, preselected ch and rc in module server
  ep_th <- tmp$delta.enpoint
  epvcct_th <- tmp$delta.enpoint.abn
  ct_low <- tmp$ct.low
  ct_high <- tmp$ct.high
  ct_treshold <- tmp$ct.threshold
  ct.threshold.min <- tmp$ct.threshold.min
  # print(paste0(RC, CH,ep_th,ct.threshold.min))
  
  exclusionplot <- targets %>% 
    filter(rc == as.character(RC), ch == as.character(CH)) %>%  # TO DO: group %in% input$study_checkbox, Sample %in% input$sample_checkbox --> not in theia
    select(id, uid, rc, ch, idx, cp8_ct, cp8_dEP, cp8_snr, group, 
             concordance, result) %>% 
    plot_ly(
      colors = c("dark blue", "magenta", "green", 'black')
    ) %>%
    add_lines(x = c(7,ct_low),
              y = c(epvcct_th,epvcct_th),
              line = list(color = 'red', width = 2, dash = 'dot'),
              fill = "tozeroy",
              fillcolor = 'beige',
              name = "EPvsCT threshold",
              showlegend = TRUE) %>%
    add_lines(x = seq(ct_low,ct_high, 0.1),
              y = (seq(ct_low,ct_high, 0.1) - ct_low) /
                (ct_high - ct_low) * (ep_th - epvcct_th) + epvcct_th,
              line = list(color = 'red', width = 2, dash = 'dot'),
              fill = "tozeroy",
              fillcolor = 'beige',
              name = "EPvsCT threshold",
              showlegend = TRUE) %>%
    add_lines(x = c(ct_high,41),
              y = c(ep_th,ep_th),
              line = list(color = 'red', width = 2, dash = 'dot'),
              fill = "tozeroy",
              fillcolor = 'beige',
              name = "EP threshold",
              showlegend = TRUE) %>%
    add_markers(x = ~cp8_ct,
                y = ~cp8_dEP,
                color = ~as.factor(concordance), # TO DO: allow select grouping factor
                symbol = ~as.factor(group), # TO DO: allow select grouping factor
                key = ~idx,
                text = ~paste0('<b>ID:</b> ', id, '<br></br>',
                               '<b>UID:</b> ', uid, '<br></br>',
                               '<b>dEP/rmse:</b> ', round(as.numeric(cp8_snr),2), '<br></br>',
                               '<b>Group:</b> ', group, '<br></br>',
                               '<b>Result:</b> ', result, '<br></br>')) %>%
    layout(yaxis = list(type = "log")) %>%
    layout(title = paste0(
      "Exclusion criteria for RC ", RC, ' and CH ',
      CH))%>%
    layout(xaxis = list(title = 'Ct (cycles)'),
           yaxis = list(title = 'dEP (A.U.)')) %>%
    layout(shapes = list(vline(ct_treshold),
                         vline(ct.threshold.min))) %>% 
                         # hline(input$th))) %>% # To DO: add hline for threshold user selected
    layout(xaxis = list(showgrid = TRUE,
                        gridcolor = toRGB("grey10", alpha = 0.2)),
           yaxis = list(showgrid = TRUE,
                        gridcolor = toRGB("grey10", alpha = 0.2)))

  # Return plotly plot
  exclusionplot
}

# SNR plot ---------------------------------------------------------------------
# SNR_plot <- reactive({
#   p2<-  subset(dataset_data()[['targets']],
#                ch == input$CH_Tunning &
#                  rc == input$RC_Tunning &
#                  (group %in% input$study_checkbox 
#                   # & Sample %in% input$sample_checkbox
#                  )) %>%
#     plot_ly() %>%
#     add_markers(x = ~cp8_snr,
#                 y = ~cp8_dEP,
#                 # colors = c("coral", "blueviolet", "limegreen"),
#                 color = ~as.factor(get(input$Trace_Tunning_color)),
#                 symbol = ~as.factor(get(input$Trace_Tunning_symbol)),
#                 key = ~idx,
#                 text = ~paste0('<b>ID:</b> ', id, '<br></br>',
#                                '<b>UID:</b> ', uid, '<br></br>',
#                                '<b>dEP/rmse:</b> ', round(as.numeric(cp8_snr),2), '<br></br>',
#                                '<b>Ct:</b> ', cp8_ct, '<br></br>',
#                                '<b>Group:</b> ', group, '<br></br>',
#                                '<b>Result:</b> ', result, '<br></br>'),
#                 showlegend = TRUE) %>%
#     layout(xaxis = list(type = "log"), yaxis = list(type = "log"))%>%
#     layout(title = paste0(
#       "Exclusion criteria for RC ", input$RC_Tunning, ' and CH ',
#       input$CH_Tunning))%>%
#     layout(xaxis = list(title = 'SNR'),
#            yaxis = list(title = 'dEP (A.U.)')) %>% 
#     layout(xaxis = list(showgrid = TRUE,
#                         gridcolor = toRGB("grey10", alpha = 0.2)),
#            yaxis = list(showgrid = TRUE,
#                         gridcolor = toRGB("grey10", alpha = 0.2)))
#   
# })
# 
# 
