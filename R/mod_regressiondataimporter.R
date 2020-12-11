## ---------------------------
##
## Script name:  MODULE FOR SELECTING DATA FROM REGRESSION
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




mod_regressiondataimporterUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      fileInput(label = 'Select Dataset',
                inputId = ns('input_dataset'),
                accept = c('.zip'),
                multiple = FALSE),
      fileInput(label = 'Select configuration file',
                inputId = ns('input_config'),
                accept = '.xml')
    )
    )
}


mod_regressiondataimporterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Load regression datazip and return relevant data: messages and targets
      dataset_data <- reactive({
        
        data_path <- input$input_dataset # get file path
        # return NULL if no zip file selected and exit execution
        if (is.null(data_path)){
          return(NULL)
        }
        
        unzip(data_path$datapath, # unzip file into temporary directory
              exdir = tempdir())   
        
        df <- readRDS(file.path(tempdir(), 'df.RDS')) # read form temporary dir
        
        targets <- df %>%
          select(id, targets, metadata) %>% 
          unnest(metadata) %>%
          select(id, uid, group, targets) %>% 
          unnest(targets) %>% 
          select(id, uid, rc, ch, ct, ep, ep_cp7,
                 baseline_pre, cp8_dEP, cp8_snr, cp8_rmse, cp8_ct,
                 alpha, beta, a, b, c0, result,
                 expected, concordance, group) %>% 
          mutate(idx = paste0(id, rc, ch))
        
        
        messages <- df %>% 
          select(id, messages) %>% 
          unnest(messages) %>% 
          filter(rc != 'NA') %>% 
          mutate_at(vars(rc, ch), as.numeric) %>% 
          mutate(idx = paste0(id, rc, ch))
        
        rm(df) # remove data frame not to slow down execution 
        
        return(list(targets = reactive({ targets }),
                    messages = reactive({ messages })))
        
      })
      
      # Load config file data
      conf_xml <- reactive({
        cfg_path <- input$input_config
        cfg <- func_dataextractorfromconfigfile(cfg_path$datapath)
        # return config data frame. TO DO: necessary to write return?
        return(cfg)
      })
      
      
      return(list(
        data = dataset_data,
        conf = conf_xml
      )) 
    }
  )
}
