# FUNCTIONS FOR IMPORTING DATA FROM EIITHER CSV FILES LOCAL FOLDER OR FROM IN2DATA.



#' TO DO:
#' More direct extraction of raw data! (intermediate processing steps probably not
#' needed by using RCAutils)
#' Better to have data loaded once or twice one per metadata and once per data inside importerfromin2data
#' allow multiple cartridges data
#' add all possible options to extract data to func_dataImporter final function
#' add more possibilities to typeofdata input of func_dataImporter
#' make data tidy inside mod_rawdataimproter not after
#' maybe pass nested data and then unest out of module





# Necessary variables --------------------------------------------------------------

csvfiles_dir <- file.path(
  read_json("./info/paths.json")$project_dir,
  read_json("./info/paths.json")$testCSV_dir # path to folder with csv
)


# Necessary functions --------------------------------------------------------------


#' Function to import metadata and calls/analyte data from In2Data
#'
#' @param testID Character string containing ID of test desired to extract data from
#'
#'
#'
#' @return list containing the following components
#' \describe{
#'   \item{meta}{Data frame containing: uid|in2DataID|CartridgeSN|sample_id|startTimestamp|
#'   AM|OM|ADF_name|ADF_version|total_result|error_code|IC|AM_firmware,OM_version,
#'   Barcode|PatientID|sample_type|Lot|Assay(ADF_name)|ADF_version}
#'   \item{calling}{Data frame containing: id|name|RC|Sensor|result|baseline|
#'   chamber|ct|ep}
#' @noRd


analytedataImporter_fromIn2Data <- function(testID) {

  # To convenient input format for RCAutils function ("testID" set as column name)
  to_dataframe <- as.data.frame(testID)

  # Load data from In2Data
  data <- RCAutils::generateCartridge(to_dataframe)

  # Create metadata (naming convention for variables consistent with Theia)
  metadata <- data %>%
    select(
      uid, id, cartridge_SN, sample_id, start_time, AM, OM, ADF_name,
      ADF_version, total_result, error_code, IC, AM_firmware,
      OM_version, Barcode, Patient_ID, sample_type, lotNumber, ADF_name,
      ADF_version
    ) %>%
    rename(
      # rename to have same format as in Theia app
      in2DataID = id,
      CartridgeSN = cartridge_SN,
      PatientID = Patient_ID,
      Lot = lotNumber,
      Assay = ADF_name,
      AssayVersion = ADF_version,
      startTimestamp = start_time
    ) %>%
    mutate(
      startTimestamp = as_datetime(as.numeric(as.character(startTimestamp)) / 1000),
      startTimestamp = as.character(startTimestamp)
    )
  # Handle case when no metadata for cartridge
  if (metadata$total_result == "fail") {
    calls <- data.frame(
      RC = "No amplifications",
      ct = "No amplifications",
      Sensor = "No amplifications"
    )
  } else {
    calls <- data %>%
      select(
        id, contains("Chamber"), contains("controls."), -contains("color"),
        -contains("label"), -contains("shortName")
      ) %>%
      melt('id') %>%
      separate(variable, c("chamber", "Sensor", "variable"), "[.]") %>%
      select(-chamber, Sensor) %>%
 
      spread(key = variable, value = value) %>%
      mutate(
        RC = str_extract(str_extract(Sensor, "RC[1-8]"), "[1-8]"),
        Sensor = str_extract(Sensor, "S[0-5]")
      ) %>%
      mutate_all(na_if, 0) %>%
      mutate_all(na_if, "0.0") %>%
      select(id, name, RC, Sensor, result, everything()) %>%
      mutate(
        result = str_extract(result, "[a-z]+"),
        ep = as.numeric(str_replace(ep, "[,]", ""))
      )
  }

  return(list(
    # metadata
    meta = metadata,
    # analyte data
    calling = calls
  ))
}


#' Function to import raw data from csv folder
#'
#' @param testID Character string vector containing ID of test desired to extract data from
#'
#'
#'
#' @return data, Data frame fo raw data containing RC|Cycle|S0:S5|qPCR.Temp|

#' @noRd

rawdataImporter_fromcsv <- function(testID) {
  
  print(paste0('inside datimporter: \n', getwd()))
  # Read all cartridges/the cartridge at once
  filepaths <- testID %>% 
    as.data.frame()
  names(filepaths)<-'ID'
  filepaths %<>% 
    mutate(filepath=file.path(csvfiles_dir, paste0(ID, '.csv'))) # TO DO: avoid possible name conflicts
  
  data <- filepaths %>% 
    as.data.frame() %>% 
    mutate(csv= map(filepath, read_tsv, col_names=TRUE, col_types = 
                      '-iiii-iiid--')) %>% 
    unnest(csv) %>% 
    select(-filepath) %>% 
    group_by(ID, Cycle, RC) %>%
    slice_head(n=8) %>% # only first readings
    ungroup() %>%
    filter(Cycle > 0) %>% 
    rename(testID=ID, qPCRTemp='qPCR Temp')
  return(data)
  
}

#' Function that aggregates the previous functions
#'
#' @param testID Character string containing ID of test desired to extract data from
#' @param fromIn2Data, Logical value controlling whether to extract from In2Data or locally
#' @param typeofdata, Logical value controlling whether to extract raw data or other type.
#' Allowed options: 'raw', 'analyte', 'metadata'...
#'
#' @return data, Data frame containing selected info (depends on input values)
#'

func_dataImporter <- function(testID, fromIn2Data = FALSE, typeofdata = "raw") {

  if (fromIn2Data) {
    switch(typeofdata,
      raw = NULL, # TO DO: build function to extract csv from in2data,
      analyte = analytedataImporter_fromIn2Data(testID)$calling,
      metadata = analytedataImporter_fromIn2Data(testID)$meta,
      stop('Invalid "typeofdata" value')
    )
  } else {
    switch(typeofdata,
      raw = rawdataImporter_fromcsv(testID), # TO DO: build function to extract csv from in2data,
      analyte = NULL, # TO DO: build function to extract from local data
      stop('Invalid "typeofdata" value')
    )
  }
}
