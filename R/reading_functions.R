library(tidyverse)

# Function to obtain the metadata sheet from the metadata folder.
# 
#  Input: Expedetion name, folder name.
# Output: a tibble containing the sampling day metadata 
#         with an additional `meta_to_site` column

download_meta_sheet <- function(expedition_name, folder_name){
  metadata_id <- googledrive::drive_get(
    path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/Metadata/{folder_name} - metadata"))$id
  meta_table <- googlesheets4::read_sheet(metadata_id,"metadata")
  
  if (unique(meta_table$Project) %in% projects$`Tel Aviv Transects`)
  {meta_table <- mutate(meta_table,meta_to_deployment_id = str_glue(
    "{`First Observer`} and {`Second Observer`} - {Spot}"))}
  if (unique(meta_table$Project) %in% projects$`Mediterranean Transects`)
  {meta_table <- mutate(meta_table,meta_to_deployment_id = str_glue(
    "{`First Observer`} and {`Second Observer`} - {SiteID}"))}
  if (unique(meta_table$Project) %in% projects$`Eilat Transects`)
  {meta_table <- mutate(meta_table,meta_to_deployment_id = str_glue(
    "{`First Observer`} and {`Second Observer`} - {SiteID}"))}
  if (unique(meta_table$Project) %in% projects$`Eilat Knolls`)
  {meta_table <- mutate(meta_table,meta_to_deployment_id = str_glue(
    "{`First Observer`} and {`Second Observer`} - {KnollID}"))}
  
  return(meta_table)
}

# Function to modify deployment metadata of tel aviv observer tables.
# A helper function for `read_metadata_columns` function.
#
#  Input: a tibble
# Output: a modified tibble with a Fish/Invertebrates column named "Group"

modify_telaviv_deployment_metadata <- function(deployment_metadata_tbl,sheet_identifier){
  deployment_metadata_tbl %>% 
    mutate(Group = if_else(str_detect(sheet_identifier, "Fish"),"Fish", "Invertebrates")) %>% 
    return()
}

# Function to modify deployment metadata of eilat transects observer tables.
# A helper function for `read_metadata_columns` function.
#
#  Input: a tibble
# Output: a modified tibble with a T/C column named "Letter"

modify_eilat_transects_deployment_metadata <- function(deployment_metadata_tbl,sheet_identifier){
  deployment_metadata_tbl %>% 
    mutate(Letter = if_else(str_detect(sheet_identifier,"TRANSIENTS"),"T","C")) %>% 
    return()
}

# Function to read the two deployment metadata columns in 
# a worksheet within a spreadsheet.
# A helper function for `read_worksheet` function.
#
#  Input: sampling day metadata tibble obtained by `download_meta_sheet`,
#         and a worksheet tibble
# Output: A tibble containing the two metadata columns in
#         a wide format.

read_metadata_columns <- function(day_metadata, worksheet,sheet_identifier){
  deployment_metadata <- worksheet[,1:2] %>%
    filter(!is.na(Metadata)) %>% 
    pivot_wider(names_from = Metadata, names_sort = FALSE, values_from = Value) %>% 
    mutate(across(.fns = function(x) type.convert(x,as.is = T))) 
  
  # googlesheets4 does not support hms class at the moment, so we will keep times as chr
  # %>% 
  #   mutate(across(.cols = all_of(c("Time Start","Time End")),
  #                 .fns = hms::parse_hm))
  
  if (unique(day_metadata$Project) %in% projects$`Tel Aviv Transects`){
    return(modify_telaviv_deployment_metadata(deployment_metadata,sheet_identifier))
  }
  if (unique(day_metadata$Project) %in% projects$`Eilat Transects`){
    return(modify_eilat_transects_deployment_metadata(deployment_metadata,sheet_identifier))
  }
  return(deployment_metadata)
}

# Function to read the observer table and
# arrange them according to the project
# A helper function for `read_worksheet` function.
#
#  Input: sampling day metadata tibble obtained by `download_meta_sheet`,
#         and a worksheet tibble
# Output: A tibble describing the species survey data

read_observer_worksheet <- function(day_metadata, worksheet,sheet_identifier){
  observer_worksheet <- worksheet[,-c(1:2)]
  number_of_columns <- ncol(observer_worksheet)
  
  if (unique(day_metadata$Project) %in% projects$`Tel Aviv Transects`){
    observer_worksheet %>% 
      filter(across(any_of(c("Invertebrate|Taxon","Abundance",
                             "Species","Amount","Length","Distance")),
                    .fns = function(x) !is.na(x))) %>% 
      return()
  }
  else{
    half_point <- number_of_columns/2 # where first observer ends 
    first_observer_columns <- 1:half_point
    second_observer_columns <- (1 + half_point):number_of_columns
    
    
    # some regex to get column name without the "...NUMBER"   
    # addition from google sheets (ie. "Observer...3" -> "Observer")
    columns <- str_split(colnames(observer_worksheet),
                         pattern = "\\.\\.\\.",simplify = T)[,1][first_observer_columns] 
    
    first_observer_tbl <-  observer_worksheet %>% 
      select(all_of(first_observer_columns)) %>% 
      rename_all( ~ columns)
    
    second_observer_tbl <- observer_worksheet %>% 
      select(all_of(second_observer_columns)) %>% 
      rename_all( ~ columns)
    
    bind_rows(first_observer_tbl,second_observer_tbl) %>% 
      filter(across(any_of(c("Observer","Species","Amount","Length")),
                    .fns = function(x) !is.na(x))) %>% 
      return()
  }
}

# Function to bind sample metadata with sample data
#
#  Input: sampling day metadata tibble obtained by `download_meta_sheet`,
#         spreadsheet ID, and a worksheet identifier
# Output: A tibble describing the species survey data with sample metadata

read_worksheet <- function(day_meta, spreadsheet_id, sheet_identifier){
  worksheet <- googlesheets4::read_sheet(spreadsheet_id,sheet_identifier,col_types = "c")
  message(glue::glue("Waiting for 10 seconds between worksheet"))
  Sys.sleep(10)
  sample_metadata <- read_metadata_columns(day_meta, worksheet,sheet_identifier)
  sample_data <- read_observer_worksheet(day_meta, worksheet,sheet_identifier)
  bind_cols(sample_metadata,sample_data) %>% 
    return()
}

# Function to read quadrates data and metadata, and bind them together
#
#  Input: spreadsheet ID and a worksheet identifier
# Output: A tibble describing the quadrate survey data with quadrates metadata

read_quadrate_worksheet <- function(spreadsheet_id, sheet_identifier){
  quadrate_worksheet <- googlesheets4::read_sheet(spreadsheet_id,sheet_identifier,col_types = "c")
  message(glue::glue("Waiting for 10 seconds between worksheet"))
  Sys.sleep(10)
  quadrate_metadata <- quadrate_worksheet[,1:2] %>%
    filter(!is.na(Metadata)) %>% 
    pivot_wider(names_from = Metadata, names_sort = FALSE, values_from = Value) %>% 
    mutate(across(.fns = function(x) type.convert(x,as.is = T))) 
    quadrate_data <- quadrate_worksheet[,-c(1:2)] %>% 
    filter(!is.na(Quadrate)) %>% 
    mutate(across(.fns = function(col) replace(x = col, is.na(col), 0)))
  bind_cols(quadrate_metadata,quadrate_data) %>% 
    return()
}

# Function to join quadrate data with the invertebrate sampling data
#
#  Input: worksheet identifier of the quadrate sheets,
#         the worksheets read by the first part of `read_deployment_spreadsheet`,
#         and a spreadsheet ID object of the deployment.
# Output: A tibble describing the quadrate survey data with quadrates metadata

add_quadrate_data <- function(quadrates, all_worksheets, spreadsheet_id){
  all_quadrate_worksheets <- lapply(quadrates, function(sheet_identifier)
    read_quadrate_worksheet(spreadsheet_id, sheet_identifier))
  
  all_quadrate_worksheets_df <- bind_rows(all_quadrate_worksheets)
  
  lapply(all_worksheets, function(worksheet){
    if (unique(worksheet$Group) == "Invertebrates"){
      return(left_join(worksheet,all_quadrate_worksheets_df,
                       by = c("Fish Observer", "Invertebrate Observer","Site","Transect","Quadrate")))
    } else {
      return(worksheet)
    }
  })
}

# Function to read all worksheets in a deployment spreadsheet
#
#  Input: sampling day metadata tibble obtained by `download_meta_sheet`,
#         spreadsheet ID object of the deployment.
# Output: A tibble containing all sample data and metadata within the deployment

read_deployment_spreadsheet <- function(day_metadata, spreadsheet_id){
  samples <- googlesheets4::sheet_properties(googlesheets4::as_sheets_id(spreadsheet_id)) %>% 
    filter(!str_detect(string = name,"Species List|Readme.txt|Quadrate data")) %>% 
    .$name
  
  all_worksheets <- lapply(samples, function(sheet_identifier)
    read_worksheet(day_metadata, spreadsheet_id, sheet_identifier))
  
  if (unique(day_metadata$Project) %in% projects$`Tel Aviv Transects`){
    
    quadrates <- googlesheets4::sheet_properties(googlesheets4::as_sheets_id(spreadsheet_id)) %>% 
      filter(str_detect(string = name,"Quadrate data")) %>% 
      .$name
    
    if (length(quadrates) > 0) {
      all_worksheets <- add_quadrate_data(quadrates, all_worksheets,spreadsheet_id)
      }
    
    all_worksheets %>% 
      bind_rows %>% 
      group_by(Site, Transect) %>% 
      fill(`Time Start`,`Time End`,`Depth Start`,`Depth End`) %>% 
      ungroup() %>% 
      return()
  }
  else {
    lapply(all_worksheets, function(df) {
      mutate(df,across(.fns = as.character))
    }) %>% 
      bind_rows() %>%
      return()
    }
}


# Function to read all spreadsheets in a folder.
# Feature: Added ability to supply a vector of
#          spreadsheets names to be read.
#
#  Input: Expedition name, folder name, 
#         day metadata tibble, optional: a vector of 
#         spreadsheet names.
# Output: A tibble containing all data from the 
#         supplied folder or alternatively, from
#         selected spreadsheets.

read_sampling_day_data <- function(day_metadata, expedition_name, folder_name, names = NULL){
  if (is.null(names)){
    spreadsheets <- googledrive::drive_ls(str_glue("~/Data Sheets/{expedition_name}/{folder_name}/")) %>% 
      filter(!name  %in% c("Metadata","Photos","COMPLETE DATA")) %>% 
      .$id
  }
  else{
    spreadsheets <- googledrive::drive_ls(str_glue("~/Data Sheets/{expedition_name}/{folder_name}/")) %>% 
      filter(name  %in% names) %>% 
      .$id
  }
  
  sampling_day_data <- lapply(spreadsheets, function(spreadsheet_id){
    read_deployment_spreadsheet(day_metadata, spreadsheet_id)
  })

  sampling_day_data %>%
    bind_rows() %>%
    return()
}

# Function to join day metadata tibble with sampling data
# Feature: Can upload complete tibble to a COMPLETE DATA
#          folder within the day's folder.
# 
#  Input: Expedition name, folder name, optional: 
#         logical value stating whether to upload
#         or not (DEFAULT)
# Output: A tibble containing all of the sampling day data,
#         alternatively - uploaded into a created folder within
#         folder name.

download_day_complete_data <- function(expedition_name, folder_name,upload = FALSE){
  day_metadata <- download_meta_sheet(expedition_name, folder_name)
  day_sample_data <- read_sampling_day_data(day_metadata, expedition_name, folder_name)
  
  if (unique(day_metadata$Project) %in% projects$`Tel Aviv Transects`)
  {day_sample_data <- mutate(day_sample_data,meta_to_deployment_id = str_glue(
    "{`Fish Observer`} and {`Invertebrate Observer`} - {Site}"))}
  if (unique(day_metadata$Project) %in% projects$`Mediterranean Transects`)
  {day_sample_data <- mutate(day_sample_data,meta_to_deployment_id = str_glue(
    "{`First Observer`} and {`Second Observer`} - {Site}"))}
  if (unique(day_metadata$Project) %in% projects$`Eilat Transects`)
  {day_sample_data <- mutate(day_sample_data,meta_to_deployment_id = str_glue(
    "{`First Observer`} and {`Second Observer`} - {Site}"))}
  if (unique(day_metadata$Project) %in% projects$`Eilat Knolls`)
  {day_sample_data <- mutate(day_sample_data,meta_to_deployment_id = str_glue(
    "{`First Observer`} and {`Second Observer`} - {Knoll}"))}
  
  day_complete_data <- left_join(day_metadata,day_sample_data, by = c("meta_to_deployment_id"))
  
  if (upload) {
    googledrive::drive_mkdir(name = "COMPLETE DATA",overwrite = TRUE,
                             path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/"))
    day_spreadsheet <- googlesheets4::gs4_create(name = str_glue("{folder_name}"),
                                                  sheets = list(Data = day_complete_data))
    googledrive::drive_mv(file = day_spreadsheet, 
                          path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/COMPLETE DATA/"))
  }
  
  return(day_complete_data)
}

# Function to download individual days data, join them, 
# and upload to a EXPEDITION DATA folder.
#  Input: Expedition name,
#         optional: whether to upload individual day data
# Output: A tibble containing all of the expedition data,
#         uploaded into a created folder within
#         expedition directory

download_expedition_data <- function(expedition_name, upload = FALSE){
  folders <- googledrive::drive_ls(str_glue("~/Data Sheets/{expedition_name}/"),verbose = FALSE) %>% 
    filter(name != "EXPEDITION DATA") %>% 
    .$name
  
  days_data <- lapply(folders, function(folder_name){
    message(glue::glue("Downloading data from {folder_name}"))
    if (upload) {
      message(glue::glue("{folder_name} data uploaded"))
    }
    return(download_day_complete_data(expedition_name, folder_name, upload))
  })
  
  days_data <- days_data %>% bind_rows
  
  googledrive::drive_mkdir(name = "EXPEDITION DATA",overwrite = TRUE,
                           path = str_glue("~/Data Sheets/{expedition_name}/"))
  all_data <- googlesheets4::gs4_create(name = str_glue("{expedition_name}"),
                                               sheets = list(Data = days_data))
  googledrive::drive_mv(file = all_data, 
                        path = str_glue("~/Data Sheets/{expedition_name}/EXPEDITION DATA/"))
  
  return(days_data)
}


# Function to join individual day complete data, 
# and upload to a EXPEDITION DATA folder.
#  Input: Expedition name
# Output: A tibble containing all of the expedition data,
#         uploaded into a created folder within
#         expedition directory
combine_days_data <- function(expedition_name){
  folders <- googledrive::drive_ls(str_glue("~/Data Sheets/{expedition_name}/"),verbose = FALSE) %>% 
    filter(name != "EXPEDITION DATA") %>% 
    .$name
  
  days_data <- lapply(folders, function(folder_name){
    day_complete_data_id <- googledrive::drive_get(
      str_glue("~/Data Sheets/{expedition_name}/{folder_name}/COMPLETE DATA/{folder_name}"))$id
    return(googlesheets4::read_sheet(day_complete_data_id,sheet = "Data"))
  })
  
    days_data <- days_data %>% bind_rows
    
    googledrive::drive_mkdir(name = "EXPEDITION DATA",overwrite = TRUE,
                             path = str_glue("~/Data Sheets/{expedition_name}/"))
    all_data <- googlesheets4::gs4_create(name = str_glue("{expedition_name}"),
                                          sheets = list(Data = days_data))
    googledrive::drive_mv(file = all_data, 
                          path = str_glue("~/Data Sheets/{expedition_name}/EXPEDITION DATA/"))
    
    return(days_data)
}
