library(tidyverse)
source("R/tlv_functions.R")

# Function to obtain the metadata sheet from the metadata folder.
# 
#  Input: Expedetion name, folder name.
# Output: a tibble containing the sampling day metadata 
#         with an additional `meta_to_site` column

download_meta_sheet <- function(expedition_name, folder_name){
  metadata_id <- googledrive::drive_get(
    path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/Metadata/{folder_name} - metadata"))$id
  meta_table <- googlesheets4::read_sheet(metadata_id, "metadata")
  
  if (unique(meta_table$Project) %in% projects$`Tel Aviv Transects`)
  {meta_table <- mutate(meta_table, meta_to_deployment_id = str_glue(
    "{`First Observer`} and {`Second Observer`} - {Spot}"))}
  if (unique(meta_table$Project) %in% projects$`Mediterranean Transects`)
  {meta_table <- mutate(meta_table, meta_to_deployment_id = str_glue(
    "{`First Observer`} and {`Second Observer`} - {SiteID}"))}
  if (unique(meta_table$Project) %in% projects$`Eilat Transects`)
  {meta_table <- mutate(meta_table, meta_to_deployment_id = str_glue(
    "{`First Observer`} and {`Second Observer`} - {SiteID}"))}
  if (unique(meta_table$Project) %in% projects$`Eilat Knolls`)
  {meta_table <- mutate(meta_table, meta_to_deployment_id = str_glue(
    "{`First Observer`} and {`Second Observer`} - {KnollID}"))}
  
  return(meta_table)
}

# Function to modify deployment metadata of eilat transects observer tables.
# A helper function for `read_metadata_columns` function.
#
#  Input: a tibble
# Output: a modified tibble with a T/C column named "Letter"

modify_eilat_transects_deployment_metadata <- function(deployment_metadata_tbl,sheet_identifier){
  deployment_metadata_tbl %>% 
    mutate(Letter = if_else(str_detect(sheet_identifier,"TRANSIENTS"), "T", "C")) %>% 
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

read_metadata_columns <- function(day_metadata, worksheet, sheet_identifier){
  deployment_metadata <- worksheet[,1:2] %>%
    filter(!is.na(Metadata)) %>% 
    pivot_wider(names_from = Metadata, names_sort = FALSE, values_from = Value) %>% 
    mutate(across(.fns = function(x) type.convert(x, as.is = T))) %>% 
    mutate(across(contains(c("Depth","Visib")) & where(is.logical), as.numeric)) %>% 
    mutate(across(where(is.logical), as.character)) 
  
  # googlesheets4 does not support hms class at the moment, so we will keep times as chr
  # %>% 
  #   mutate(across(.cols = all_of(c("Time Start","Time End")),
  #                 .fns = hms::parse_hm))
  
  if (unique(day_metadata$Project) %in% projects$`Eilat Transects`){
    return(modify_eilat_transects_deployment_metadata(deployment_metadata, sheet_identifier))
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
      filter(across(any_of(c("Species","Amount","Length","Distance")),
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
  worksheet <- googlesheets4::read_sheet(spreadsheet_id, sheet_identifier, col_types = "c",
                                         .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))
  Sys.sleep(2)
  sample_metadata <- read_metadata_columns(day_meta, worksheet, sheet_identifier)
  sample_data <- read_observer_worksheet(day_meta, worksheet, sheet_identifier)
  bind_cols(sample_metadata, sample_data) %>% 
    return()
}

# Function to read all worksheets in a deployment spreadsheet
#
#  Input: sampling day metadata tibble obtained by `download_meta_sheet`,
#         spreadsheet ID object of the deployment.
# Output: A tibble containing all sample data and metadata within the deployment

read_deployment_spreadsheet <- function(day_metadata, spreadsheet_id){
  samples <- googlesheets4::sheet_properties(googlesheets4::as_sheets_id(spreadsheet_id)) %>% 
    filter(!str_detect(string = name,"Invertebrates|Species List|Readme.txt|Quadrate data")) %>% 
    .$name
  
  all_worksheets <- lapply(samples, function(sheet_identifier)
    read_worksheet(day_metadata, spreadsheet_id, sheet_identifier))
  
  
  lapply(all_worksheets, function(df) {
    mutate(df,across(.fns = as.character))
  }) %>% 
    bind_rows() %>%
    return()
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

# Function to store a local backup of the created expedition data
# in a dedicated folder for local backups. Adds a warning column
# to not use the backup for analyses.
# 
#  Input: Complete expedition data, expedition name.
# Output: Local `.csv` file in a dedicated folder stating the
#         date and time of its creation.

store_local_backup <- function(days_data, expedition_name){
  
  if (!dir.exists(str_glue("Local Backups/{expedition_name}/")))
    dir.create(str_glue("Local Backups/{expedition_name}/"), recursive = TRUE)
  
  current_time <- Sys.time() %>% as.character() %>% 
    str_replace_all(":","-")
  
  backup <- days_data %>% 
    mutate(Flag = "This is a local backup file and should not be used for further analyses!")
  
  write_csv(backup, file = str_glue("Local Backups/{expedition_name}/{expedition_name} - local version created {current_time}.csv"))
  
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

create_day_complete_data <- function(expedition_name, folder_name, upload_individual_days = FALSE){
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
  
  unique_ids_meta <- bind_cols(meta_to_deployment_id = day_metadata$meta_to_deployment_id)
  unique_ids_samples <- bind_cols(meta_to_deployment_id = day_sample_data$meta_to_deployment_id)
  
  mismatches <- anti_join(unique_ids_meta, unique_ids_samples,
                          by = "meta_to_deployment_id") %>%
    pull(meta_to_deployment_id)
  
  if (length(mismatches) > 0){
    cli::cli_abort("Error in {.val {folder_name}} - {cli::qty(mismatches)} Mismatching identifier{?s}: {.val {mismatches}}. 
                   Check Google Drive folder for any input errors.")
  }
  
  day_complete_data <- left_join(day_metadata,day_sample_data, by = c("meta_to_deployment_id"))
  
  if (upload_individual_days) {
    googledrive::drive_mkdir(name = "COMPLETE DATA", overwrite = TRUE,
                             path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/"))
    day_spreadsheet <- googlesheets4::gs4_create(name = str_glue("{folder_name}"),
                                                 sheets = list(Data = day_complete_data))
    googledrive::drive_mv(file = day_spreadsheet, 
                          path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/COMPLETE DATA/"))
  }
  
  return(day_complete_data)
}

# Function to check for a complete data sheet for the day
# 
#  Input: Expedetion name, folder name.
# Output: a tibble containing the sampling day metadata 
#         with an additional `meta_to_site` column

check_day_complete_data <- function(expedition_name, folder_name){
  folders <- googledrive::drive_ls(
    path = str_glue("~/Data Sheets/{this_expedition}/{todays_folder}/"))$name
  
  return(COMPLETE)
}

# Function to create individual days data, join them, 
# and upload them to a EXPEDITION DATA folder. Also creates
# a local backup using `store_local_backup`.
#
#  Input: Expedition name,
#         optional: whether to upload individual day data
# Output: A tibble containing all of the expedition data,
#         uploaded into a created folder within
#         expedition directory

create_expedition_data <- function(expedition_name, upload_individual_days = TRUE){
  
  folders <- googledrive::with_drive_quiet(googledrive::drive_ls(str_glue("~/Data Sheets/{expedition_name}/"))) %>% 
    filter(name != "EXPEDITION DATA") %>% 
    .$name
  
  days_data <- lapply(folders, function(folder_name){
    message(glue::glue("Downloading data from {folder_name}"))
    if (upload_individual_days) {
      message(glue::glue("{folder_name} data uploaded"))
    }
    return(create_day_complete_data(expedition_name, folder_name, upload_individual_days))
  })
  
  days_data <- days_data %>% bind_rows
  
  googledrive::drive_mkdir(name = "EXPEDITION DATA", overwrite = TRUE,
                           path = str_glue("~/Data Sheets/{expedition_name}/"))
  all_data <- googlesheets4::gs4_create(name = str_glue("{expedition_name}"),
                                        sheets = list(Data = days_data))
  googledrive::drive_mv(file = all_data, 
                        path = str_glue("~/Data Sheets/{expedition_name}/EXPEDITION DATA/"))
  
  store_local_backup(days_data, expedition_name)
  
  return(days_data)
}


# Function to join individual day complete data, 
# and upload to a EXPEDITION DATA folder.
# Also creates a local backup using `store_local_backup`.
#
#  Input: Expedition name
# Output: A tibble containing all of the expedition data,
#         uploaded into a created folder within
#         expedition directory

join_days_data <- function(expedition_name){
  folders <- googledrive::with_drive_quiet(googledrive::drive_ls(str_glue("~/Data Sheets/{expedition_name}/"))) %>% 
    filter(name != "EXPEDITION DATA") %>% 
    .$name
  
  days_data <- lapply(folders, function(folder_name){
    day_complete_data_id <- googledrive::drive_get(
      str_glue("~/Data Sheets/{expedition_name}/{folder_name}/COMPLETE DATA/{folder_name}"))$id
    return(googlesheets4::read_sheet(day_complete_data_id,sheet = "Data"))
  })
  
  days_data <- days_data %>% bind_rows
  
  googledrive::drive_mkdir(name = "EXPEDITION DATA", overwrite = TRUE,
                           path = str_glue("~/Data Sheets/{expedition_name}/"))
  all_data <- googlesheets4::gs4_create(name = str_glue("{expedition_name}"),
                                        sheets = list(Data = days_data))
  googledrive::drive_mv(file = all_data, 
                        path = str_glue("~/Data Sheets/{expedition_name}/EXPEDITION DATA/"))
  
  store_local_backup(days_data, expedition_name)
  
  return(days_data)
}
