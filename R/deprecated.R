# These functions were used for obtaining data. 
# However, as the invertebrate sampling protocol changes, so do their respective data sheets.
# As a result, some of the following functions were modified in the main script.



# Function to modify deployment metadata of tel aviv observer tables.
# A helper function for `read_metadata_columns` function.
#
#  Input: a tibble
# Output: a modified tibble with a Fish/Invertebrates column named "Group"
# Deprecated on: 7/11/2021
# Reason: Invertebrate metadata is now on a different sheet type, so this function is unnecessary

modify_telaviv_deployment_metadata <- function(deployment_metadata_tbl,sheet_identifier){
  deployment_metadata_tbl %>% 
    mutate(Group = if_else(str_detect(sheet_identifier, "Fish"),"Fish", "Invertebrates")) %>% 
    return()
}


# Function to read all worksheets in a deployment spreadsheet
#
#  Input: sampling day metadata tibble obtained by `download_meta_sheet`,
#         spreadsheet ID object of the deployment.
# Output: A tibble containing all sample data and metadata within the deployment
# Deprecated on: 3/11/2021
# Reason: Change to Invertebrate worksheet - no more quadrate worksheets

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


# Function to read quadrates data and metadata, and bind them together
#
#  Input: spreadsheet ID and a worksheet identifier
# Output: A tibble describing the quadrate survey data with quadrates metadata
# Deprecated on: 3/11/2021
# Reason: Change to Invertebrate worksheet - no more quadrate worksheets

read_quadrate_worksheet <- function(spreadsheet_id, sheet_identifier){
  quadrate_worksheet <- googlesheets4::read_sheet(spreadsheet_id, sheet_identifier, col_types = "c")
  message(glue::glue("Waiting for 10 seconds between worksheets"))
  Sys.sleep(10)
  quadrate_metadata <- quadrate_worksheet[,1:2] %>%
    filter(!is.na(Metadata)) %>% 
    pivot_wider(names_from = Metadata, names_sort = FALSE, values_from = Value) %>% 
    mutate(across(.fns = function(x) type.convert(x, as.is = T))) 
  
  quadrate_data <- quadrate_worksheet[,-c(1:2)] %>% 
    filter(!is.na(Quadrate)) %>% 
    mutate(across(.fns = function(col) replace(x = col, is.na(col), 0)))
  bind_cols(quadrate_metadata, quadrate_data) %>% 
    return()
}

# Function to join quadrate data with the invertebrate sampling data
#
#  Input: worksheet identifier of the quadrate sheets,
#         the worksheets read by the first part of `read_deployment_spreadsheet`,
#         and a spreadsheet ID object of the deployment.
# Output: A tibble describing the quadrate survey data with quadrates metadata
# Deprecated on: 3/11/2021
# Reason: Change to Invertebrate worksheet - no more quadrate worksheets

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

# Function to read the observer table and
# arrange them according to the project
# A helper function for `read_worksheet` function.
#
#  Input: sampling day metadata tibble obtained by `download_meta_sheet`,
#         and a worksheet tibble
# Output: A tibble describing the species survey data
# Deprecated on: 3/11/2021
# Reason: Different reading functions for TLV deployments

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

# Function to create photo folders for each surveyor couple.
#
#  Input: row of the surveyor data obtained by `get_surveyors_data`,
#         expedition name, and folder name
# Output: A number of folders named according to couple deployments,
#         with subfolders named A-D, created under the "Photos" folder. 
# Deprecated on: 3/1/2022
# Reason: `verbose` argument being deprecated, replacing with `local_drive_quiet`

create_photo_folders_row <- function(surveyors_data, expedition_name, folder_name){
  
  photo_folder <- googledrive::drive_mkdir(name = surveyors_data$spreadsheet_name,
                                           path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/Photos/"),
                                           verbose = FALSE)
  
  lapply(unlist(surveyors_data$deployments), function(dep) {
    googledrive::drive_mkdir(name =  as.character(dep),
                             path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/Photos/{surveyors_data$spreadsheet_name}/"),
                             verbose = FALSE)
    for (transect in LETTERS[4:1]){
      googledrive::drive_mkdir(name = transect,
                               path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/Photos/{surveyors_data$spreadsheet_name}/{as.character(dep)}/"),
                               verbose = FALSE)
    }
  })
  grant_writing_permission(spreadsheet_id = photo_folder, vector_of_emails = unlist(surveyors_data$emails))
}

# Function to create photo folders framework for all surveyor couples.
# This function uses the input file as input and can be run individually.
#
#  Input: Path of input csv file 
# Output: A "Photos" folder under the sampling day folder, with
#         A number of folders named according to couple deployments,
#         with subfolders named A-D. 
# Deprecated on: 3/1/2022
# Reason: `verbose` argument being deprecated, replacing with `local_drive_quiet`

create_photo_folders <- function(file){
  
  meta_table <- read_metadata(input_sheet = file)
  
  expedition_name <- unique(meta_table$Expedition)
  folder_name <- paste(
    unique(meta_table$Location),unique(meta_table$Date))
  
  googledrive::drive_mkdir(name = "Photos",
                           path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/"),
                           verbose = FALSE)
  
  surveyors_data <- get_surveyors_data(input_data = meta_table)
  
  message(glue::glue("Creating Photos folders"))
  
  create_photo_folders_from_surveyor_data(surveyors_data = surveyors_data,
                                          expedition_name = expedition_name,
                                          folder_name = folder_name)
}

# Function to create photo folders framework for all surveyor couples.
# This function runs automatically with `build_framework` if `photos_needed` is TRUE.
#
#  Input: Surveyor data obtained by `get_surveyors_data`,
#         expedition name, and folder name
# Output: A "Photos" folder under the sampling day folder, with
#         A number of folders named according to couple deployments,
#         with subfolders named A-D. 
# Deprecated on: 3/1/2022
# Reason: `verbose` argument being deprecated, replacing with `local_drive_quiet`

create_photo_folders_for_framework <- function(surveyors_data, expedition_name, folder_name){
  message(glue::glue("Creating Photos folders...\n\n"))
  
  googledrive::drive_mkdir(name = "Photos",
                           path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/"),
                           verbose = FALSE)
  
  create_photo_folders_from_surveyor_data(surveyors_data = surveyors_data,
                                          expedition_name = expedition_name,
                                          folder_name = folder_name)
}

# Function to copy the the respective skeleton which will be used to build observer spreadsheets
# A helper function for `create_spreadsheet` function
#
#  Input: A string specifying the project, 
#         expedition name and folder name which for destination of the skeleton,
#         Name of new spreadsheet after copying
# Output: A copied skeleton spreadsheet with new name in the selected destination
# Deprecated on: 3/1/2022
# Reason: `verbose` argument being deprecated, replacing with `local_drive_quiet`

copy_skeleton <- function(project, expedition_name, folder_name, spreadsheet_name) {
  skeleton <- case_when(
    project %in% projects$`Tel Aviv Transects`      ~ "~/Data Sheets/Skeleton Folder/Tel Aviv Skeleton 2.0",
    project %in% projects$`Eilat Transects`         ~ "~/Data Sheets/Skeleton Folder/Eilat Skeleton - Transects",
    project %in% projects$`Eilat Knolls`            ~ "~/Data Sheets/Skeleton Folder/Eilat Skeleton - Knolls",
    project %in% projects$`Mediterranean Transects` ~ "~/Data Sheets/Skeleton Folder/Bioblitz Skeleton"
  )

  googledrive::drive_cp(file = skeleton,
                        path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/"),
                        name = spreadsheet_name,
                        verbose = FALSE)
}

# Function to download individual days data, join them, 
# and upload to a EXPEDITION DATA folder.
#  Input: Expedition name,
#         optional: whether to upload individual day data
# Output: A tibble containing all of the expedition data,
#         uploaded into a created folder within
#         expedition directory
# Deprecated on: 3/1/2022
# Reason: `verbose` argument being deprecated, replacing with `with_drive_quiet`

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
# Deprecated on: 3/1/2022
# Reason: `verbose` argument being deprecated, replacing with `with_drive_quiet`

combine_days_data <- function(expedition_name, upload = TRUE){
  folders <- googledrive::drive_ls(str_glue("~/Data Sheets/{expedition_name}/"),verbose = FALSE) %>% 
    filter(name != "EXPEDITION DATA") %>% 
    .$name
  
  days_data <- lapply(folders, function(folder_name){
    day_complete_data_id <- googledrive::drive_get(
      str_glue("~/Data Sheets/{expedition_name}/{folder_name}/COMPLETE DATA/{folder_name}"))$id
    return(googlesheets4::read_sheet(day_complete_data_id,sheet = "Data"))
  })
  
  days_data <- days_data %>% bind_rows
  
  if (upload){ googledrive::drive_mkdir(name = "EXPEDITION DATA",overwrite = TRUE,
                                        path = str_glue("~/Data Sheets/{expedition_name}/"))
    all_data <- googlesheets4::gs4_create(name = str_glue("{expedition_name}"),
                                          sheets = list(Data = days_data))
    googledrive::drive_mv(file = all_data, 
                          path = str_glue("~/Data Sheets/{expedition_name}/EXPEDITION DATA/"))
  }
  return(days_data)
}

# Function to create a directory for the expedition:
# This should be executed once at the start of each expedition
#
#  Input: Name of expedition
# Output: Folder created in Google Drive

create_expedition_directory <- function(expedition_name){
  googledrive::drive_mkdir(name = expedition_name,
                           path = "~/Data Sheets/",verbose = FALSE)
}

# Function to create a folder in Google Sheets named folder name
# This should be executed for each day or for each location
#
#  Input: Expedition name (used in `create_expedition_directory` function),
#         "location date" string indicating location and
#         date of the dive (eg. "Katza 2020-02-11")
# Output: Folder created in Google Drive, under expedition folder,
#         with a child folder named "Metadata"
# Deprecated on: 3/1/2022
# Reason: `verbose` argument being deprecated, replacing with `local_drive_quiet`

create_main_directory <- function(expedition_name,folder_name) {
  googledrive::drive_mkdir(name = folder_name,
                           path = str_glue("~/Data Sheets/{expedition_name}/"),
                           verbose = FALSE)
  dir <- str_glue("~/Data Sheets/{expedition_name}/{folder_name}/")
  googledrive::drive_mkdir(name = "Metadata",
                           path = dir,
                           verbose = FALSE)
}

# Function to upload the metadata sheet to the Metadata folder
#
#  Input: Metadata table obtained by `read_metadata`,
#         Expedition name (used in `create_expedition_directory` function),
#         Location name (used in `create_main_directory` function) 
# Output: Metadata table uploaded to Metadata folder 
# Deprecated on: 3/1/2022
# Reason: `verbose` argument being deprecated, replacing with `with_drive_quiet`

upload_meta_sheet <- function(meta_table, expedition_name, folder_name){
  spreadsheet_name <- str_glue("{folder_name} - metadata")
  meta_spreadsheet <- googlesheets4::gs4_create(name = spreadsheet_name,
                                                sheets = list(metadata = meta_table))
  googledrive::drive_mv(file = meta_spreadsheet, verbose = FALSE,
                        path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/Metadata/"))
}

# Function to generate reading permission of a spreadsheet
# This is a helper function for the following function `grant_metadata_reading_permission`
# 
#  Input: A googlesheet ID object of the spreadsheet and vector of emails
# Output: Reading permission of the spreadsheet sent to all emails in the vector
# Deprecated on: 3/1/2022
# Reason: `verbose` argument being deprecated, replacing with `local_drive_quiet`

grant_reading_permission <- function(spreadsheet_id, vector_of_emails){
  lapply(vector_of_emails, function(email) {
    if (is.na(email)) { warning("No Email Assigned to Surveyor, permission not granted")
    } else {
      googledrive::drive_share(file = spreadsheet_id, role = "reader", type = "user",
                               emailAddress = email,verbose = F)
    }
  }
  )
}

# Function to grant reading permission of the metadata to all surveyors involved in the sampling
# 
#  Input: Expedition name, Folder name, Surveyor table obtained by `get_surveyors_data`
# Output: Reading permission of the metadata spreadsheet sent to all emails in the surveyor data table
# Deprecated on: 3/1/2022
# Reason: `verbose` argument being deprecated, replacing with `local_drive_quiet`

grant_metadata_reading_permission <- function(expedition_name, folder_name, surveyor_data){
  id <- googledrive::drive_get(verbose = FALSE,
                               str_glue("~/Data Sheets/{expedition_name}/{folder_name}/Metadata/{folder_name} - metadata"))$id
  day_surveyors <- unlist(surveyor_data$emails) %>% unique
  grant_reading_permission(spreadsheet_id = googledrive::as_id(id),vector_of_emails = day_surveyors)
}
