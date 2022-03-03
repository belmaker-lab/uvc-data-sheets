#####      Load the tidyverse library       ####
## Other packages are explicitly              ##
## called for each functions:                 ##
## lubridate, googlesheets4, googledrive      ##
## Read the respective functions'             ## 
## documentation for additional informations. ##                  

library(tidyverse) 

####      Housekeeping Functions         ####

# Function to obtain ID of the Data Sheets folder in Drive
# This runs automatically when loading all the functions.
#  Input: None
# Output: variable names `data_sheets_id` of class `drive_id`

get_data_sheets_id <- function(){
  data_sheets_id <<- googledrive::drive_find(pattern = "Data Sheets", type = "folder",
                                             q = "'root' in parents", n_max = 1)$id
}

get_data_sheets_id()


# Function to obtain surveyors emails
# This runs automatically when loading all the functions.
#  Input: None
# Output: Variable named `email_lookup_table` 
#         containing names and emails of all active surveyors

get_surveyors_emails <- function() {
  id <- googledrive::drive_find(pattern = "Surveyors", type = "spreadsheet",
                                q = str_glue("'{data_sheets_id}' in parents"))$id
  email_lookup_table <<- googlesheets4::read_sheet(ss = id,sheet = "Names") 
}

get_surveyors_emails()



# Function to create a directory for the expedition:
# This should be executed once at the start of each expedition
#
#  Input: Name of expedition
# Output: Folder created in Google Drive

create_expedition_directory <- function(expedition_name){
  return(googledrive::drive_mkdir(name = expedition_name,
                                  path = data_sheets_id))
}

# Function to create a folder in Google Sheets named folder name
# This should be executed for each day or for each location
#
#  Input: Expedition dribble (from `create_expedition_directory` function),
#         "location date" string indicating location and
#         date of the dive (eg. "Katza 2020-02-11")
# Output: Folder dribble created in Google Drive, under expedition folder.


create_main_directory <- function(expedition_dribble, folder_name) {
  return(googledrive::drive_mkdir(name = folder_name, 
                                  path = expedition_dribble))
}

# Function to get data from input file
#
#  Input: Path of input csv file 
# Output: A tibble of input table with filled cells for each column
#         but First Observer and Second Observer (NAs remain NAs for these),
#         Dive column omitted, Site renamed to SiteID, and Fish/Invertebrate
#         is renamed First/Second.

read_metadata <- function(input_sheet){
  input <-  read_csv(input_sheet, show_col_types = FALSE)
  if ("Fish Observer" %in% colnames(input))
    input <- input %>% rename('First Observer' = "Fish Observer")
  if ("Invertebrate Observer" %in% colnames(input))
    input <- input %>% rename('Second Observer' = "Invertebrate Observer")
  if ("Dive" %in% colnames(input))
    input <- input %>%  select(-Dive)
  if ("Site" %in% colnames(input))
    input <- input %>% rename(SiteID = Site)
  if ("Knoll" %in% colnames(input))
    input <- input %>% rename(KnollID = Knoll)
  if (any(is.na(input$`First Observer`))) 
    warning("Missing First Observer, please check input sheet")
  if (any(is.na(input$`Second Observer`))) 
    warning("Missing Second Observer, please check input sheet")
  input <- input %>% 
    fill(-c(`First Observer`,`Second Observer`),.direction = "down") %>% 
    mutate(Date = lubridate::dmy(Date))
  return(input)
}

# Function to upload the metadata sheet to the Metadata folder
#
#  Input: Metadata table obtained by `read_metadata`,
#         Expedition dribble (from `create_expedition_directory` function),
#         folder dribble (from `create_main_directory` function) 
# Output: Metadata table uploaded to Metadata folder 

upload_meta_sheet <- function(meta_table, folder_dribble){
  spreadsheet_name <- str_glue("{folder_dribble$name} - metadata")
  meta_spreadsheet <- googlesheets4::gs4_create(name = spreadsheet_name, 
                                                sheets = list(metadata = meta_table))
  metadata_dir <- googledrive::with_drive_quiet(
    googledrive::drive_mkdir(name = "Metadata", path = folder_dribble))
  googledrive::with_drive_quiet(googledrive::drive_mv(file = meta_spreadsheet,
                                                      path = metadata_dir))
}

# Function to get surveyors names, emails, and number of dives in current day:
# This is then used for creating the data sheets.
# 
#  Input: Metadata table obtained by `read_metadata`,  
#         `email_table` obtained by `get_surveyors_emails`
# Output: A tibble containing First and Second observer names and emails, 
#         names for the spreadsheet that will be created in the drive, 
#         deployment ids of each pair, and the project this sampling is a part of.

get_surveyors_data <- function(input_data, email_table = email_lookup_table) {
  
  if ("Location" %in% colnames(input_data)){
    if (length(unique(input_data$Location)) > 1 ){
      stop("\nMore than one location provided.
           \nPlease use one metadata sheet for each location.")
    }
  }
  
  if (unique(input_data$Project %in% projects$`Eilat Juveniles Transects`)){
    return(get_juveniles_surveyors_data(input_data, email_table = email_lookup_table))
  }
  
  # first part wrangles `email_lookup_table` to join with the input data
  first_observer_name <- email_table %>%
    rename(`First Observer` = FullName) %>% select(`First Observer`, email)
  second_observer_name <- email_table %>% 
    rename(`Second Observer` = FullName) %>% select(`Second Observer`, email)
  
  surveyors <- input_data %>%
    group_by(Project, `First Observer`, `Second Observer`) %>%
    summarize(.groups = "keep", across(.cols = any_of(c("KnollID","SiteID","Spot")),
                                       .fns = list, .names = "deployments")) %>% 
    mutate(spreadsheet_name =  str_glue("{`First Observer`} and {`Second Observer`}")) %>% 
    left_join(first_observer_name, by = "First Observer") %>%     # get email of first observer
    left_join(second_observer_name, by = "Second Observer") %>%   # get email of second observer
    mutate(emails = map2(.x = `email.x`,.y = `email.y`, .f = function(x,y) c(x,y))) %>% 
    select(`First Observer`, `Second Observer`, spreadsheet_name, deployments, emails,Project)
  return(surveyors)
}

# Function to get surveyors names, emails, and number of dives in current day:
# This is then used for creating the data sheets. This function only applies to
# Juveniles surveys as there may be 4 different surveyors
# 
#  Input: Metadata table obtained by `read_metadata`,  
#         `email_table` obtained by `get_surveyors_emails`
# Output: A tibble containing First and Second observer names and emails, 
#         names for the spreadsheet that will be created in the drive, 
#         deployment ids of each pair, and the project this sampling is a part of.

get_juveniles_surveyors_data <- function(input_data, email_table = email_lookup_table) {
  
  # first part wrangles `email_lookup_table` to join with the input data
  first_observer_name <- email_table %>%
    rename(`First Observer` = FullName) %>% select(`First Observer`, email)
  second_observer_name <- email_table %>% 
    rename(`Second Observer` = FullName) %>% select(`Second Observer`, email)
  juveniles_first_observer_name <- email_table %>%
    rename(`Juveniles First Observer` = FullName) %>% select(`Juveniles First Observer`, email)
  juveniles_second_observer_name <- email_table %>% 
    rename(`Juveniles Second Observer` = FullName) %>% select(`Juveniles Second Observer`, email)
  
  surveyors <- input_data %>%
    group_by(Project, `First Observer`, `Second Observer`, 
             `Juveniles First Observer`, `Juveniles Second Observer`) %>%
    summarize(.groups = "keep", across(.cols = any_of(c("KnollID","SiteID","Spot")),
                                       .fns = list, .names = "deployments")) %>% 
    mutate(spreadsheet_name =  str_glue("{`First Observer`}, {`Second Observer`}, {`Juveniles First Observer`}, and {`Juveniles Second Observer`}")) %>% 
    left_join(first_observer_name, by = "First Observer") %>%     # get email of first observer
    rename(email_1 = email) %>% 
    left_join(second_observer_name, by = "Second Observer") %>%   # get email of second observer
    rename(email_2 = email) %>% 
    left_join(juveniles_first_observer_name, by = "Juveniles First Observer") %>%     # get email of juvies first observer
    rename(email_3 = email) %>% 
    left_join(juveniles_second_observer_name, by = "Juveniles Second Observer") %>%   # get email of juvies second observer
    rename(email_4 = email) %>% 
    mutate(emails = pmap(.l = list(`email_1`,`email_2`,
                                   `email_3`, email_4), .f = function(x,y,z,a) c(x,y,z,a))) %>% 
    select(`First Observer`, `Second Observer`, `Juveniles First Observer`,
           `Juveniles Second Observer`, spreadsheet_name, deployments, emails, Project)
  return(surveyors)
}


# Function to generate reading permission of a spreadsheet
# This is a helper function for the following function `grant_metadata_reading_permission`
# 
#  Input: A googlesheet ID object of the spreadsheet and vector of emails
# Output: Reading permission of the spreadsheet sent to all emails in the vector

grant_reading_permission <- function(spreadsheet_id, vector_of_emails){
  googledrive::local_drive_quiet()
  lapply(vector_of_emails, function(email) {
    if (is.na(email)) { warning("No Email Assigned to Surveyor, permission not granted")
    } else {
      googledrive::drive_share(file = spreadsheet_id, role = "reader", type = "user",
                               emailAddress = email)
    }
  }
  )
}

# Function to grant reading permission of the metadata to all surveyors involved in the sampling
# 
#  Input: Expedition name, Folder name, Surveyor table obtained by `get_surveyors_data`
# Output: Reading permission of the metadata spreadsheet sent to all emails in the surveyor data table

grant_metadata_reading_permission <- function(expedition_name, folder_name, surveyor_data){
  googledrive::local_drive_quiet()
  id <- googledrive::drive_get(str_glue("~/Data Sheets/{expedition_name}/{folder_name}/Metadata/{folder_name} - metadata"))$id
  day_surveyors <- unlist(surveyor_data$emails) %>% unique
  grant_reading_permission(spreadsheet_id = googledrive::as_id(id),vector_of_emails = day_surveyors)
}

# Function to generate writing permissions
# This is a helper function for the following function `create_observer_tables`
#
#  Input: A googlesheet ID object of the spreadsheet and vector of emails
# Output: Writing permission of the spreadsheet sent to all emails in the vector


grant_writing_permission <- function(spreadsheet_id, vector_of_emails){
  lapply(vector_of_emails, function(email) {
    if (is.na(email)) { warning("No Email Assigned to Surveyor, permission not granted")
    } else {
      googledrive::drive_share(file = spreadsheet_id, role = "writer", type = "user",
                               emailAddress = email)
    }
  }
  )
}

# Function to show cli-style messages notifying spreadsheet creation.
# A helper function for `create_spreadsheets`
#
#  Input: Spreadsheet name, usually names of the surveyor pair
# Output: CLI-formatted message

notify_spreadsheet_creation <- function(spreadsheet_name) {
  cli::cli_div(theme = list(span.emph = list("font-style" = "regular",
                                             "color" = "cyan")))
  cli::cli_text("Creating spreadsheet for {.emph {spreadsheet_name}}")
  cli::cli_end()
}

# Function to show cli-style messages notifying the emails being sent.
# A helper function for `create_spreadsheets`
#
#  Input: Spreadsheet name, usually names of the surveyor pair
# Output: CLI-formatted message

notify_email_sending <- function(spreadsheet_name) {
  cli::cli_div(theme = list(span.emph = list("font-style" = "regular",
                                             "color" = "cyan")))
  cli::cli_text("Sending emails to {.emph {spreadsheet_name}}")
  cli::cli_end()
}

# Functions to create observers tables, place them in folder name, 
# create individual sheets for each transect, and give writing permissions to surveyors
#  Input: row of the surveyor data obtained by `get_surveyors_data`,
#         folder dribble, project name
# Output: Spreadsheets ready for surveyors data :)
#   Note: This function operates on a single row in a tibble


create_spreadsheets_row <- function(surveyors_data, folder_dribble, project) {
  notify_spreadsheet_creation(surveyors_data$spreadsheet_name)
  spreadsheet <- suppressMessages(copy_skeleton(project = project,
                                                folder_dribble = folder_dribble,
                                                spreadsheet_name = surveyors_data$spreadsheet_name))
  
  lapply(unlist(surveyors_data$deployments), function(dep) {
    suppressMessages(
      create_observer_working_sheets(project = project, deployment = dep, spreadsheet = spreadsheet,
                                     observer1 = surveyors_data$`First Observer`,
                                     observer2 = surveyors_data$`Second Observer`,
                                     j_observer1 = ifelse("Juveniles First Observer" %in% colnames(surveyors_data),
                                                          surveyors_data$`Juveniles First Observer`, NA),
                                     j_observer2 = ifelse("Juveniles Second Observer" %in% colnames(surveyors_data),
                                                          surveyors_data$`Juveniles Second Observer`, NA))
    )
  }
  )
  
  suppressMessages(delete_skeleton_sheets(project = project, spreadsheet = spreadsheet))
  notify_email_sending(surveyors_data$spreadsheet_name)
  grant_writing_permission(spreadsheet_id = spreadsheet, vector_of_emails = unlist(surveyors_data$emails))
}


# Functions to apply `create_spreadsheets_row` on all rows of `surveyor_data`
# create individual sheets for each transect, and give writing permissions to surveyors
#  Input: surveyor data obtained by `get_surveyors_data`,
#         folder dribble (from `create_main_directory` function).
# Output: Spreadsheets ready for surveyors data :)
#   Note: This function operates on a single row in a tibble

create_spreadsheets <- function(surveyors_data, folder_dribble){
  project <- unique(surveyors_data$Project)
  apply(surveyors_data, 1,
        function(row) create_spreadsheets_row(
          surveyors_data = row,
          folder_dribble = folder_dribble,
          project = project))
  
}
