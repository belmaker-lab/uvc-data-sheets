# Function to test if current project needs photo folders.
#
#  Input: Project name
# Output: TRUE  or FALSE depending on project

photos_needed <- function(project){
  out <- case_when(
    project %in% projects$`Tel Aviv Transects`      ~ FALSE,
    project %in% projects$`Eilat Transects`         ~ TRUE,
    project %in% projects$`Eilat Knolls`            ~ FALSE,
    project %in% projects$`Mediterranean Transects` ~ TRUE
  )
  return(out)
}

# Function to create photo folders for each surveyor couple.
#
#  Input: row of the surveyor data obtained by `get_surveyors_data`,
#         expedition name, and folder name
# Output: A number of folders named according to couple deployments,
#         with subfolders named A-D, created under the "Photos" folder. 

create_photo_folders_row <- function(surveyors_data, expedition_name, folder_name){
  googledrive::local_drive_quiet()
  photo_folder <- googledrive::drive_mkdir(name = surveyors_data$spreadsheet_name,
                           path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/Photos/"))
  
  lapply(unlist(surveyors_data$deployments), function(dep) {
    googledrive::drive_mkdir(name =  as.character(dep),
                             path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/Photos/{surveyors_data$spreadsheet_name}/"))
    for (transect in LETTERS[4:1]){
      googledrive::drive_mkdir(name = transect,
                               path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/Photos/{surveyors_data$spreadsheet_name}/{as.character(dep)}/"))
    }
  })
  grant_writing_permission(spreadsheet_id = photo_folder, vector_of_emails = unlist(surveyors_data$emails))
}

# Function to create photo folders for all surveyor couples.
#
#  Input: Surveyor data obtained by `get_surveyors_data`,
#         expedition name, and folder name
# Output: A number of folders named according to couple deployments,
#         with subfolders named A-D, created under the "Photos" folder. 
  
create_photo_folders_from_surveyor_data <- function(surveyors_data, expedition_name, folder_name){
  apply(surveyors_data, 1,
        function(row) create_photo_folders_row(
          surveyors_data = row,
          expedition_name = expedition_name,
          folder_name = folder_name))
  
}

# Function to create photo folders framework for all surveyor couples.
# This function uses the input file as input and can be run individually.
#
#  Input: Path of input csv file 
# Output: A "Photos" folder under the sampling day folder, with
#         A number of folders named according to couple deployments,
#         with subfolders named A-D. 

create_photo_folders <- function(file){
  
  googledrive::local_drive_quiet()
  meta_table <- read_metadata(input_sheet = file)
  
  expedition_name <- unique(meta_table$Expedition)
  folder_name <- paste(
    unique(meta_table$Location),unique(meta_table$Date))
  
  googledrive::drive_mkdir(name = "Photos",
                           path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/"))
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

create_photo_folders_for_framework <- function(surveyors_data, expedition_name, folder_name){
  
  googledrive::local_drive_quiet()
  message(glue::glue("Creating Photos folders...\n\n"))
  
  googledrive::drive_mkdir(name = "Photos",
                           path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/"))
  
  create_photo_folders_from_surveyor_data(surveyors_data = surveyors_data,
                                          expedition_name = expedition_name,
                                          folder_name = folder_name)
}