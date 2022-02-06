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

create_photo_folders_row <- function(surveyors_data, photo_folder_dribble){
  googledrive::local_drive_quiet()
  photo_folder <- googledrive::drive_mkdir(name = surveyors_data$spreadsheet_name,
                           path = photo_folder_dribble)
  
  lapply(unlist(surveyors_data$deployments), function(dep) {
    surveyors_folder <- googledrive::drive_mkdir(name =  as.character(dep),
                             path = photo_folder)
    for (transect in LETTERS[1:4]){
      googledrive::drive_mkdir(name = transect,
                               path = surveyors_folder)
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
  
create_photo_folders_from_surveyor_data <- function(surveyors_data, photo_folder_dribble){
  apply(surveyors_data, 1,
        function(row) create_photo_folders_row(
          surveyors_data = row,
          photo_folder_dribble = photo_folder_dribble))
}

# Function to create photo folders framework for all surveyor couples.
# This function uses the input file as input and can be run individually.
#
#  Input: Path of input csv file 
# Output: A "Photos" folder under the sampling day folder, with
#         A number of folders named according to couple deployments,
#         with subfolders named A-D. 
# NOTE: Not tested as far as I remember,
# I also don't see much use for it as `create_photo_folders_for_framework` exists...

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

create_photo_folders_for_framework <- function(surveyors_data, folder_dribble){
  
  googledrive::local_drive_quiet()
  cli::cli_text("Creating Photos folders...\n\n")
  
  photo_folder_dribble <- googledrive::drive_mkdir(name = "Photos",
                           path = folder_dribble)
  
  create_photo_folders_from_surveyor_data(surveyors_data = surveyors_data,
                                          photo_folder_dribble = photo_folder_dribble)
}