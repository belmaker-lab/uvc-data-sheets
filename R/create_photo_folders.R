photos_needed <- function(project){
  out <- case_when(
    project %in% projects$`Tel Aviv Transects`      ~ FALSE,
    project %in% projects$`Eilat Transects`         ~ TRUE,
    project %in% projects$`Eilat Knolls`            ~ FALSE,
    project %in% projects$`Mediterranean Transects` ~ TRUE
  )
  return(out)
}

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
  
create_photo_folders_from_surveyor_data <- function(surveyors_data, expedition_name, folder_name){
  apply(surveyors_data, 1,
        function(row) create_photo_folders_row(
          surveyors_data = row,
          expedition_name = expedition_name,
          folder_name = folder_name))
  
}

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

create_photo_folders_for_framework <- function(surveyors_data, expedition_name, folder_name){
  message(glue::glue("Creating Photos folders...\n\n"))
  
  googledrive::drive_mkdir(name = "Photos",
                           path = str_glue("~/Data Sheets/{expedition_name}/{folder_name}/"),
                           verbose = FALSE)
  
  create_photo_folders_from_surveyor_data(surveyors_data = surveyors_data,
                                          expedition_name = expedition_name,
                                          folder_name = folder_name)
}