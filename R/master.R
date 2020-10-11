build_framework <- function(file){
  
  ########  Step 1: Reading the metadata table #############
  # 
  #   This step reads the metadata file from the input file
  
  meta_table <- read_metadata(input_sheet = file)
  
  
  ########  Step 2: Setting expedition name #############
  #
  #   This step sets the name for the current expedition
  #   and sampling day based on the input file.
  
  expedition_name <- unique(meta_table$Expedition)
  folder_name <- paste(
    unique(meta_table$Location),unique(meta_table$Date))
  
  ########  Step 3: Testing for expedition directory #############
  #
  #   This step searches for the an expedition
  #   directory for the current expedition. If it
  #   doesn't find one, it creates one.
  
  message(glue::glue("Searching for expedition directory named {expedition_name}...\n\n"))
  
  search_results <- googledrive::drive_get(
    path = str_glue("~/Data Sheets/{expedition_name}/"))
  if (nrow(search_results) == 1) {
    message(glue::glue("Found expedition directory named {expedition_name}.\n\n"))
  } else if (nrow(search_results) == 0){
    message(glue::glue("Expedition directory named {expedition_name} does not exist."))
    message(glue::glue("Creating expedition directory now.\n\n"))
    create_expedition_directory(expedition_name)
  } else stop(glue::glue("Something is not right. Could be one of the following:
                         1. Duplicate Expedition directory name in Data Sheets.
                         2. Error with drive_get."))
  
  ########  Step 4: Creating folders in Drive ###################
  # 
  #   This step creates a folder under the current expedition
  #   directory, and an additional folder nested within 
  #   it called "Metadata, based on the input file Location
  #   and Date.
  #   Note:
  #     Folder name should be a "Location YYYY-MM-DD" format
  
  message(glue::glue("Creating folder named {folder_name}\n\n"))
  
  create_main_directory(expedition_name,folder_name)
  
  
  #########  Step 5: Uploading the metadata table ############
  #
  #   This step uploads the metadata file from the input file
  #   into the Metadata folder under today's folder in 
  #   the expedition directory.
  
  message(glue::glue("Uploading metadata to {folder_name} - Metadata\n\n"))
  
  suppressMessages(upload_meta_sheet(meta_table, expedition_name, folder_name))
  
  
  ########  Step 6: Obtaining surveyors data  ########################
  #
  #   This step uses the local metadata object to create
  #   a tibble containing surveyors names, emails, deployments,
  #   and what project this sampling is a part of. 
  #   This is then used for creating the data sheets in the next part.
  # 
  
  surveyors_data <- get_surveyors_data(input_data = meta_table)
  
  
  
  ########  Step 7: Creating observer sheets  #####################
  #
  #   This step uses the surveyor data to generate spreadsheets and
  #   sends emails with writing permission for each surveyor.
  #   Spreadsheets are located in:
  #   "~/Data Sheets/{this_expedition}/{todays_folder}/
  
  message(glue::glue("Creating spreadsheets...\n\n"))
  
  create_spreadsheets(surveyors_data, expedition_name,folder_name)
}
