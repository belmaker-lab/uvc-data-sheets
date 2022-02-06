build_framework <- function(file){
  
  ########  Step 1: Reading the metadata table #############
  # 
  #   This step reads the metadata file from the input file,
  #   and stops if project is not in the list of compatible 
  #   projects for this script.
  
  meta_table <- read_metadata(input_sheet = file)
  
  project <- unique(meta_table$Project)
  if (!(project %in% unlist(projects))){
    stop("Project supplied is not supported.")
  }
  
  ########  Step 2: Setting expedition name #############
  #
  #   This step sets the name for the current expedition
  #   and sampling day based on the input file.
  
  expedition_name <- unique(meta_table$Expedition)
  if (length(expedition_name) > 1) 
    cli::cli_abort("Multiple expedition names supplied: {.val {expedition_name}}.")
  folder_name <- paste(unique(meta_table$Location), unique(meta_table$Date))
  if (length(folder_name) > 1) 
    cli::cli_abort("Multiple dates and or locations supplied.")
  
  ########  Step 3: Testing for expedition directory #############
  #
  #   This step searches for the an expedition
  #   directory for the current expedition. If it
  #   doesn't find one, it creates one.
  
  cli::cli_alert_info("Searching for expedition directory named {.val {expedition_name}}...\n\n")
  
  search_results <- googledrive::with_drive_quiet(
    googledrive::drive_find(pattern = expedition_name,
                            q = str_glue("'{data_sheets_id}' in parents")))
  if (nrow(search_results) == 1) {
    cli::cli_alert_success("Found expedition directory named {.val {expedition_name}}.")
    expedition_dribble <- search_results
    rm(search_results)
  } else if (nrow(search_results) == 0){
    cli::cli_alert_warning("Expedition directory named {.val {expedition_name}} does not exist.")
    cli::cli_alert_info("Creating expedition directory now.")
    expedition_dribble <- create_expedition_directory(expedition_name)
  } else {
    cli::cli_abort("\n Something is not right. Could be duplicate Expedition directory name in Data Sheets.")
  }
  
  ########  Step 4: Creating folders in Drive ###################
  # 
  #   This step creates a folder under the current expedition
  #   directory, and an additional folder nested within 
  #   it called "Metadata, based on the input file Location
  #   and Date.
  #   Note:
  #     Folder name should be a "Location YYYY-MM-DD" format
  
  cli::cli_alert_info("Creating folder named {folder_name}\n\n")
  
  folder_dribble <- create_main_directory(expedition_dribble, folder_name)
  
  #########  Step 5: Uploading the metadata table ############
  #
  #   This step uploads the metadata file from the input file
  #   into the Metadata folder under today's folder in 
  #   the expedition directory.
  
  upload_meta_sheet(meta_table, folder_dribble)
  
  
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
  #   send emails with writing permission for each surveyor.
  #   Spreadsheets are located in:
  #   "~/Data Sheets/{this_expedition}/{todays_folder}/
  
  cli::cli_text("Creating spreadsheets...\n\n")
  
  create_spreadsheets(surveyors_data, folder_dribble)
  
  ########  Step 8: Creating photo folders  #####################
  #
  #   This step uses the surveyor data to generate a "Photos" folder,
  #   multiple transects subfolders, and to send emails with 
  #   editting permission for each surveyor.
  #   "Photos" folder is located in:
  #   "~/Data Sheets/{this_expedition}/{todays_folder}/
  
  if (photos_needed(project = project)){
    create_photo_folders_for_framework(surveyors_data, folder_dribble)}
  
}
