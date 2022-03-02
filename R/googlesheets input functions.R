# Function to ask for user input on what type of sampling protocol was used.
#
#  Input: Nothing, asks for user input in console.
# Output: The project name as a character object

select_project <- function(){
  projects <- c("Bioblitz","ASSEMBLE", "Eilat Knolls", "Eilat Transects", "Tel Aviv Project")
  cli::cli_h1("Select Sampling Type:")
  cli::cli_ol(projects)
  choice <- readline("Selection: ")
  if (!choice %in% seq_along(projects)){
    cli::cli_abort("Please choose value from the list.")
  }
  choice <- as.numeric(choice)
  return(projects[choice])
}

# Function to open the appropriate google sheets input spreadsheet 
#
#  Input: Name of project
# Output: Opens the spreadsheet in the browser and returns the URL
#         for later use.

open_sheet_in_browser <- function(project_name){
  
  input_folder <- googledrive::drive_find("Input Sheets", type = "folder",
                                          n_max = 1, 
                                          q = str_glue("'{data_sheets_id}' in parents"))
  
  input_sheet <- googledrive::drive_find(project_name, type = "spreadsheet",
                                         q = str_glue("'{input_folder$id}' in parents"))
  
  return(googlesheets4::gs4_browse(input_sheet))
}

# Function to read the appropriate google sheets input spreadsheet 
#
#  Input: The URL of the appropriate spreadsheet
# Output: A tibble

read_input_sheet <- function(input_sheet_url){
  
  googlesheets4::local_gs4_quiet()
  
  cli::cli_alert_info("Reading the appropriate input sheet...")
  
  input_sheet <- googlesheets4::read_sheet(ss = input_sheet_url)
  
  return(input_sheet)
  
}

# Function to reshape input spreadsheets into the a usable format 
#
#  Input: tibble from `read_input_sheet`
# Output: A re-formatted tibble

reshape_input_sheet <- function(input_sheet){
  metadata <- input_sheet[,1:2] %>%
    filter(!is.na(Metadata)) %>% 
    pivot_wider(names_from = Metadata, names_sort = FALSE, values_from = Value) %>% 
    mutate(across(.fns = function(x) type.convert(x, as.is = T))) %>% 
    mutate(across(where(is.logical), as.character)) %>% 
    mutate(across(contains(c("Project","Country","Expedition", "Location")), as.character)) %>% 
    mutate(across(contains("Date"), lubridate::ymd))
  
  surveyors <- input_sheet[,3:length(input_sheet)] %>% 
    filter(if_any(any_of(c("Site","SiteID","Knoll","Spot")),
                  .fns = function(x) !is.na(x)))
  
  bind_cols(metadata, surveyors) %>% 
    return()
}

# Function to store the formatted tibble locally, just in case
#
#  Input: tibble from `reshape_input_sheet`
# Output: Local version of the tibble as a csv file

store_local_input <- function(input_sheet){
  
  expedition_name <- unique(input_sheet$Expedition)
  day_sample <- paste(unique(input_sheet$Location),
                      unique(input_sheet$Date))
  
  if (!dir.exists(str_glue("input files/{expedition_name}/")))
    dir.create(str_glue("input files/{expedition_name}/"), recursive = TRUE)
  
  current_time <- Sys.time() %>% as.character() %>% 
    str_replace_all(":","-")
  
  write_csv(input_sheet, file = str_glue("input files/{expedition_name}/{day_sample} - created {current_time}.csv"))
  
}

# Function to reformat the column names, etc. to better match previous formats
#
#  Input: the tibble from `reshape_input_sheet` and the project name
# Output: a tibble with re-formatted columns

reformat_meta_tibble_columns <- function(input_sheet, project_name){
  if ("Fish Observer" %in% colnames(input_sheet))
    input_sheet <- input_sheet %>% rename('First Observer' = "Fish Observer")
  if ("Invertebrate Observer" %in% colnames(input_sheet))
    input_sheet <- input_sheet %>% rename('Second Observer' = "Invertebrate Observer")
  if ("Dive" %in% colnames(input_sheet))
    input_sheet <- input_sheet %>%  select(-Dive)
  if ("Site" %in% colnames(input_sheet))
    input_sheet <- input_sheet %>% rename(SiteID = Site)
  if ("Knoll" %in% colnames(input_sheet))
    input_sheet <- input_sheet %>% rename(KnollID = Knoll)
  if (any(is.na(input_sheet$`First Observer`))) 
    warning("Missing First Observer, please check input sheet")
  if (any(is.na(input_sheet$`Second Observer`))) 
    warning("Missing Second Observer, please check sheet")
  if (!"Project" %in% colnames(input_sheet)){
    input_sheet <- mutate(input_sheet, Project = project_name) %>% 
      relocate(Project, .before = Expedition)
  }
  if (!"Country" %in% colnames(input_sheet)){
    input_sheet <- input_sheet %>% mutate(Country = "Israel") %>% 
      relocate(Country)
  }
  return(input_sheet)
}

# The main function.
#
#  Input: None needed, user input required within
# Output: Spreadsheets ready for surveyors data :)

build_framework_from_googlesheet <- function(){
  
  ########  Step 1: Reading the metadata table from Google Sheets #############
  # 
  #   This step reads the metadata file from the input sheet,
  #   and stops if project is not in the list of compatible 
  #   projects for this script.
  
  project_name <- select_project()
  
  cli::cli_ol(c("Press Enter to open sheet in browser", 
                "Fill the sheet",
                "Once done, close the tab and continue here."))
  
  readline()
  input_sheet_url <- open_sheet_in_browser(project_name)
  
  cli::cli_text("Welcome back, press Enter to continue")
  readline()
  
  input_sheet <- read_input_sheet(input_sheet_url)
  input_sheet <- reshape_input_sheet(input_sheet) 
  input_sheet <- reformat_meta_tibble_columns(input_sheet)
  
  cli::cli_h2("Please make sure these are correct before proceeding:")
  print(input_sheet)
  
  stay_in_loop <- TRUE
  while (stay_in_loop){
    cli::cli_text("")
    cli::cli_text("Do you wish to proceed? Y/N")
    selection <- readline(prompt = "Selection: ")
    if (selection == "Y"){
      stay_in_loop <- FALSE
    } else if (selection == "N") {
      stay_in_loop <- FALSE
      cli::cli_abort("Check the input sheet again and re-run this program.")
    } else {
      cli::cli_alert_warning("Y/N only...")
    }
  }
  
  meta_table <- input_sheet
  
  store_local_input(meta_table)
  
  project <- unique(meta_table$Project)
  if (!(project %in% unlist(projects))){
    cli::cli_abort("Project supplied is not supported.")
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
  
  cli::cli_alert_warning("Clearing out the input sheet...")
  
  googlesheets4::with_gs4_quiet(
    googlesheets4::range_clear(input_sheet_url, sheet = 1,
                               range = "B2:K999", reformat = FALSE)
  )
  
  cli::cli_alert_success("Done! Happy Hazanat Netunim!!")
}
