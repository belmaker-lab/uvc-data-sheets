# This script contains the project specific functions.
# If you wish to add anything, this is the place to do so.


# Function to list all projects. 
# This runs automatically when sourcing this script.
#
#  Input: None.
# Output: A named list containing vectors of project names.

list_projects <- function() {
  projects <<- list("Mediterranean Transects" = c("Bioblitz","ASSEMBLE"),
                    "Eilat Transects" = c("FunDiversity"),
                    "Eilat Knolls" = c("EcoCamp"),
                    "Tel Aviv Transects" = c("TLV"),
                    "Eilat Juveniles Transects" = c("Juveniles"),
                    "Eilat Juveniles Knolls" = c("Juveniles Knolls"),
                    "Eilat Juveniles Artificial" = c("Juveniles Artificial"))
}

list_projects()

# Function to find id of a specific Skeleton spreadsheet.
# A helper function for `copy_skeleton` function
#
#  Input: A name of a project
# Output: ID of the corresponding skeleton spreadsheet

get_skeleton_id <- function(skeleton_name){
  skeleton_folder_id <- googledrive::drive_find(pattern = "Skeleton Folder",
                                                type = "folder", n_max = 1,
                                                q = str_glue("'{data_sheets_id}' in parents"))$id
  skeleton_spreadsheet <- googledrive::drive_find(pattern = skeleton_name, type = "spreadsheet",
                                                  q = str_glue("'{skeleton_folder_id}' in parents"), n_max = 1)
  return(skeleton_spreadsheet$id)
}


# Function to copy the the respective skeleton which will be used to build observer spreadsheets
# A helper function for `create_spreadsheet` function
#
#  Input: A string specifying the project, 
#         folder dribble indicating the path to the skeleton,
#         Name of new spreadsheet after copying
# Output: A copied skeleton spreadsheet with new name in the selected destination

copy_skeleton <- function(project, folder_dribble, spreadsheet_name) {
  skeleton <- case_when(
    project %in% projects$`Tel Aviv Transects`         ~ get_skeleton_id("Tel Aviv Skeleton 2.0"),
    project %in% projects$`Eilat Transects`            ~ get_skeleton_id("Eilat Skeleton - Transects"),
    project %in% projects$`Eilat Knolls`               ~ get_skeleton_id("Eilat Skeleton - Knolls"),
    project %in% projects$`Mediterranean Transects`    ~ get_skeleton_id("Bioblitz Skeleton"),
    project %in% projects$`Eilat Juveniles Transects`  ~ get_skeleton_id("Eilat Juvies Skeleton"),
    project %in% projects$`Eilat Juveniles Knolls`     ~ get_skeleton_id("Eilat Juvies Skeleton"),
    project %in% projects$`Eilat Juveniles Artificial` ~ get_skeleton_id("Eilat Juvies Skeleton")
  )
  
  googledrive::local_drive_quiet()
  googledrive::drive_cp(file = skeleton,
                        path = folder_dribble,
                        name = spreadsheet_name)
}


# Function to generate worksheets within a spreadsheet based on the project
# A helper function for `create_spreadsheets`function
#
#  Input: A character string stating the project name, 
#         a deployment ID, a spreadsheet ID, and the two observer names
# Output: The appropriate worksheets generated with input spreadsheet

create_observer_working_sheets <- function(project, deployment, spreadsheet, location,  
                                           observer1, observer2, j_observer1, j_observer2) {
  if (project %in% projects$`Tel Aviv Transects`) {
    for (transect_letter in LETTERS[2:1]){
      sheet_identifier <- str_glue("{deployment} - {transect_letter}")
      googlesheets4::sheet_copy(from_ss = spreadsheet,from_sheet = "Observer Invertebrates - MASTER",to_ss = spreadsheet,
                                to_sheet = str_glue("{sheet_identifier} - Invertebrates"))
      googlesheets4::range_write(ss = spreadsheet,sheet = str_glue("{sheet_identifier} - Invertebrates"),
                                 col_names = FALSE, range = "B2:B5",
                                 data = as.data.frame(c(observer1, observer2, deployment, transect_letter)),reformat = F)
      googlesheets4::sheet_copy(from_ss = spreadsheet,from_sheet = "Observer Table - MASTER",to_ss = spreadsheet,
                                to_sheet = str_glue("{sheet_identifier} - Fish"))
      googlesheets4::range_write(ss = spreadsheet,sheet = str_glue("{sheet_identifier} - Fish"),
                                 col_names = F, range = "B2:B5",
                                 data = as.data.frame(c(observer1, observer2, deployment, transect_letter)),reformat = F)
    } 
  }
  if (project %in% projects$`Mediterranean Transects`) {
    for (transect_letter in LETTERS[4:1]){
      sheet_identifier <- str_glue("Site {deployment} - Transect {transect_letter}")
      googlesheets4::sheet_copy(from_ss = spreadsheet,from_sheet = "Observer Table - MASTER",to_ss = spreadsheet,
                                to_sheet = sheet_identifier)
      googlesheets4::range_write(ss = spreadsheet,sheet = sheet_identifier,
                                 col_names = F, range = "B2:B5",
                                 data = as.data.frame(c(observer1, observer2, deployment, transect_letter)),reformat = F)
    }
  }
  if (project %in% projects$`Eilat Transects`) { 
    for (transect_letter in LETTERS[4:1]){
      sheet_identifier <- str_glue("Site {deployment} - Transect {transect_letter}")
      googlesheets4::sheet_copy(from_ss = spreadsheet,
                                from_sheet = "Observer Table - Cryptic - MASTER",
                                to_ss = spreadsheet,
                                to_sheet = str_glue("{sheet_identifier} - CRYPTIC"))
      googlesheets4::range_write(ss = spreadsheet,
                                 sheet = str_glue("{sheet_identifier} - CRYPTIC"),
                                 col_names = FALSE,
                                 range = "B2:B5",
                                 data = as.data.frame(c(observer1, observer2, deployment, transect_letter)),
                                 reformat = F)
      googlesheets4::sheet_copy(from_ss = spreadsheet,
                                from_sheet = "Observer Table - MASTER",
                                to_ss = spreadsheet,
                                to_sheet = str_glue("{sheet_identifier} - TRANSIENTS"))
      googlesheets4::range_write(ss = spreadsheet,
                                 sheet = str_glue("{sheet_identifier} - TRANSIENTS"),
                                 col_names = F, 
                                 range = "B2:B5",
                                 data = as.data.frame(c(observer1,observer2,deployment, transect_letter)),reformat = F)
    } 
  }
  if (project %in% projects$`Eilat Knolls`) {
    googlesheets4::sheet_copy(from_ss = spreadsheet,
                              from_sheet = "Observer Table - MASTER",
                              to_ss = spreadsheet,
                              to_sheet = str_glue("Knoll {deployment}"))
    googlesheets4::range_write(ss = spreadsheet,
                               sheet = str_glue("Knoll {deployment}"),
                               col_names = F, 
                               range = "B2:B3",
                               data = as.data.frame(c(observer1,observer2)),reformat = F)
  }
  if (project %in% projects$`Eilat Juveniles Transects`) {
    for (transect_letter in LETTERS[3:1]){
      sheet_identifier <- str_glue("Site {deployment} - Transect {transect_letter}")
      googlesheets4::sheet_copy(from_ss = spreadsheet,
                                from_sheet = "Observer Table - Cryptic - MASTER",
                                to_ss = spreadsheet,
                                to_sheet = str_glue("{sheet_identifier} - CRYPTIC"))
      googlesheets4::range_write(ss = spreadsheet,
                                 sheet = str_glue("{sheet_identifier} - CRYPTIC"),
                                 col_names = FALSE,
                                 range = "B2:B9",
                                 data = as.data.frame(c(observer1, observer2, j_observer1, j_observer2,
                                                        "Transects", location, deployment, transect_letter)),
                                 reformat = F)
      googlesheets4::sheet_copy(from_ss = spreadsheet,
                                from_sheet = "Observer Table - Transients - MASTER",
                                to_ss = spreadsheet,
                                to_sheet = str_glue("{sheet_identifier} - TRANSIENTS"))
      googlesheets4::range_write(ss = spreadsheet,
                                 sheet = str_glue("{sheet_identifier} - TRANSIENTS"),
                                 col_names = F, 
                                 range = "B2:B9",
                                 data = as.data.frame(c(observer1, observer2, j_observer1, j_observer2,
                                                        "Transects", location, deployment, transect_letter)),
                                 reformat = F)
      googlesheets4::sheet_copy(from_ss = spreadsheet,
                                from_sheet = "Juveniles Observer Table - MASTER",
                                to_ss = spreadsheet,
                                to_sheet = str_glue("{sheet_identifier} - Juveniles"))
      googlesheets4::range_write(ss = spreadsheet,
                                 sheet = str_glue("{sheet_identifier} - Juveniles"),
                                 col_names = F, 
                                 range = "B2:B9",
                                 data = as.data.frame(c(observer1, observer2, j_observer1, j_observer2,
                                                        "Transects", location, deployment, transect_letter)),
                                 reformat = F)
    }
  }
  if (project %in% projects$`Eilat Juveniles Knolls`) {
    googlesheets4::sheet_copy(from_ss = spreadsheet,
                              from_sheet = "Observer Table - Transients - MASTER",
                              to_ss = spreadsheet,
                              to_sheet = str_glue("Knoll {deployment}"))
    googlesheets4::range_write(ss = spreadsheet,
                               sheet = str_glue("Knoll {deployment}"),
                               col_names = F, 
                               range = "B2:B8",
                               data = as.data.frame(c(observer1, observer2, j_observer1, j_observer2,
                                                      "Knoll", location, deployment)),
                               reformat = F)
    googlesheets4::sheet_copy(from_ss = spreadsheet,
                              from_sheet = "Juveniles Observer Table - MASTER",
                              to_ss = spreadsheet,
                              to_sheet = str_glue("Juveniles - Knoll {deployment}"))
    googlesheets4::range_write(ss = spreadsheet,
                               sheet = str_glue("Juveniles - Knoll {deployment}"),
                               col_names = F, 
                               range = "B2:B8",
                               data = as.data.frame(c(observer1, observer2, j_observer1, j_observer2,
                                                      "Knoll", location, deployment)),
                               reformat = F)
  }
  if (project %in% projects$`Eilat Juveniles Artificial`) {
    googlesheets4::sheet_copy(from_ss = spreadsheet,
                              from_sheet = "Observer Table - Transients - MASTER",
                              to_ss = spreadsheet,
                              to_sheet = str_glue("Artificial {deployment}"))
    googlesheets4::range_write(ss = spreadsheet,
                               sheet = str_glue("Artificial {deployment}"),
                               col_names = F, 
                               range = "B2:B8",
                               data = as.data.frame(c(observer1, observer2, j_observer1, j_observer2,
                                                      "Artificial", location, deployment)),
                               reformat = F)
    googlesheets4::sheet_copy(from_ss = spreadsheet,
                              from_sheet = "Juveniles Observer Table - MASTER",
                              to_ss = spreadsheet,
                              to_sheet = str_glue("Juveniles - Artificial {deployment}"))
    googlesheets4::range_write(ss = spreadsheet,
                               sheet = str_glue("Juveniles - Artificial {deployment}"),
                               col_names = F, 
                               range = "B2:B8",
                               data = as.data.frame(c(observer1, observer2, j_observer1, j_observer2,
                                                      "Artificial", location, deployment)),
                               reformat = F)
  }
}


# Function to delete existing skeleton sheets in a spreadsheet
# A helper function for `create_spreadsheets` function
#
#  Input: A string specifying the project and target spreadsheet ID 
# Output: Appropriate sheets deleted from worksheet 

delete_skeleton_sheets <- function(project, spreadsheet){
  if (project %in% projects$`Tel Aviv Transects`) {
    googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Observer Table - MASTER")
    googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Observer Invertebrates - MASTER")
  }
  if (project %in% projects$`Mediterranean Transects`) {
    googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Observer Table - MASTER")
  }
  if (project %in% projects$`Eilat Transects`) {
    googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Observer Table - MASTER")
    googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Observer Table - Cryptic - MASTER")
  }
  if (project %in% projects$`Eilat Knolls`) {
    googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Observer Table - MASTER")
  }
  if (project %in% projects$`Eilat Juveniles Transects`) {
    googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Observer Table - Transients - MASTER")
    googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Observer Table - Cryptic - MASTER")
    googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Juveniles Observer Table - MASTER")
  }
  if (project %in% projects$`Eilat Juveniles Knolls`) {
    googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Observer Table - Transients - MASTER")
    googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Observer Table - Cryptic - MASTER")
    googlesheets4::sheet_delete(ss = spreadsheet,sheet = "Juveniles Observer Table - MASTER")
  }
}
