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