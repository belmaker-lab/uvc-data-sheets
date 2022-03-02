# Tel Aviv Specific Functions:


# Function to read the metadata columns for invertebrate worksheets in a deployment spreadsheet
# A helper function for `read_invertebrate_worksheet` function.
#
#  Input: a worksheet tibble.
# Output: A tibble containing the deployment metadata in an Invertebrates worksheet

read_invertebrate_metadata <- function(worksheet){
  worksheet[1:14,1:2] %>% 
    pivot_wider(names_from = Metadata, names_sort = FALSE, values_from = Value) %>% 
    mutate(across(.fns = function(x) type.convert(x,as.is = T))) %>% 
    mutate(across(contains(c("Depth","Visib")) & where(is.logical), as.numeric)) %>% 
    mutate(across(where(is.logical),as.character)) %>% 
    return()
}

# Function to read the legend columns in invertebrate worksheets in a deployment spreadsheet
# A helper function for `read_invertebrate_worksheet` function.
#
#  Input: a worksheet tibble.
# Output: A tibble containing the legend displayed in the Invertebrates worksheets

read_legend_columns <- function(worksheet){
  worksheet[17:35,1:2] %>% 
    `colnames<-`(c("Code","Value")) %>% 
    return()
}

# Function to obtain the row and column ranges of quadrates in Invertebrates worksheets.
# This runs automatically within `read_invertebrates_worksheet`
#
#  Input: None
# Output: Several list variables containing the respective rows and columns as described
#         in comments within the function.

get_quadrates_dimensions <- function(){
  # These values represent the respective rows and columns of the cover data for quadrates 1:4.
  # [1:16,4:6] = First quadrate
  # [1:16, 10:12] = Second quadrate
  # [19:34,4:6] = Third quadrate
  # [19:34, 10:12] = Fourth quadrate
  
  row_range <<- list(1:16,19:34)
  meta_cols <<- list(4:6,10:12)
  
  # These values represent the respective columns of the sampling data for quadrates 1:4.
  # [1:16,7:8] = First quadrate
  # [1:16, 13:14] = Second quadrate
  # [18:33, 7:8] = Third quadrate
  # [18:33, 13:14] = Fourth quadrate
  
  counts_cols <<- list(7:8,13:14)
  
  # These next values represent the rows and columns of each complexity value for quadrates 1:4
  # [3,3] = First quadrate
  # [3, 9] = Second quadrate
  # [21, 3] = Third quadrate
  # [21, 9] = Fourth quadrate
  
  complexity_rows <<- list(3,21)
  complexity_cols <<- list(3,9)
}

# Function to read the cover columns in a quadrate
# A helper function for `read_invertebrate_worksheet` function.
#
#  Input: a worksheet tibble, row and column range for the quadrate, and quadrate number
# Output: A tibble containing the quadrate cover data

read_quadrates_cover <- function(worksheet, rows, cols, quadrate){
  
  worksheet[row_range[[rows]], meta_cols[[cols]]] %>% 
    `colnames<-`(c("Point","Code","Species")) %>% 
    mutate(Quadrate = quadrate,
           "Complexity" =  as.numeric(worksheet[[complexity_rows[[rows]],complexity_cols[[cols]]]]),
           Point = unlist(Point)) %>% 
    return()
}

# Function to read the sample columns in a quadrate
# A helper function for `read_invertebrate_worksheet` function.
#
#  Input: a worksheet tibble, row and column range for the quadrate, and quadrate number
# Output: A tibble containing the quadrate samples data

read_quadrates_counts <- function(worksheet, rows, cols, quadrate){
  worksheet[row_range[[rows]], counts_cols[[cols]]] %>% 
    `colnames<-`(c("Taxon","Count")) %>% 
    mutate(Quadrate = quadrate) %>% 
    filter(!is.na(Taxon)) %>% 
    mutate(Count = as.numeric(Count)) %>% 
    return()
}

# Function to read an invertebrate worksheet within a deployment
#
#  Input: a spreadsheet ID and a worksheet identifier
# Output: A tibble describing the invertebrates survey data

read_invertebrate_worksheet <- function(spreadsheet_id, sheet_identifier){
  worksheet <- googlesheets4::read_sheet(spreadsheet_id, sheet_identifier, col_types = "c",
                                         .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))
  Sys.sleep(2)
  meta_data <- read_invertebrate_metadata(worksheet)
  legend <- read_legend_columns(worksheet)
  
  get_quadrates_dimensions()
  
  quadrate <- 1
  cover <- vector(mode = "list", length = 4)
  counts <- vector(mode = "list", length = 4)
  
  for (i in 1:2){
    for (j in 1:2){
      cover[[quadrate]] <- read_quadrates_cover(worksheet, i, j, quadrate)
      counts[[quadrate]] <- read_quadrates_counts(worksheet, i, j, quadrate)
      quadrate <- quadrate + 1
    }
  }
  
  # Manipulate orientation of the data tables:
  
  quadrates_cover <-  bind_rows(cover) %>% 
    select(Quadrate, Complexity, Point, Code, Species) 
  
  quadrates_count <- bind_rows(counts) %>% 
    select(Quadrate, Taxon, Count)
  
  quadrate_cover_code <- quadrates_cover %>% 
    select(-Species)
  
  quadrates_cover_species <- quadrates_cover %>% 
    select(-Code) %>% 
    pivot_wider(names_from = Point, values_from = Species)
  
  # Merge all the tables:
  
  quadrate_wide <- quadrate_cover_code %>% 
    right_join(legend, by = "Code") %>% 
    count(Quadrate, Complexity, Value) %>% 
    mutate(Percent = 100*n/16) %>%
    select(-n) %>% 
    pivot_wider(names_from = Value, values_from = Percent, values_fill = 0) %>% 
    filter(!is.na(Quadrate))
  
  quadrate_wide <- bind_cols(meta_data,quadrate_wide) %>% 
    left_join(quadrates_count, by = "Quadrate") %>% 
    left_join(quadrates_cover_species, by = c("Quadrate", "Complexity"))
  
  return(quadrate_wide)
}

# Function to read all worksheets in a deployment spreadsheet
#
#  Input: Spreadsheet ID object of the deployment.
# Output: A tibble containing all invertebrate sample data and metadata within the deployment

read_invertebrate_deployment_spreadsheet <- function(spreadsheet_id){
  samples <- googlesheets4::sheet_properties(googlesheets4::as_sheets_id(spreadsheet_id)) %>% 
    filter(!str_detect(string = name,"Fish|Species List|Readme.txt|Quadrate data")) %>% 
    .$name
  
  all_worksheets <- lapply(samples, function(sheet_identifier)
    read_invertebrate_worksheet(spreadsheet_id, sheet_identifier))
  
  all_worksheets %>% 
    bind_rows %>% 
    return()
}

# Function to read all spreadsheets in a folder, focusing on invertebrate worksheets
# Feature: Added ability to supply a vector of
#          spreadsheets names to be read.
#
#  Input: folder dribble (from `get_day_folder_dribble` function)
# Output: A tibble containing all data from the 
#         supplied folder or alternatively, from
#         selected spreadsheets.

read_invertebrates_sampling_day_data <- function(folder_dribble, names = NULL){
  if (is.null(names)){
    spreadsheets <- googledrive::drive_ls(path = folder_dribble) %>% 
      filter(!name  %in% c("Metadata","Photos","COMPLETE DATA")) %>% 
      .$id
  }
  else{
    spreadsheets <- googledrive::drive_ls(path = folder_dribble) %>% 
      filter(name %in% names) %>% 
      .$id
  }
  
  sampling_day_data <- lapply(spreadsheets, function(spreadsheet_id){
    read_invertebrate_deployment_spreadsheet(spreadsheet_id)
  })
  
  sampling_day_data %>%
    bind_rows() %>%
    return()
}

# Function that adds the length-weight and origin of fish species
# based on the google sheet `Species Traits`. 
# A helper function for `format_tlv_sheet` function.
#
#  Input: a tibble
# Output: A tibble with `a`, `b`, `IUCN`, `weight_g`,
#         and `weight_kg`  columns for each species

add_species_traits <- function(complete_data){
  species_list <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1HcbT0_3crAhTy8pNXzpjT75HJFuIzJSmvIjB3TBfU0c",
                                            sheet = 1)
  left_join(complete_data, species_list, by = "Species") %>%
    mutate(across(c(Amount,Length),.fns = as.numeric)) %>% 
    mutate(weight_g = Amount * (a * (Length ^ b))) %>%
    mutate(weight_kg = weight_g/1000) %>% 
    return()
}

# Function to add segment number according to site
# A helper function for `format_tlv_sheet` function.
#
#  Input: a tibble
# Output: A tibble with Site_number column, and Segment column renamed to Site

add_site_number <- function(complete_data){
  
  segments <- data.frame(
    stringsAsFactors = FALSE,
    Site_number = c(1:14), # Called "Site_number" in the formatted tibble, so we call it site here
    Segment = c("Zuk_North","Zuk_Tel_Baruch", # Segment will be renamed to site late in this function
                "Tel_Baruch","Sde_Dov","Reading","TLV_Port",
                "Mezizim_Seperate_Hilton","Marina_Gordon_Frishman",
                "Bugrashov_Jerusalem_Aviv","Dolfinarium","Charles_Clore","Jaffa",
                "Jaffa_Slope","Givat_Alia")
  )
  
  left_join(complete_data, segments, by = "Segment") %>% 
    mutate(Transect_id = paste(.$meta_to_deployment_id,.$Date,.$Transect,sep = "_"),
           Survey_id = paste(.$meta_to_deployment_id,.$Date, sep = "_")) %>% 
    mutate(Site = Segment) %>% # Rename Segment to Site
    return()
}

# Function to add Season column to complete data
# A helper function for `format_tlv_sheet` function.
#
#  Input: a tibble
# Output: A tibble with Season column

add_season <- function(complete_data){
  complete_data %>% 
    mutate(Season = case_when(
      lubridate::month(Date) %in% c(12, 1, 2) ~ "Winter",
      lubridate::month(Date) %in% c(3, 4, 5) ~ "Spring",
      lubridate::month(Date) %in% c(6, 7, 8) ~ "Summer",
      lubridate::month(Date) %in% c(9, 10, 11) ~ "Fall"
    )) %>% 
    return()
}

# A function to format fish datasets
# A helper function for `format_tlv_sheet` function.
#
#  Input: a tibble
# Output: A formatted tibble

format_fish <- function(complete_data) {
  complete_data %>%
    rename(`Fish_Observer` = `Fish Observer`,
           `Invertebrate_Observer` = `Invertebrate Observer`) %>%
    mutate(Date = lubridate::ymd(Date)) %>%
    mutate(Transect_id = paste(.$meta_to_deployment_id,.$Date,.$Transect,sep = "_"),
           Survey_id = paste(.$meta_to_deployment_id,.$Date, sep = "_")) %>%
    add_species_traits() %>% 
    add_site_number() %>% 
    add_season() %>% 
    select(Project, Site,
           Site_number, Survey_id,
           Spot, Date, Season,
           Fish_Observer,
           Invertebrate_Observer,
           Lat, Lon, Transect,
           Transect_id, Substrate_type = `Substrate Type`, `Depth Start`,
           `Depth End`, `Time Start`, `Time End`, Visibility, Logger, Photos,
           Notes, Group, Species, Amount, Length, Distance, Confidence, a, b, weight_kg, IUCN,
           `1`, `2`, `3`, `4`, `5`,
           `6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`, `14`, `15`, `16`, `17`, `18`, `19`,
           `20`, `21`, `22`, `23`, `24`, `25`) %>%
    return()
}

# A function to format invertebrates datasets
# A helper function for `format_tlv_sheet` function.
#
#  Input: a tibble
# Output: A formatted tibble

format_invertebrates <- function(complete_data){
  complete_data %>% 
    rename(`Fish_Observer` = `Fish Observer`,
           `Invertebrate_Observer` = `Invertebrate Observer`) %>%
    mutate(Date = lubridate::ymd(Date)) %>%
    mutate(Transect_id = paste(.$meta_to_deployment_id,.$Date,.$Transect,sep = "_"),
           Survey_id = paste(.$meta_to_deployment_id,.$Date, sep = "_"),
           Quadrate_id = paste(Transect_id, Quadrate), sep = "_") %>%
    add_site_number() %>% 
    add_season() %>% 
    mutate(Method = "Quadrate") %>% 
    select(Project, Site, Site_number, Survey_id, Spot, Date, Season,
           Fish_Observer, Invertebrate_Observer, Group, Method, Lat, Lon, Transect, 
           Transect_id, Depth_Start = `Depth Start`, Depth_End = `Depth End`, 
           Time_Start = `Time Start`, Time_End = `Time End`, Visibility, Logger, Photos, Notes,
           Substrate_type = `Substrate Type`, Quadrate_number = Quadrate, Quadrate_id, Taxon, Count,
           Complexity, `Algae >2cm`, Bivalvia, Bryozoa, Cnidaria, Crustacea, Echinodermata, Gastropoda,
           `Other live cover`, Polychaeta, Rock, Sand, Tunicata, `CCA Algae`, `Algae <2cm`, Sponge,
           `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`, `14`, `15`, `16`) %>% 
    return() 
}


# Function to format the output tlv tibble according to
# the current format. 
# A helper function for `download_tlv_day_complete_data` function.
# 
#  Input: A tibble 
# Output: A reformatted tibble

format_tlv_sheet <- function(complete_data){
  
  if (unique(complete_data$Group) == "Fish") return(format_fish(complete_data))
  if (unique(complete_data$Group) == "Invertebrates") return(format_invertebrates(complete_data))
}

# Function to join day metadata tibble with sampling data, and format it
# Features:
#          Can upload complete tibble to a COMPLETE DATA
#          folder within the day's folder.
#          Select between either fish or invertebrates samples.
#
#  Input: Expedition name, folder name, optional: 
#         logical value stating whether to upload
#         or not (DEFAULT)
# Output: A tibble containing all of the sampling day data,
#         alternatively - uploaded into a created folder within
#         folder name.

create_tlv_day_complete_data <- function(expedition_name, folder_name, group, upload_individual_days = TRUE){
  
  folder_dribble <- get_day_folder_dribble(expedition_name, folder_name)
  
  day_metadata <- download_meta_sheet(folder_dribble)
  
  if (unique(day_metadata$Date) < lubridate::dmy("1/11/2021")) 
    stop(glue::glue("This function is designed for use with TLV sheets created November 2021 onwards!")) 
  
  if (group == "Fish") {
    day_sample_data <- read_sampling_day_data(day_metadata, folder_dribble) %>% 
      mutate(Group = "Fish")
  }
  
  if (group == "Invertebrates") {
    day_sample_data <- read_invertebrates_sampling_day_data(folder_dribble) %>% 
      mutate(Group = "Invertebrates")
  }
  
  if (unique(day_metadata$Project) %in% projects$`Tel Aviv Transects`)
  {day_sample_data <- mutate(day_sample_data, meta_to_deployment_id = str_glue(
    "{`Fish Observer`} and {`Invertebrate Observer`} - {Site}"))}
  
  
  day_complete_data <- left_join(day_metadata, day_sample_data, by = c("meta_to_deployment_id")) %>% 
    format_tlv_sheet()
  
  if (upload_individual_days) {
    search_results <- googledrive::with_drive_quiet(
      googledrive::drive_find(pattern = "COMPLETE DATA", type = "folder", n_max = 1,
                              q = str_glue("'{folder_dribble$id}' in parents")))
    complete_data_folder <-  search_results
    if (nrow(search_results) == 0) {
      complete_data_folder <- googledrive::drive_mkdir(name = "COMPLETE DATA", 
                                                       overwrite = TRUE, path = folder_dribble)
    }
    
    day_spreadsheet <- googlesheets4::gs4_create(name = str_glue("{folder_dribble$name} - {group}"),
                                                 sheets = list(Data = day_complete_data))
    
    googledrive::drive_mv(file = day_spreadsheet, path = complete_data_folder)
  }
  
  return(day_complete_data)
}
