# This is what happens in the background...

######## Step 1: Activating Belmaker Lab API for Google Drive and Google Sheets. 

if (file.exists("OAuth/OAuth.R")){
  source("OAuth/OAuth.R")
} else {
  googledrive::drive_auth()
  googlesheets4::gs4_auth() 
}

#  Users should receive these two messages after either Step 1 or Step 1 Alternative:
#  Users should select `1` for both.
#  
#   The googledrive package is requesting access to your Google account.
#   Select a pre-authorised account or enter '0' to obtain a new token. 
#   Press Esc/Ctrl + C to abort.
#   
#   1: lab.belmaker@gmail.com
#  
#   The googlesheets4 package is requesting access to your Google account.
#   Select a pre-authorised account or enter '0' to obtain a new token. 
#   Press Esc/Ctrl + C to abort.
#   
#   1: lab.belmaker@gmail.com


########  Step 2: Sourcing the functions    ################################

source("R/universal_functions.R")
source("R/observer_tables_functions.R")
source("R/create_photo_folders.R")
source("R/master.R")
source("R/reading_functions.R")
source("R/googlesheets input functions.R")

########  Step 3: Creating data input infrastructure  #####################
#
#   This step uses the metadata file to build the infrastructure
#   needed for data input, including:
#     - Expedition directory if needed.
#     - Sampling day folder.
#     - metadata uploaded to designated folder.
#     - Observer spreadsheets.
#     - Photo folders if the project calls for it. 
#     - Emails with writing permission for each surveyor.
#   Spreadsheets are located in:
#   "~/Data Sheets/{this_expedition}/{todays_folder}/

build_framework_from_googlesheet()