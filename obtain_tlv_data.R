# 
# 
#         ██████╗ ███████╗██╗     ███╗   ███╗ █████╗ ██╗  ██╗███████╗██████╗     ██╗      █████╗ ██████╗
#         ██╔══██╗██╔════╝██║     ████╗ ████║██╔══██╗██║ ██╔╝██╔════╝██╔══██╗    ██║     ██╔══██╗██╔══██╗
#         ██████╔╝█████╗  ██║     ██╔████╔██║███████║█████╔╝ █████╗  ██████╔╝    ██║     ███████║██████╔╝
#         ██╔══██╗██╔══╝  ██║     ██║╚██╔╝██║██╔══██║██╔═██╗ ██╔══╝  ██╔══██╗    ██║     ██╔══██║██╔══██╗
#         ██████╔╝███████╗███████╗██║ ╚═╝ ██║██║  ██║██║  ██╗███████╗██║  ██║    ███████╗██║  ██║██████╔╝
#         ╚═════╝ ╚══════╝╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝    ╚══════╝╚═╝  ╚═╝╚═════╝
#         
#              ██████╗  █████╗ ████████╗ █████╗     ███████╗██╗  ██╗███████╗███████╗████████╗███████╗
#              ██╔══██╗██╔══██╗╚══██╔══╝██╔══██╗    ██╔════╝██║  ██║██╔════╝██╔════╝╚══██╔══╝██╔════╝
#              ██║  ██║███████║   ██║   ███████║    ███████╗███████║█████╗  █████╗     ██║   ███████╗
#              ██║  ██║██╔══██║   ██║   ██╔══██║    ╚════██║██╔══██║██╔══╝  ██╔══╝     ██║   ╚════██║
#              ██████╔╝██║  ██║   ██║   ██║  ██║    ███████║██║  ██║███████╗███████╗   ██║   ███████║
#              ╚═════╝ ╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝    ╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝   ╚═╝   ╚══════╝
# 
# 
# 
# This script downloads the Tel-Aviv project observer sheets from the belmaker lab Google Drive
# 
# Follow the instructions below if you wish to download data from the drive.
# Otherwise, modify the functions in `reading_functions.R` files.

########  Script Prerequisites:   ########################
# 
# This script was written using: 
#   R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# and using the following packages:
#   tidyverse >= 1.3.0
#   googlesheets4 >= 0.2.0
#   googledrive >= 1.0.1
#   lubridate >= 1.7.4
#
# Please make sure to have these installed prior to running this script.


########  Step 1 and Step 1 Alternative   #####################################
#
# Step 1: Activating Belmaker Lab API for Google Drive and Google Sheets. 
#   This runs the `OAuth.R` file which is not available 
#   online (DO NOT SHARE THIS FILE WITH ANYONE!).
#   This step is not necessary. However, it allows this and the `obtain_data.R` 
#   scripts to run with a lower risk of "Quota exceeded" errors. 
#

source("OAuth/OAuth.R")

# Step 1 Alternative
#   Run these lines if you did not run the `OAuth.R` script.

# googledrive::drive_auth()
# googlesheets4::gs4_auth() 

#  Users should receive these two messages after either Step 1 or Step 1 Alternative:
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
#
#   This step loads the `univeral_functions.R` as well as 
#   the `observer_tables_functions.R` and the `reading_functions.R` scripts
#   which loads  the necessary functions to the global environment,
#    along with two variables:
#     projects:            a list containing various projects.
#     email_lookup_table:  a dataframe containing emails of active surveyors

source("R/universal_functions.R")
source("R/observer_tables_functions.R")
source("R/reading_functions.R")

########  Step 3: Selecting expedition folder in Drive ###################
# 
#   Select an expedition folder under Data Sheets in Google Drive
#   and the folder of today's sampling.

this_expedition <- "Tel Aviv Project"
todays_folder   <- "Tel Aviv 2021-11-07"

#######  Step 4: Download the data #############
# 
#   This step saves today's data as an object in R.
#   Additionally, uploads into a folder named COMPLETE DATA

# Fish Data:
download_tlv_day_complete_data(this_expedition, todays_folder, group = "Fish", upload = TRUE)

# Invertebrate Data:
download_tlv_day_complete_data(this_expedition, todays_folder, group = "Invertebrates", upload = TRUE)
