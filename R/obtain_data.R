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
# This script merges the observer sheets from the belmaker lab Google Drive into a formated sheet and downloads it.
# 
# Follow the instructions below if you wish to download data from the drive.
# Otherwise, modify the functions in `reading_functions.R` files.

########  Script Prerequisites:   ########################
# 
# This script was written using: 
#   R version 4.1.0 (2021-05-18) -- "Camp Pontanezen"
# and using the following packages:
#   tidyverse >= 1.3.1
#   googlesheets4 >= 1.0.0
#   googledrive >= 2.0.0
#   lubridate >= 1.7.10
#   glue >= 1.4.2
#   cli >= 3.0.1
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

this_expedition <- "Eilat June 2020"
todays_folder   <- ""

#######  Step 4: Download the data #############
# 
#   This step saves today's data as an object in R.
#   Additionally, uploads into a folder named COMPLETE DATA

create_specific_day_complete_data(this_expedition, todays_folder, upload_individual_days = TRUE)

#######  Step 5 Combine expedition data #############
# 
#   This step saves the expedition data as an object in R.
#   Additionally, uploads into a folder named EXPEDITION DATA

join_days_data(this_expedition)

#######  Step 5 Alternative: Download entire expedition data #############
# 
#   This step is for performing the individual upload of daily data.
#   Use this if you daily data were not uploaded during the expedition.
#   This step saves the expedition data as an object in R.
#   Additionally, uploads into a folder named EXPEDITION DATA

create_expedition_data(this_expedition, upload_individual_days = TRUE, skip_complete = FALSE)
