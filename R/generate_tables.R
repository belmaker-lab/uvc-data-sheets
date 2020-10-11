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
# This script generates observer spreadsheets based on a .csv file describing surveyor deployments like so:
#   
#   +--------------+---------------+---------------+---------------------------+----------------+-----------------+
#   |   Project    |   Location    |     Date      |       Deployment_ID       | First Observer | Second Observer |
#   +--------------+---------------+---------------+---------------------------+----------------+-----------------+
#   | Project_Name | Location_Name | Sampling_Date | Either Knoll, Site,       | Name_1         |    Name2        |
#   | 	           |	             |	             | or Spot identifier        |                |                 |
#   +--------------+---------------+---------------+---------------------------+----------------+-----------------+
#
# (Note: Additional columns may be present)
#
# Follow the instuctions below if you wish to generate tables based on existing templates. 
# Otherwise, modify the functions in `observer_tables_functions.R` files.

########  Script Prerequisites:   ########################
# 
# This script was written using: 
#   R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# and using the following packages:
#   tidyverse >= 1.3.0
#   googlesheets4 >= 0.2.0
#   googledrive >= 1.0.1
#   lubridate >= 1.7.4
#   glue >= 1.4.1
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
#   This will use the `googledrive` and `googlesheets4` APIs
#   which are shared with all other package users (the entire world)

googledrive::drive_auth()
googlesheets4::gs4_auth() 


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
#
#   This step loads the `univeral_functions.R` as well as 
#   the `observer_tables_functions.R` scripts
#   which load  the necessary functions to the global environment,
#    along with two variables:
#     projects:            a list containing various projects.
#     email_lookup_table:  a tibble containing emails of active surveyors

source("R/universal_functions.R")
source("R/observer_tables_functions.R")
source("R/master.R")

########  Step 3: Supplying the metadata table #############
# 
#   This step sets the metadata file path.

file <- "test files/eilat knolls.csv"

########  Step 4: Creating data input infrastructure  #####################
#
#   This step uses the metadata file to build the infrastructure
#   needed for data input, including:
#     - Expedition directory if needed
#     - Sampling day folder
#     - metadata uploaded to designated folder
#     - Observer spreadsheets
#     - Emails with writing permission for each surveyor.
#   Spreadsheets are located in:
#   "~/Data Sheets/{this_expedition}/{todays_folder}/

build_framework(file)

