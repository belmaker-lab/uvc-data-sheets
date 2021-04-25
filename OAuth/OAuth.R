google_app <- httr::oauth_app(
  appname = "belmaker-sheets",
  key = "971361814917-1t294qa43d60q6mhscapqah8vbkp2ei3.apps.googleusercontent.com",
  secret = "dBy1k_K_ST0s_0OTi-VHYpqu"
)

googledrive::drive_auth_configure(app = google_app)

googledrive::drive_oauth_app()

googledrive::drive_auth() # Login should say that the app "Belmaker Sheets" is trying to access google

googlesheets4::gs4_auth_configure(app = google_app)

googlesheets4::gs4_auth() # Login should say that the app "Belmaker Sheets" is trying to access google
