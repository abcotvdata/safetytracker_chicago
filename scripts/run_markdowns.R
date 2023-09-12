library(rmarkdown)

# Script builds each of the pages - or all at once
# Starts by loading data frames created daily in process_chicago_crime.R
# Resulting html files are rendered in /docs for publication via Github Pages

# MURDERS
# Load RDS
murders_area <- readRDS("scripts/rds/murders_area.rds")
murders_city <- readRDS("scripts/rds/murders_city.rds")
asofdate <- readRDS("scripts/rds/asofdate.rds")
# Render page
rmarkdown::render('scripts/Chicago_Safety_Tracker.Rmd', 
                  output_dir = "docs",
                  output_file = 'Chicago_Safety_Tracker.html')

# BURGLARIES
# Load RDS
burglaries_area <- readRDS("scripts/rds/burglaries_area.rds")
burglaries_city <- readRDS("scripts/rds/burglaries_city.rds")
asofdate <- readRDS("scripts/rds/asofdate.rds")
# Render page
rmarkdown::render('scripts/Chicago_Safety_Tracker_Burglaries.Rmd', 
                  output_dir = "docs",
                  output_file = "Chicago_Safety_Tracker_Burglaries.html")

# THEFTS
# Load RDS
thefts_area <- readRDS("scripts/rds/thefts_area.rds")
thefts_city <- readRDS("scripts/rds/thefts_city.rds")
asofdate <- readRDS("scripts/rds/asofdate.rds")
# Render page
rmarkdown::render('scripts/Chicago_Safety_Tracker_Thefts.Rmd', 
                  output_dir = "docs",
                  output_file = 'Chicago_Safety_Tracker_Thefts.html')

# VEHICLE THEFTS
# Load RDS
autothefts_area <- readRDS("scripts/rds/autothefts_area.rds")
autothefts_city <- readRDS("scripts/rds/autothefts_city.rds")
asofdate <- readRDS("scripts/rds/asofdate.rds")
# Render page
rmarkdown::render('scripts/Chicago_Safety_Tracker_VehicleThefts.Rmd', 
                  output_dir = "docs",
                  output_file = 'Chicago_Safety_Tracker_VehicleThefts.html')

# ROBBERIES
# Load RDS
robberies_area <- readRDS("scripts/rds/robberies_area.rds")
robberies_city <- readRDS("scripts/rds/robberies_city.rds")
asofdate <- readRDS("scripts/rds/asofdate.rds")
# Render page
rmarkdown::render('scripts/Chicago_Safety_Tracker_Robberies.Rmd', 
                  output_dir = "docs",
                  output_file = 'Chicago_Safety_Tracker_Robberies.html')

# BATTERIES
# Load RDS
batteries_area <- readRDS("scripts/rds/batteries_area.rds")
batteries_city <- readRDS("scripts/rds/batteries_city.rds")
asofdate <- readRDS("scripts/rds/asofdate.rds")
# Render page
rmarkdown::render('scripts/Chicago_Safety_Tracker_Batteries.Rmd', 
                  output_dir = "docs",
                  output_file = 'Chicago_Safety_Tracker_Batteries.html')

# SEXUAL ASSAULTS
# Load RDS
sexassaults_area <- readRDS("scripts/rds/sexassaults_area.rds")
sexassaults_city <- readRDS("scripts/rds/sexassaults_city.rds")
asofdate <- readRDS("scripts/rds/asofdate.rds")
# Render page
rmarkdown::render('scripts/Chicago_Safety_Tracker_SexualAssaults.Rmd', 
                  output_dir = "docs",
                  output_file = 'Chicago_Safety_Tracker_SexualAssaults.html')

