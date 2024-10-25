library(tidyverse)
library(sf)
library(readxl)
library(zoo)

options(timeout=300)
# One-time download of annual slices of Chicago crime incidents from city's open data site
#download.file("https://data.cityofchicago.org/api/views/w98m-zvie/rows.csv","data/source/annual/chicago2019.csv")
#download.file("https://data.cityofchicago.org/api/views/qzdf-xmn8/rows.csv","data/source/annual/chicago2020.csv")
#download.file("https://data.cityofchicago.org/api/views/dwme-t96c/rows.csv","data/source/annual/chicago2021.csv")

# Download previous year updated data for current year from city open data site
download.file("https://data.cityofchicago.org/api/views/9hwr-2zxp/rows.csv",
              "data/source/recent/chicago2022.csv")

download.file("https://data.cityofchicago.org/api/views/xguy-4ndq/rows.csv",
              "data/source/recent/chicago2023.csv")

# Download latest updated data for current year from city open data site

download.file("https://data.cityofchicago.org/api/views/dqcy-ctma/rows.csv",
             "data/source/recent/chicago2024.csv")

# import the archived annual files
chicago2019 <- read_csv("data/source/annual/chicago2019.csv", 
                        col_types = cols(ID = col_character(), 
                                         Arrest = col_character(), Domestic = col_character(), 
                                         Ward = col_character(), `Community Area` = col_character(), 
                                         `X Coordinate` = col_skip(), `Y Coordinate` = col_skip()))
chicago2020 <- read_csv("data/source/annual/chicago2020.csv", 
                        col_types = cols(ID = col_character(), 
                                         Arrest = col_character(), Domestic = col_character(), 
                                         Ward = col_character(), `Community Area` = col_character(), 
                                         `X Coordinate` = col_skip(), `Y Coordinate` = col_skip()))
chicago2021 <- read_csv("data/source/annual/chicago2021.csv", 
                        col_types = cols(ID = col_character(), 
                                         Arrest = col_character(), Domestic = col_character(), 
                                         Ward = col_character(), `Community Area` = col_character(), 
                                         `X Coordinate` = col_skip(), `Y Coordinate` = col_skip()))

# import the latest 2023 annual files in same format as previous annual archives
chicago2022 <- read_csv("data/source/recent/chicago2022.csv", 
                        col_types = cols(ID = col_character(), 
                                         Arrest = col_character(), Domestic = col_character(), 
                                         Ward = col_character(), `Community Area` = col_character(), 
                                         `X Coordinate` = col_skip(), `Y Coordinate` = col_skip()))

# import the latest annual files in same format as previous annual archives
chicago2023 <- read_csv("data/source/recent/chicago2023.csv", 
                        col_types = cols(ID = col_character(), 
                                         Arrest = col_character(), Domestic = col_character(), 
                                         Ward = col_character(), `Community Area` = col_character(), 
                                         `X Coordinate` = col_skip(), `Y Coordinate` = col_skip()))

# import the latest annual files in same format as previous annual archives
chicago2024 <- read_csv("data/source/recent/chicago2024.csv", 
                        col_types = cols(ID = col_character(), 
                                         Arrest = col_character(), Domestic = col_character(), 
                                         Ward = col_character(), `Community Area` = col_character(), 
                                         `X Coordinate` = col_skip(), `Y Coordinate` = col_skip()))

### COMBINE 2019, 2020, 2021 file with 2022 year to date update file
chicago_crime <- bind_rows(chicago2019,chicago2020,chicago2021,chicago2022,chicago2023,chicago2024) %>% janitor::clean_names()
rm(chicago2019,chicago2020,chicago2021,chicago2022,chicago2023,chicago2024)

# Create cleaned date, month, hour columns for tracker charts
# eliminate unnecessarily duplicative date,location fields
chicago_crime$date <- lubridate::mdy_hms(chicago_crime$date)
chicago_crime$hour <- lubridate::hour(chicago_crime$date)
chicago_crime$month <- lubridate::floor_date(as.Date(chicago_crime$date),"month")
chicago_crime$date <- substr(chicago_crime$date,1,10)
chicago_crime$date <- ymd(chicago_crime$date)
chicago_crime$updated_on <- NULL
chicago_crime$location <- NULL

# Import crime classification codes crosswalk created from Chicago PD codebooks
chicago_class_codes <- read_csv("data/source/reference/chicago_crime_classifications_2024.csv")

# merge
chicago_class_codes$category <- as.character(chicago_class_codes$category)
chicago_crime <- left_join(chicago_crime,chicago_class_codes %>% select(1,4,5),by="iucr")

# If community area is blank, add word Unknown
chicago_crime$community_area[is.na(chicago_crime$community_area)] <- "Unknown"

# clean up premise names throughout file
# the case when is stored once as a value by separate script
chicago_crime$location_description <- case_when(chicago_crime$location_description == 'AIRPORT BUILDING NON-TERMINAL - NON-SECURE AREA' ~ 'Airport',
                                                chicago_crime$location_description == 'AIRPORT BUILDING NON-TERMINAL - SECURE AREA' ~ 'Airport',
                                                chicago_crime$location_description == 'AIRPORT EXTERIOR - NON-SECURE AREA' ~ 'Airport',
                                                chicago_crime$location_description == 'AIRPORT EXTERIOR - SECURE AREA' ~ 'Airport',
                                                chicago_crime$location_description == 'AIRPORT PARKING LOT' ~ 'Airport',
                                                chicago_crime$location_description == 'AIRPORT TERMINAL LOWER LEVEL - NON-SECURE AREA' ~ 'Airport',
                                                chicago_crime$location_description == 'AIRPORT TERMINAL LOWER LEVEL - SECURE AREA' ~ 'Airport',
                                                chicago_crime$location_description == 'AIRPORT TERMINAL MEZZANINE - NON-SECURE AREA' ~ 'Airport',
                                                chicago_crime$location_description == 'AIRPORT TERMINAL UPPER LEVEL - NON-SECURE AREA' ~ 'Airport',
                                                chicago_crime$location_description == 'AIRPORT TERMINAL UPPER LEVEL - SECURE AREA' ~ 'Airport',
                                                chicago_crime$location_description == 'AIRPORT TRANSPORTATION SYSTEM (ATS)' ~ 'Airport',
                                                chicago_crime$location_description == 'AIRPORT VENDING ESTABLISHMENT' ~ 'Airport',
                                                chicago_crime$location_description == 'AIRPORT/AIRCRAFT' ~ 'Airport',
                                                chicago_crime$location_description == 'ANIMAL HOSPITAL' ~ 'Business',
                                                chicago_crime$location_description == 'APARTMENT' ~ 'Residence',
                                                chicago_crime$location_description == 'APPLIANCE STORE' ~ 'Business',
                                                chicago_crime$location_description == 'ATHLETIC CLUB' ~ 'Business',
                                                chicago_crime$location_description == 'ATM (AUTOMATIC TELLER MACHINE)' ~ 'Bank',
                                                chicago_crime$location_description == 'AUTO' ~ 'Automobile',
                                                chicago_crime$location_description == 'AUTO / BOAT / RV DEALERSHIP' ~ 'Business',
                                                chicago_crime$location_description == 'BANK' ~ 'Bank',
                                                chicago_crime$location_description == 'BANQUET HALL' ~ 'Business',
                                                chicago_crime$location_description == 'BAR OR TAVERN' ~ 'Bar, Tavern or Club',
                                                chicago_crime$location_description == 'BARBER SHOP/BEAUTY SALON' ~ 'Business',
                                                chicago_crime$location_description == 'BARBERSHOP' ~ 'Business',
                                                chicago_crime$location_description == 'BASEMENT' ~ 'Residence',
                                                chicago_crime$location_description == 'BOAT / WATERCRAFT' ~ 'Watercraft',
                                                chicago_crime$location_description == 'BOAT/WATERCRAFT' ~ 'Watercraft',
                                                chicago_crime$location_description == 'BOWLING ALLEY' ~ 'Business',
                                                chicago_crime$location_description == 'CAR WASH' ~ 'Business',
                                                chicago_crime$location_description == 'CHA APARTMENT' ~ 'Housing Authority',
                                                chicago_crime$location_description == 'CHA ELEVATOR' ~ 'Housing Authority',
                                                chicago_crime$location_description == 'CHA GROUNDS' ~ 'Housing Authority',
                                                chicago_crime$location_description == 'CHA HALLWAY' ~ 'Housing Authority',
                                                chicago_crime$location_description == 'CHA HALLWAY / STAIRWELL / ELEVATOR' ~ 'Housing Authority',
                                                chicago_crime$location_description == 'CHA HALLWAY/STAIRWELL/ELEVATOR' ~ 'Housing Authority',
                                                chicago_crime$location_description == 'CHA LOBBY' ~ 'Housing Authority',
                                                chicago_crime$location_description == 'CHA PARKING LOT' ~ 'Housing Authority',
                                                chicago_crime$location_description == 'CHA PARKING LOT / GROUNDS' ~ 'Housing Authority',
                                                chicago_crime$location_description == 'CHA PARKING LOT/GROUNDS' ~ 'Housing Authority',
                                                chicago_crime$location_description == 'CHA PLAY LOT' ~ 'Housing Authority',
                                                chicago_crime$location_description == 'CHURCH / SYNAGOGUE / PLACE OF WORSHIP' ~ 'Place of Worship',
                                                chicago_crime$location_description == 'CHURCH/SYNAGOGUE/PLACE OF WORSHIP' ~ 'Place of Worship',
                                                chicago_crime$location_description == 'CLEANING STORE' ~ 'Business',
                                                chicago_crime$location_description == 'CLUB' ~ 'Bar, Tavern or Club',
                                                chicago_crime$location_description == 'COIN OPERATED MACHINE' ~ 'Other',
                                                chicago_crime$location_description == 'COLLEGE / UNIVERSITY - GROUNDS' ~ 'College or University',
                                                chicago_crime$location_description == 'COLLEGE / UNIVERSITY - RESIDENCE HALL' ~ 'College or University',
                                                chicago_crime$location_description == 'COLLEGE/UNIVERSITY GROUNDS' ~ 'College or University',
                                                chicago_crime$location_description == 'COLLEGE/UNIVERSITY RESIDENCE HALL' ~ 'College or University',
                                                chicago_crime$location_description == 'COMMERCIAL / BUSINESS OFFICE' ~ 'Business',
                                                chicago_crime$location_description == 'CONVENIENCE STORE' ~ 'Business',
                                                chicago_crime$location_description == 'CREDIT UNION' ~ 'Bank',
                                                chicago_crime$location_description == 'CTA "L" PLATFORM' ~ 'Transit',
                                                chicago_crime$location_description == 'CTA "L" TRAIN' ~ 'Transit',
                                                chicago_crime$location_description == 'CTA BUS' ~ 'Transit',
                                                chicago_crime$location_description == 'CTA BUS STOP' ~ 'Transit Property',
                                                chicago_crime$location_description == 'CTA GARAGE / OTHER PROPERTY' ~ 'Transit Property',
                                                chicago_crime$location_description == 'CTA PARKING LOT / GARAGE / OTHER PROPERTY' ~ 'Transit Property',
                                                chicago_crime$location_description == 'CTA PLATFORM' ~ 'Transit',
                                                chicago_crime$location_description == 'CTA PROPERTY' ~ 'Transit Property',
                                                chicago_crime$location_description == 'CTA STATION' ~ 'Transit Property',
                                                chicago_crime$location_description == 'CTA SUBWAY STATION' ~ 'Transit Property',
                                                chicago_crime$location_description == 'CTA TRACKS - RIGHT OF WAY' ~ 'Transit Property',
                                                chicago_crime$location_description == 'CTA TRAIN' ~ 'Transit',
                                                chicago_crime$location_description == 'CURRENCY EXCHANGE' ~ 'Business',
                                                chicago_crime$location_description == 'DAY CARE CENTER' ~ 'Business',
                                                chicago_crime$location_description == 'DEPARTMENT STORE' ~ 'Business',
                                                chicago_crime$location_description == 'DRIVEWAY' ~ 'Other',
                                                chicago_crime$location_description == 'DRIVEWAY - RESIDENTIAL' ~ 'Residence',
                                                chicago_crime$location_description == 'DRUG STORE' ~ 'Business',
                                                chicago_crime$location_description == 'ELEVATOR' ~ 'Other',
                                                chicago_crime$location_description == 'FACTORY / MANUFACTURING BUILDING' ~ 'Business',
                                                chicago_crime$location_description == 'FACTORY/MANUFACTURING BUILDING' ~ 'Business',
                                                chicago_crime$location_description == 'FEDERAL BUILDING' ~ 'Government Building or Property',
                                                chicago_crime$location_description == 'FIRE STATION' ~ 'Government Building or Property',
                                                chicago_crime$location_description == 'FOREST PRESERVE' ~ 'Recreation Area',
                                                chicago_crime$location_description == 'GANGWAY' ~ 'Other',
                                                chicago_crime$location_description == 'GARAGE' ~ 'Parking Lot/Garage',
                                                chicago_crime$location_description == 'GAS STATION' ~ 'Business',
                                                chicago_crime$location_description == 'GAS STATION DRIVE/PROP.' ~ 'Business',
                                                chicago_crime$location_description == 'GOVERNMENT BUILDING / PROPERTY' ~ 'Government Building or Property',
                                                chicago_crime$location_description == 'GOVERNMENT BUILDING/PROPERTY' ~ 'Government Building or Property',
                                                chicago_crime$location_description == 'GROCERY FOOD STORE' ~ 'Business',
                                                chicago_crime$location_description == 'HALLWAY' ~ 'Other',
                                                chicago_crime$location_description == 'HIGHWAY / EXPRESSWAY' ~ 'Street or Highway',
                                                chicago_crime$location_description == 'HIGHWAY/EXPRESSWAY' ~ 'Street or Highway',
                                                chicago_crime$location_description == 'HORSE STABLE' ~ 'Other',
                                                chicago_crime$location_description == 'HOSPITAL' ~ 'Medical Facility',
                                                chicago_crime$location_description == 'HOSPITAL BUILDING / GROUNDS' ~ 'Medical Facility',
                                                chicago_crime$location_description == 'HOSPITAL BUILDING/GROUNDS' ~ 'Medical Facility',
                                                chicago_crime$location_description == 'HOTEL' ~ 'Hotel or Motel',
                                                chicago_crime$location_description == 'HOTEL / MOTEL' ~ 'Hotel or Motel',
                                                chicago_crime$location_description == 'HOTEL/MOTEL' ~ 'Hotel or Motel',
                                                chicago_crime$location_description == 'HOUSE' ~ 'Residence',
                                                chicago_crime$location_description == 'JAIL / LOCK-UP FACILITY' ~ 'Jail or Prison',
                                                chicago_crime$location_description == 'KENNEL' ~ 'Other',
                                                chicago_crime$location_description == 'LAKE' ~ 'Recreation Area',
                                                chicago_crime$location_description == 'LAKEFRONT / WATERFRONT / RIVERBANK' ~ 'Recreation Area',
                                                chicago_crime$location_description == 'LAKEFRONT/WATERFRONT/RIVERBANK' ~ 'Recreation Area',
                                                chicago_crime$location_description == 'LIBRARY' ~ 'Government Building or Property',
                                                chicago_crime$location_description == 'LIQUOR STORE' ~ 'Business',
                                                chicago_crime$location_description == 'MEDICAL / DENTAL OFFICE' ~ 'Medical Facility',
                                                chicago_crime$location_description == 'MEDICAL/DENTAL OFFICE' ~ 'Medical Facility',
                                                chicago_crime$location_description == 'MOTEL' ~ 'Hotel or Motel',
                                                chicago_crime$location_description == 'MOVIE HOUSE / THEATER' ~ 'Business',
                                                chicago_crime$location_description == 'MOVIE HOUSE/THEATER' ~ 'Business',
                                                chicago_crime$location_description == 'NEWSSTAND' ~ 'Business',
                                                chicago_crime$location_description == 'NURSING / RETIREMENT HOME' ~ 'Medical Facility',
                                                chicago_crime$location_description == 'NURSING HOME' ~ 'Medical Facility',
                                                chicago_crime$location_description == 'NURSING HOME/RETIREMENT HOME' ~ 'Medical Facility',
                                                chicago_crime$location_description == 'OFFICE' ~ 'Business',
                                                chicago_crime$location_description == 'OTHER (SPECIFY)' ~ 'Other',
                                                chicago_crime$location_description == 'OTHER COMMERCIAL TRANSPORTATION' ~ 'Other Transportation Facility',
                                                chicago_crime$location_description == 'OTHER RAILROAD PROP / TRAIN DEPOT' ~ 'Other Transportation Facility',
                                                chicago_crime$location_description == 'OTHER RAILROAD PROPERTY / TRAIN DEPOT' ~ 'Other Transportation Facility',
                                                chicago_crime$location_description == 'PARK PROPERTY' ~ 'Recreation Area',
                                                chicago_crime$location_description == 'PARKING LOT' ~ 'Parking Lot/Garage',
                                                chicago_crime$location_description == 'PARKING LOT / GARAGE (NON RESIDENTIAL)' ~ 'Parking Lot/Garage',
                                                chicago_crime$location_description == 'PARKING LOT/GARAGE(NON.RESID.)' ~ 'Parking Lot/Garage',
                                                chicago_crime$location_description == 'PAWN SHOP' ~ 'Business',
                                                chicago_crime$location_description == 'POLICE FACILITY / VEHICLE PARKING LOT' ~ 'Police Facility',
                                                chicago_crime$location_description == 'POLICE FACILITY/VEH PARKING LOT' ~ 'Police Facility',
                                                chicago_crime$location_description == 'POOL ROOM' ~ 'Recreation Area',
                                                chicago_crime$location_description == 'PORCH' ~ 'Residence',
                                                chicago_crime$location_description == 'RAILROAD PROPERTY' ~ 'Other Transportation Facility',
                                                chicago_crime$location_description == 'RESIDENCE' ~ 'Residence',
                                                chicago_crime$location_description == 'RESIDENCE - GARAGE' ~ 'Residence',
                                                chicago_crime$location_description == 'RESIDENCE - PORCH / HALLWAY' ~ 'Residence',
                                                chicago_crime$location_description == 'RESIDENCE - YARD (FRONT / BACK)' ~ 'Residence',
                                                chicago_crime$location_description == 'RESIDENCE PORCH/HALLWAY' ~ 'Residence',
                                                chicago_crime$location_description == 'RESIDENCE-GARAGE' ~ 'Residence',
                                                chicago_crime$location_description == 'RESIDENTIAL YARD (FRONT/BACK)' ~ 'Residence',
                                                chicago_crime$location_description == 'RESTAURANT' ~ 'Business',
                                                chicago_crime$location_description == 'RETAIL STORE' ~ 'Business',
                                                chicago_crime$location_description == 'RIVER BANK' ~ 'Recreation Area',
                                                chicago_crime$location_description == 'SAVINGS AND LOAN' ~ 'Bank',
                                                chicago_crime$location_description == 'SCHOOL - PRIVATE BUILDING' ~ 'School',
                                                chicago_crime$location_description == 'SCHOOL - PRIVATE GROUNDS' ~ 'School',
                                                chicago_crime$location_description == 'SCHOOL - PUBLIC BUILDING' ~ 'School',
                                                chicago_crime$location_description == 'SCHOOL - PUBLIC GROUNDS' ~ 'School',
                                                chicago_crime$location_description == 'SCHOOL YARD' ~ 'School',
                                                chicago_crime$location_description == 'SCHOOL, PRIVATE, BUILDING' ~ 'School',
                                                chicago_crime$location_description == 'SCHOOL, PRIVATE, GROUNDS' ~ 'School',
                                                chicago_crime$location_description == 'SCHOOL, PUBLIC, BUILDING' ~ 'School',
                                                chicago_crime$location_description == 'SCHOOL, PUBLIC, GROUNDS' ~ 'School',
                                                chicago_crime$location_description == 'SMALL RETAIL STORE' ~ 'Business',
                                                chicago_crime$location_description == 'SPORTS ARENA / STADIUM' ~ 'Sports Facility',
                                                chicago_crime$location_description == 'SPORTS ARENA/STADIUM' ~ 'Sports Facility',
                                                chicago_crime$location_description == 'STAIRWELL' ~ 'Other',
                                                chicago_crime$location_description == 'STREET' ~ 'Street or Highway',
                                                chicago_crime$location_description == 'TAVERN' ~ 'Bar, Tavern or Club',
                                                chicago_crime$location_description == 'TAVERN / LIQUOR STORE' ~ 'Bar, Tavern or Club',
                                                chicago_crime$location_description == 'TAVERN/LIQUOR STORE' ~ 'Bar, Tavern or Club',
                                                chicago_crime$location_description == 'TAXICAB' ~ 'Vehicle',
                                                chicago_crime$location_description == 'TRAILER' ~ 'Vehicle',
                                                chicago_crime$location_description == 'TRUCK' ~ 'Vehicle',
                                                chicago_crime$location_description == 'VACANT LOT / LAND' ~ 'Vacant Lot',
                                                chicago_crime$location_description == 'VACANT LOT/LAND' ~ 'Vacant Lot',
                                                chicago_crime$location_description == 'VEHICLE - COMMERCIAL' ~ 'Vehicle',
                                                chicago_crime$location_description == 'VEHICLE - COMMERCIAL: ENTERTAINMENT / PARTY BUS' ~ 'Vehicle',
                                                chicago_crime$location_description == 'VEHICLE - COMMERCIAL: TROLLEY BUS' ~ 'Vehicle',
                                                chicago_crime$location_description == 'VEHICLE - DELIVERY TRUCK' ~ 'Vehicle',
                                                chicago_crime$location_description == 'VEHICLE - OTHER RIDE SHARE SERVICE (E.G., UBER, LYFT)' ~ 'Vehicle',
                                                chicago_crime$location_description == 'VEHICLE - OTHER RIDE SHARE SERVICE (LYFT, UBER, ETC.)' ~ 'Vehicle',
                                                chicago_crime$location_description == 'VEHICLE NON-COMMERCIAL' ~ 'Vehicle',
                                                chicago_crime$location_description == 'VEHICLE-COMMERCIAL' ~ 'Vehicle',
                                                chicago_crime$location_description == 'VEHICLE-COMMERCIAL - ENTERTAINMENT/PARTY BUS' ~ 'Vehicle',
                                                chicago_crime$location_description == 'VEHICLE-COMMERCIAL - TROLLEY BUS' ~ 'Vehicle',
                                                chicago_crime$location_description == 'VESTIBULE' ~ 'Other',
                                                chicago_crime$location_description == 'WAREHOUSE' ~ 'Business',
                                                chicago_crime$location_description == 'NA' ~ 'Other',
                                                TRUE ~ str_to_title(chicago_crime$location_description, locale = "en"))

# Before we save, we're going to delete the most recent day in the file's crimes
# tldr version is it's a partial day and often a very small portion of the day
chicago_crime <- chicago_crime %>% filter(date<max(chicago_crime$date))

# Get latest date in our file and save for
# automating the updated date text in building tracker
asofdate <- max(chicago_crime$date)
saveRDS(asofdate,"scripts/rds/asofdate.rds")

# write csv of Chicago crime as a backup
# full csv save; only for redundancy
write_csv(chicago_crime,"data/output/chicago_crime.csv")
saveRDS(chicago_crime,"scripts/rds/chicago_crime.rds")

# Extract the last 12 months into a separate file
chicago_crime_last12 <- chicago_crime %>% filter(date>(max(chicago_crime$date)-365))

### CITYWIDE CRIME TOTALS AND OUTPUT

# Set variable of Chicago population
# likely needs added to the tracker itself
chicago_population <- 2665064

# Calculate of each detailed offense type CITYWIDE
citywide_detailed <- chicago_crime %>%
  group_by(category,description,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_detailed <- citywide_detailed %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024")
# add last 12 months
citywide_detailed_last12 <- chicago_crime_last12 %>%
  group_by(category,description) %>%
  summarise(last12mos = n())
citywide_detailed <- left_join(citywide_detailed,citywide_detailed_last12,by=c("category","description"))
# remove anything where 
# add zeros where there were no crimes tallied that year
citywide_detailed$total19[is.na(citywide_detailed$total19)] <- 0
citywide_detailed$total20[is.na(citywide_detailed$total20)] <- 0
citywide_detailed$total21[is.na(citywide_detailed$total21)] <- 0
citywide_detailed$total22[is.na(citywide_detailed$total22)] <- 0
citywide_detailed$total23[is.na(citywide_detailed$total23)] <- 0
citywide_detailed$total24[is.na(citywide_detailed$total24)] <- 0
citywide_detailed$last12mos[is.na(citywide_detailed$last12mos)] <- 0

rm(citywide_detailed_last12)
# Calculate a total across the 3 prior years
citywide_detailed$total_prior3years <- citywide_detailed$total21+citywide_detailed$total22+citywide_detailed$total23
citywide_detailed$avg_prior3years <- round(citywide_detailed$total_prior3years/3,1)
# calculate increases
citywide_detailed$inc_19to23 <- round(citywide_detailed$total23/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_19tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_23tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total23*100-100,1)
citywide_detailed$inc_prior3yearavgtolast12 <- round((citywide_detailed$last12mos/citywide_detailed$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_detailed$rate19 <- round(citywide_detailed$total19/chicago_population*100000,1)
citywide_detailed$rate20 <- round(citywide_detailed$total20/chicago_population*100000,1)
citywide_detailed$rate21 <- round(citywide_detailed$total21/chicago_population*100000,1)
citywide_detailed$rate22 <- round(citywide_detailed$total22/chicago_population*100000,1)
citywide_detailed$rate23 <- round(citywide_detailed$total23/chicago_population*100000,1)
citywide_detailed$rate_last12 <- round(citywide_detailed$last12mos/chicago_population*100000,1)
# calculate a multiyear rate
citywide_detailed$rate_prior3years <- round(citywide_detailed$avg_prior3years/chicago_population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
citywide_detailed <- citywide_detailed %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
citywide_detailed <- citywide_detailed %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

# Calculate of each detailed offense type CITYWIDE
citywide_detailed_monthly <- chicago_crime %>%
  group_by(category,description,month) %>%
  summarise(count = n())
# add rolling average of 3 months for chart trend line & round to clean
citywide_detailed_monthly <- citywide_detailed_monthly %>%
  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
citywide_detailed_monthly$rollavg_3month <- round(citywide_detailed_monthly$rollavg_3month,0)
# write to save for charts for detailed monthly
write_csv(citywide_detailed_monthly,"data/output/monthly/citywide_detailed_monthly.csv")
citywide_detailed_monthly %>% filter(description=="RETAIL THEFT") %>% write_csv("data/output/monthly/retail_theft_monthly.csv")

# Calculate of each category of offense CITYWIDE
citywide_category <- chicago_crime %>%
  group_by(category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_category <- citywide_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024")
# add last 12 months
citywide_category_last12 <- chicago_crime_last12 %>%
  group_by(category) %>%
  summarise(last12mos = n())
citywide_category <- left_join(citywide_category,citywide_category_last12,by=c("category"))
# add zeros where there were no crimes tallied that year
# citywide_category[is.na(citywide_category)] <- 0

citywide_category$total19[is.na(citywide_category$total19)] <- 0
citywide_category$total20[is.na(citywide_category$total20)] <- 0
citywide_category$total21[is.na(citywide_category$total21)] <- 0
citywide_category$total22[is.na(citywide_category$total22)] <- 0
citywide_category$total23[is.na(citywide_category$total23)] <- 0
citywide_category$total24[is.na(citywide_category$total24)] <- 0
citywide_category$last12mos[is.na(citywide_category$last12mos)] <- 0

# Calculate a total across the 3 prior years
citywide_category$total_prior3years <- citywide_category$total21+citywide_category$total22+citywide_category$total23
citywide_category$avg_prior3years <- round(citywide_category$total_prior3years/3,1)
# calculate increases
citywide_category$inc_19to23 <- round(citywide_category$total23/citywide_category$total19*100-100,1)
citywide_category$inc_19tolast12 <- round(citywide_category$last12mos/citywide_category$total19*100-100,1)
citywide_category$inc_23tolast12 <- round(citywide_category$last12mos/citywide_category$total23*100-100,1)
citywide_category$inc_prior3yearavgtolast12 <- round((citywide_category$last12mos/citywide_category$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_category$rate19 <- round(citywide_category$total19/chicago_population*100000,1)
citywide_category$rate20 <- round(citywide_category$total20/chicago_population*100000,1)
citywide_category$rate21 <- round(citywide_category$total21/chicago_population*100000,1)
citywide_category$rate22 <- round(citywide_category$total22/chicago_population*100000,1)
citywide_category$rate23 <- round(citywide_category$total23/chicago_population*100000,1)

citywide_category$rate_last12 <- round(citywide_category$last12mos/chicago_population*100000,1)
# calculate a multiyear rate
citywide_category$rate_prior3years <- round(citywide_category$avg_prior3years/chicago_population*100000,1)

# Calculate monthly totals for categories of crimes CITYWIDE
citywide_category_monthly <- chicago_crime %>%
  group_by(category,month) %>%
  summarise(count = n())
# add rolling average of 3 months for chart trend line & round to clean
citywide_category_monthly <- citywide_category_monthly %>%
  arrange(category,month) %>%
  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
citywide_category_monthly$rollavg_3month <- round(citywide_category_monthly$rollavg_3month,0)

# write series of monthly files for charts (NOTE murder is written above in detailed section)
write_csv(citywide_category_monthly,"data/output/monthly/citywide_category_monthly.csv")
citywide_category_monthly %>% filter(category=="Criminal Sexual Assault") %>% write_csv("data/output/monthly/sexassaults_monthly.csv")
citywide_category_monthly %>% filter(category=="Motor Vehicle Theft") %>% write_csv("data/output/monthly/autothefts_monthly.csv")
citywide_category_monthly %>% filter(category=="Theft Over $500") %>% write_csv("data/output/monthly/thefts_monthly.csv")
citywide_category_monthly %>% filter(category=="Burglary") %>% write_csv("data/output/monthly/burglaries_monthly.csv")
citywide_category_monthly %>% filter(category=="Robbery") %>% write_csv("data/output/monthly/robberies_monthly.csv")
citywide_category_monthly %>% filter(category=="Aggravated Battery") %>% write_csv("data/output/monthly/batteries_monthly.csv")
citywide_category_monthly %>% filter(category=="Murder") %>% write_csv("data/output/monthly/murders_monthly.csv")

# Calculate of each type of crime CITYWIDE
citywide_type <- chicago_crime %>%
  group_by(type,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_type <- citywide_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024")
# add last 12 months
citywide_type_last12 <- chicago_crime_last12 %>%
  group_by(type) %>%
  summarise(last12mos = n())
citywide_type <- left_join(citywide_type,citywide_type_last12,by=c("type"))
# Calculate a total across the 3 prior years
citywide_type$total_prior3years <- citywide_type$total21+citywide_type$total22+citywide_type$total23
citywide_type$avg_prior3years <- round(citywide_type$total_prior3years/3,1)
# add zeros where there were no crimes tallied that year
# citywide_type[is.na(citywide_type)] <- 0
citywide_type$total19[is.na(citywide_type$total19)] <- 0
citywide_type$total20[is.na(citywide_type$total20)] <- 0
citywide_type$total21[is.na(citywide_type$total21)] <- 0
citywide_type$total22[is.na(citywide_type$total22)] <- 0
citywide_type$total23[is.na(citywide_type$total23)] <- 0
citywide_type$total24[is.na(citywide_type$total24)] <- 0
citywide_type$last12mos[is.na(citywide_type$last12mos)] <- 0
# calculate increases
citywide_type$inc_19to23 <- round(citywide_type$total23/citywide_type$total19*100-100,1)
citywide_type$inc_19tolast12 <- round(citywide_type$last12mos/citywide_type$total19*100-100,1)
citywide_type$inc_23tolast12 <- round(citywide_type$last12mos/citywide_type$total23*100-100,1)
citywide_type$inc_prior3yearavgtolast12 <- round((citywide_type$last12mos/citywide_type$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_type$rate19 <- round(citywide_type$total19/chicago_population*100000,1)
citywide_type$rate20 <- round(citywide_type$total20/chicago_population*100000,1)
citywide_type$rate21 <- round(citywide_type$total21/chicago_population*100000,1)
citywide_type$rate22 <- round(citywide_type$total22/chicago_population*100000,1)
citywide_type$rate23 <- round(citywide_type$total23/chicago_population*100000,1)
citywide_type$rate_last12 <- round(citywide_type$last12mos/chicago_population*100000,1)
# calculate a multiyear rate
citywide_type$rate_prior3years <- round(citywide_type$avg_prior3years/chicago_population*100000,1)

### CHICAGO POLICE BEAT CRIME TOTALS AND OUTPUT

# MERGE WITH BEATS GEOGRAPHY AND POPULATION
# Geography and populations processed separately in 
# source(process_chicago_areas_map.R)
areas <- st_read("data/source/geo/areas.geojson")

# we need these unique lists for making the beat tables below
# this ensures that we get crime details for beats even with zero
# incidents of certain types over the entirety of the time period
list_area_category <- crossing(community_area = unique(chicago_crime$community_area), category = unique(chicago_crime$category))
list_area_type <- crossing(community_area = unique(chicago_crime$community_area), type = unique(chicago_crime$type))

# Test that all beats show in data and identify beat #s that do not
# areasindata <- chicago_crime %>% group_by(community_area,year) %>% summarise(count=n()) %>% pivot_wider(names_from=year, values_from=count)
# anti_join(areasindata,areas,by="community_area")
# OPEN WORK: Only 1 total record in 2020; we can go back and manually geocode later

# Calculate total of each detailed offense type by community area
area_detailed <- chicago_crime %>%
  group_by(community_area,category,description,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
area_detailed <- area_detailed %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024")
# add last 12 months
area_detailed_last12 <- chicago_crime_last12 %>%
  group_by(community_area,category,description) %>%
  summarise(last12mos = n())
area_detailed <- left_join(area_detailed,area_detailed_last12,by=c("community_area","category","description"))
rm(area_detailed_last12)
# add zeros where there were no crimes tallied that year
area_detailed[is.na(area_detailed)] <- 0
# Calculate a total across the 3 prior years
area_detailed$total_prior3years <- area_detailed$total21+area_detailed$total22+area_detailed$total23
area_detailed$avg_prior3years <- round(area_detailed$total_prior3years/3,1)
# calculate increases
area_detailed$inc_19to23 <- round(area_detailed$total23/area_detailed$total19*100-100,1)
area_detailed$inc_19tolast12 <- round(area_detailed$last12mos/area_detailed$total19*100-100,1)
area_detailed$inc_23tolast12 <- round(area_detailed$last12mos/area_detailed$total23*100-100,1)
area_detailed$inc_prior3yearavgtolast12 <- round((area_detailed$last12mos/area_detailed$avg_prior3years)*100-100,0)
# add population for beats
area_detailed <- full_join(areas,area_detailed,by=c("community_area"="community_area"))
# calculate the beat by beat rates PER 1K people
area_detailed$rate19 <- round(area_detailed$total19/area_detailed$population*100000,1)
area_detailed$rate20 <- round(area_detailed$total20/area_detailed$population*100000,1)
area_detailed$rate21 <- round(area_detailed$total21/area_detailed$population*100000,1)
area_detailed$rate22 <- round(area_detailed$total22/area_detailed$population*100000,1)
area_detailed$rate23 <- round(area_detailed$total23/area_detailed$population*100000,1)
area_detailed$rate_last12 <- round(area_detailed$last12mos/area_detailed$population*100000,1)
# calculate a multiyear rate
area_detailed$rate_prior3years <- round(area_detailed$avg_prior3years/area_detailed$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
area_detailed <- area_detailed %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
area_detailed <- area_detailed %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

# Calculate total of each category of offense BY POLICE BEAT
area_category <- chicago_crime %>%
  group_by(community_area,category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# merging with full list so we have data for every beat, every category_name
area_category <- left_join(list_area_category,area_category,by=c("community_area"="community_area","category"="category"))
# rename the year columns
area_category <- area_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024")
# add last 12 months
area_category_last12 <- chicago_crime_last12 %>%
  group_by(community_area,category) %>%
  summarise(last12mos = n())
area_category <- left_join(area_category,area_category_last12,by=c("community_area","category"))
rm(area_category_last12)
# add zeros where there were no crimes tallied that year
area_category[is.na(area_category)] <- 0
# Calculate a total across the 3 prior years
area_category$total_prior3years <- area_category$total21+area_category$total22+area_category$total23
area_category$avg_prior3years <- round(area_category$total_prior3years/3,1)
# calculate increases
area_category$inc_19to23 <- round(area_category$total23/area_category$total19*100-100,1)
area_category$inc_19tolast12 <- round(area_category$last12mos/area_category$total19*100-100,1)
area_category$inc_23tolast12 <- round(area_category$last12mos/area_category$total23*100-100,1)
area_category$inc_prior3yearavgtolast12 <- round((area_category$last12mos/area_category$avg_prior3years)*100-100,0)
# add population for beats
area_category <- full_join(areas,area_category,by=c("community_area"="community_area"))
# calculate the beat by beat rates PER 1K people
area_category$rate19 <- round(area_category$total19/area_category$population*100000,1)
area_category$rate20 <- round(area_category$total20/area_category$population*100000,1)
area_category$rate21 <- round(area_category$total21/area_category$population*100000,1)
area_category$rate22 <- round(area_category$total22/area_category$population*100000,1)
area_category$rate23 <- round(area_category$total23/area_category$population*100000,1)
area_category$rate_last12 <- round(area_category$last12mos/area_category$population*100000,1)

# calculate a multiyear rate
area_category$rate_prior3years <- round(area_category$avg_prior3years/area_category$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
area_category <- area_category %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
area_category <- area_category %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

# Calculate total of each type of crime BY POLICE BEAT
area_type <- chicago_crime %>%
  group_by(community_area,type,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# merging with full list so we have data for every beat, every type
area_type <- left_join(list_area_type,area_type,by=c("community_area"="community_area","type"="type"))
# rename the year columns
area_type <- area_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024")
# add last 12 months
area_type_last12 <- chicago_crime_last12 %>%
  group_by(community_area,type) %>%
  summarise(last12mos = n())
area_type <- left_join(area_type,area_type_last12,by=c("community_area","type"))
rm(area_type_last12)
# add zeros where there were no crimes tallied that year
area_type[is.na(area_type)] <- 0
# Calculate a total across the 3 prior years
area_type$total_prior3years <- area_type$total21+area_type$total22+area_type$total23
area_type$avg_prior3years <- round(area_type$total_prior3years/3,1)
# calculate increases
area_type$inc_19to23 <- round(area_type$total23/area_type$total19*100-100,1)
area_type$inc_19tolast12 <- round(area_type$last12mos/area_type$total19*100-100,1)
area_type$inc_23tolast12 <- round(area_type$last12mos/area_type$total23*100-100,1)
area_type$inc_prior3yearavgtolast12 <- round((area_type$last12mos/area_type$avg_prior3years)*100-100,0)
# add population for beats
area_type <- full_join(areas,area_type,by=c("community_area"="community_area"))
# calculate the beat by beat rates PER 1K people
area_type$rate19 <- round(area_type$total19/area_type$population*100000,1)
area_type$rate20 <- round(area_type$total20/area_type$population*100000,1)
area_type$rate21 <- round(area_type$total21/area_type$population*100000,1)
area_type$rate22 <- round(area_type$total22/area_type$population*100000,1)
area_type$rate23 <- round(area_type$total23/area_type$population*100000,1)
area_type$rate_last12 <- round(area_type$last12mos/area_type$population*100000,1)
# calculate a multiyear rate
area_type$rate_prior3years <- round(area_type$avg_prior3years/area_type$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
area_type <- area_type %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
area_type <- area_type %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

# output various csvs for basic tables to be made with crime totals
# we are dropping geometry for beats here because this is just for tables
area_detailed %>% st_drop_geometry() %>% write_csv("data/output/areas/area_detailed.csv")
area_category %>% st_drop_geometry() %>% write_csv("data/output/areas/area_category.csv")
area_type %>% st_drop_geometry() %>% write_csv("data/output/areas/area_type.csv")
citywide_detailed %>% write_csv("data/output/city/citywide_detailed.csv")
citywide_category %>% write_csv("data/output/city/citywide_category.csv")
citywide_type %>% write_csv("data/output/city/citywide_type.csv")

# Create individual spatial tables of crimes by major categories and types
murders_area <- area_category %>% filter(category=="Murder")
sexassaults_area <- area_category %>% filter(category=="Criminal Sexual Assault")
autothefts_area <- area_category %>% filter(category=="Motor Vehicle Theft")
thefts_area <- area_category %>% filter(category=="Theft Over $500")
burglaries_area <- area_category %>% filter(category=="Burglary")
robberies_area <- area_category %>% filter(category=="Robbery")
batteries_area <- area_category %>% filter(category=="Aggravated Battery")
violence_area <- area_type %>% filter(type=="People")
property_area <- area_type %>% filter(type=="Property")
retailthefts_area <- area_detailed %>% filter(description=="RETAIL THEFT")
# Create same set of tables for citywide figures
murders_city <- citywide_category %>% filter(category=="Murder")
sexassaults_city <- citywide_category %>% filter(category=="Criminal Sexual Assault")
autothefts_city <- citywide_category %>% filter(category=="Motor Vehicle Theft")
thefts_city <- citywide_category %>% filter(category=="Theft Over $500")
burglaries_city <- citywide_category %>% filter(category=="Burglary")
robberies_city <- citywide_category %>% filter(category=="Robbery")
batteries_city <- citywide_category %>% filter(category=="Aggravated Battery")
violence_city <- citywide_type %>% filter(type=="People")
property_city <- citywide_type %>% filter(type=="Property")
retailthefts_city <- citywide_detailed %>% filter(description=="RETAIL THEFT")

# Using premise to identify the kinds of places where murders happen
where_murders_happen <- chicago_crime %>%
  filter(category=="Murder") %>%
  group_by(year,location_description) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from=year, values_from=count)
# Using premise to identify the kinds of places where murders happen
where_murders_happen_last12 <- chicago_crime_last12 %>%
  filter(category=="Murder") %>%
  group_by(location_description) %>%
  summarise(last12=n())
# merge last 12 into the table
where_murders_happen <- full_join(where_murders_happen,where_murders_happen_last12,by="location_description")
# add zeros where there were no crimes tallied that year
where_murders_happen[is.na(where_murders_happen)] <- 0
rm(where_murders_happen_last12)

# Using hour to identify the hours of day when murders happen
when_murders_happen <- chicago_crime %>%
  filter(category=="Murder") %>%
  group_by(hour) %>%
  summarise(count=n()) %>% 
  arrange(hour)
when_murders_happen$time <- case_when(when_murders_happen$hour == "0" ~ "12 a.m.",
                                      when_murders_happen$hour %in% c("1","2","3","4","5","6","7","8","9","10","11") ~ paste0(when_murders_happen$hour," a.m."),
                                      when_murders_happen$hour %in% c("12") ~ paste0(when_murders_happen$hour," p.m."),
                                      when_murders_happen$hour %in% c("13","14","15","16","17","18","19","20","21","22","23") ~ paste0((as.numeric(when_murders_happen$hour)-12)," p.m."),
                                      TRUE ~ "Other")
when_murders_happen$timeframe <- case_when(when_murders_happen$hour %in% c("0","1","2","3","4","21","22","23") ~ "Overnight from 9 p.m. to 5 a.m.",
                                           when_murders_happen$hour %in% c("5","6","7","8","9","10","11") ~ "Morning from 5 a.m. to 12 p.m.",
                                           when_murders_happen$hour %in% c("12","13","14","15","16","17","18","19","20")  ~ "Afternoon/Evening from 12 p.m. to 9 p.m.",
                                           TRUE ~ "Other")
when_murders_happen <- when_murders_happen %>%
  group_by(timeframe) %>%
  summarise(total=sum(count))

# Create individual spatial tables of crimes by major categories and types
murders_area %>% st_drop_geometry() %>% write_csv("data/output/areas/murders_area.csv")
sexassaults_area %>% st_drop_geometry() %>% write_csv("data/output/areas/sexassaults_area.csv")
autothefts_area %>% st_drop_geometry() %>% write_csv("data/output/areas/autothefts_area.csv")
thefts_area %>% st_drop_geometry() %>% write_csv("data/output/areas/thefts_area.csv")
burglaries_area %>% st_drop_geometry() %>% write_csv("data/output/areas/burglaries_area.csv")
robberies_area %>% st_drop_geometry() %>% write_csv("data/output/areas/robberies_area.csv")
batteries_area %>% st_drop_geometry() %>% write_csv("data/output/areas/batteries_area.csv")
violence_area %>% st_drop_geometry() %>% write_csv("data/output/areas/violence_area.csv")
property_area %>% st_drop_geometry() %>% write_csv("data/output/areas/property_area.csv")
retailthefts_area %>% st_drop_geometry() %>% write_csv("data/output/areas/retailthefts_area.csv")

# RDS files
saveRDS(murders_city,"scripts/rds/murders_city.rds")
saveRDS(batteries_city,"scripts/rds/batteries_city.rds")
saveRDS(sexassaults_city,"scripts/rds/sexassaults_city.rds")
saveRDS(autothefts_city,"scripts/rds/autothefts_city.rds")
saveRDS(thefts_city,"scripts/rds/thefts_city.rds")
saveRDS(burglaries_city,"scripts/rds/burglaries_city.rds")
saveRDS(robberies_city,"scripts/rds/robberies_city.rds")

saveRDS(murders_area,"scripts/rds/murders_area.rds")
saveRDS(batteries_area,"scripts/rds/batteries_area.rds")
saveRDS(sexassaults_area,"scripts/rds/sexassaults_area.rds")
saveRDS(autothefts_area,"scripts/rds/autothefts_area.rds")
saveRDS(thefts_area,"scripts/rds/thefts_area.rds")
saveRDS(burglaries_area,"scripts/rds/burglaries_area.rds")
saveRDS(robberies_area,"scripts/rds/robberies_area.rds")

# additional table exports for specific charts
where_murders_happen %>% write_csv("data/output/city/where_murders_happen.csv")
when_murders_happen %>% write_csv("data/output/city/when_murders_happen.csv")

# deaths cause data update for TX specific table
deaths <- read_excel("data/source/health/deaths.xlsx") 
deaths <- deaths %>% filter(state=="IL")
deaths$Homicide <- murders_city$rate_last12
write_csv(deaths,"data/source/health/death_rates.csv")

# Transit crimes
# Using premise to identify transit crimes by location
transit_crimes <- chicago_crime %>%
  group_by(year,category,location_description) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from=year, values_from=count)
# Using premise for 12 months
transit_crimes_last12 <- chicago_crime_last12 %>%
  group_by(category,location_description) %>%
  summarise(last12=n())
# merge last 12 into the table
transit_crimes <- full_join(transit_crimes,transit_crimes_last12,by=c("location_description","category"))
# rename the year columns
transit_crimes <- transit_crimes %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024",
         "last12mos" = "last12")
# filter for transit and for major crimes
transit_crimes <- transit_crimes %>% filter(location_description=="Transit")
# add zeros where there were no crimes tallied that year
transit_crimes[is.na(transit_crimes)] <- 0
transit_crimes$total19[is.na(transit_crimes$total19)] <- 0
transit_crimes$total20[is.na(transit_crimes$total20)] <- 0
transit_crimes$total21[is.na(transit_crimes$total21)] <- 0
transit_crimes$total22[is.na(transit_crimes$total22)] <- 0
transit_crimes$total23[is.na(transit_crimes$total23)] <- 0
transit_crimes$total24[is.na(transit_crimes$total24)] <- 0
transit_crimes$last12mos[is.na(transit_crimes$last12mos)] <- 0

rm(transit_crimes_last12)
# Calculate a total across the 3 prior years
transit_crimes$total_prior3years <- transit_crimes$total21+transit_crimes$total22+transit_crimes$total23
transit_crimes$avg_prior3years <- round(transit_crimes$total_prior3years/3,1)
# calculate increases
transit_crimes$inc_19to23 <- round(transit_crimes$total23/transit_crimes$total19*100-100,1)
transit_crimes$inc_19tolast12 <- round(transit_crimes$last12mos/transit_crimes$total19*100-100,1)
transit_crimes$inc_23tolast12 <- round(transit_crimes$last12mos/transit_crimes$total23*100-100,1)
transit_crimes$inc_prior3yearavgtolast12 <- round((transit_crimes$last12mos/transit_crimes$avg_prior3years)*100-100,0)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
transit_crimes <- transit_crimes %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
transit_crimes <- transit_crimes %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
write_csv(transit_crimes,"data/output/city/transit_crimes.csv")
#separate out each violent crime and combine
murder_transit <- transit_crimes %>% filter(category == "Murder")
battery_transit <- transit_crimes %>% filter(category == "Aggravated Battery")
assault_transit <- transit_crimes %>% filter(category == "Aggravated Assault")
sexassaults_transit <- transit_crimes %>% filter(category == "Criminal Sexual Assault")
robbery_transit <- transit_crimes %>%filter(category == "Robbery")

total19 <- sum(murder_transit$total19,battery_transit$total19,assault_transit$total19,sexassaults_transit$total19,robbery_transit$total19)
total20 <- sum(murder_transit$total20,battery_transit$total20,assault_transit$total20,sexassaults_transit$total20,robbery_transit$total20)
total21 <- sum(murder_transit$total21,battery_transit$total21,assault_transit$total21,sexassaults_transit$total21,robbery_transit$total21)
total22 <- sum(murder_transit$total22,battery_transit$total22,assault_transit$total22,sexassaults_transit$total22,robbery_transit$total22)
total23 <- sum(murder_transit$total23,battery_transit$total23,assault_transit$total23,sexassaults_transit$total23,robbery_transit$total23)
total24 <- sum(murder_transit$total24,battery_transit$total24,assault_transit$total24,sexassaults_transit$total24,robbery_transit$total24)
last12mos <- sum(murder_transit$last12mos,battery_transit$last12mos,assault_transit$last12mos,sexassaults_transit$last12mos,robbery_transit$last12mos)
total_prior3years <- sum(murder_transit$total_prior3years,battery_transit$total_prior3years,assault_transit$total_prior3years,sexassaults_transit$total_prior3years,robbery_transit$total_prior3years)





violent_transit <- data.frame(category = c("Violent Crime"),
                              location_description = c("Transit"),
                              total19 = c(total19),
                              total20 = c(total20),
                              total21 = c(total21),
                              total22 = c(total22),
                              total23 = c(total23),
                              total24 = c(total24),
                              last12mos = c(last12mos),
                              total_prior3years = c(total_prior3years)
                              )

violent_transit <- violent_transit %>% mutate(avg_prior3years = round(total_prior3years/3,1))
violent_transit <- violent_transit %>% mutate(inc_19to23 = round(total23/total19*100-100,1))
violent_transit <- violent_transit %>% mutate(inc_19tolast12 = round(last12mos/total19*100-100,1))
violent_transit <- violent_transit %>% mutate(inc_23tolast12 = round(last12mos/total23*100-100,1))
violent_transit <- violent_transit %>% mutate(inc_prior3yearavgtolast12 = round(last12mos/avg_prior3years*100-100,1))
violent_transit <- violent_transit %>% mutate(rate19 = round(total19/chicago_population*100000,1))
violent_transit <- violent_transit %>% mutate(rate20 = round(total20/chicago_population*100000,1))
violent_transit <- violent_transit %>% mutate(rate21 = round(total21/chicago_population*100000,1))
violent_transit <- violent_transit %>% mutate(rate22 = round(total22/chicago_population*100000,1))
violent_transit <- violent_transit %>% mutate(rate23 = round(total23/chicago_population*100000,1))
violent_transit <- violent_transit %>% mutate(rate_last12 = round(last12mos/chicago_population*100000,1))
violent_transit <- violent_transit %>% mutate(rate_prior3years = round(avg_prior3years/chicago_population*100000,1))


#transit by area
violent <- c("Murder","Aggravated Assault","Aggravated Battery","Criminal Sexual Assault","Robbery")
all_violent_transit <- chicago_crime %>% filter(location_description == "Transit") %>% filter(category %in% violent)

cta_area <- all_violent_transit %>%
  group_by(community_area, year) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = year, values_from = count)

cta_area <- cta_area %>%
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024")

violent_transit_last12 <- chicago_crime_last12 %>% filter(location_description == "Transit") %>% filter(category %in% violent)
cta_area_last12 <- violent_transit_last12 %>%
  group_by(community_area) %>%
  summarize(last12mos = n())

cta_area <- left_join(cta_area, cta_area_last12, by = c("community_area"))
rm(violent_transit_last12)
cta_area[is.na(cta_area)] <- 0
cta_area$total_prior3years <- cta_area$total21+cta_area$total22+cta_area$total23
cta_area$avg_prior3years <- round(cta_area$total_prior3years/3,1)
# calculate increases
cta_area$inc_19to23 <- round(cta_area$total23/cta_area$total19*100-100,1)
cta_area$inc_19tolast12 <- round(cta_area$last12mos/cta_area$total19*100-100,1)
cta_area$inc_23tolast12 <- round(cta_area$last12mos/cta_area$total23*100-100,1)
cta_area$inc_prior3yearavgtolast12 <- round((cta_area$last12mos/cta_area$avg_prior3years)*100-100,0)
# add population for beats
cta_area <- full_join(areas,cta_area,by=c("community_area"="community_area"))
# calculate the beat by beat rates PER 1K people
cta_area$rate19 <- round(cta_area$total19/cta_area$population*100000,1)
cta_area$rate20 <- round(cta_area$total20/cta_area$population*100000,1)
cta_area$rate21 <- round(cta_area$total21/cta_area$population*100000,1)
cta_area$rate22 <- round(cta_area$total22/cta_area$population*100000,1)
cta_area$rate23 <- round(cta_area$total23/cta_area$population*100000,1)
cta_area$rate_last12 <- round(cta_area$last12mos/cta_area$population*100000,1)
# calculate a multiyear rate
cta_area$rate_prior3years <- round(cta_area$avg_prior3years/cta_area$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
cta_area <- cta_area %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
cta_area <- cta_area %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

cta_area %>% st_drop_geometry() %>% write_csv("data/output/areas/cta_area.csv")


#Save RDS files
saveRDS(cta_area, "scripts/rds/cta_area.rds")
saveRDS(violent_transit, "scripts/rds/violent_transit.rds")

#Monthly analysis for Datawrapper

cta_monthly <- all_violent_transit %>%
  group_by(month, category) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = category, values_from = count)

cta_monthly %>% write_csv("data/output/monthly/cta_monthly.csv")


