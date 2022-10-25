library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
library(sp)
library(sf)

# GEOGRAPHY
# downloading geojson and csv of chicago community areas from city open data site
# location of this data https://data.cityofchicago.org/Public-Safety/Boundaries-Police-Beats-current-/aerh-rz74
download.file("https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON",
              "data/source/geo/chicago_community_areas.geojson")

# Read in geojson and then transform to sf format
areas <- st_read("data/source/geo/chicago_community_areas.geojson") %>% st_transform(3857) %>% janitor::clean_names()

# Get demographic data for Census block groups to aggregate/apportion to precinct geography
# Also transforming to match the planar projection of SFPD's beats spatial file
# This also reduces us down to just the numeric population est and geometry
blocks <- get_decennial(geography = "block", 
                        year = 2020,
                        output = 'wide',
                        variables = "P1_001N", 
                        state = "IL",
                        county = c("Cook"),
                        geometry = TRUE) %>%
  rename("population"="P1_001N") %>% 
  select(3) %>%
  janitor::clean_names() %>%
  st_transform(3857)

# Calculate the estimated population of beat geographies/interpolate with tidycensus bgs
# Reminder: ext=true SUMS the population during interpolation
areas_withpop <- st_interpolate_aw(blocks, areas, ext = TRUE)
# Drops geometry so it's not duplicated in the merge
areas_withpop <- st_drop_geometry(areas_withpop)
# Binds that new population column to the table
areas <- cbind(areas,areas_withpop)
# Cleans up unneeded calculation file
# rm(beats_withpop, blocks)

# Check total population assigned/estimated across all beats
# sum(beats$population) 

# Round the population figure; rounded to nearest thousand
areas$population <- round(areas$population,-3)

areas <- areas %>% st_transform(4326)
areas <- st_make_valid(areas)

# saving a clean geojson and separate RDS for use in tracker
file.remove("data/source/geo/areas.geojson")
st_write(beats,"data/source/geo/areas.geojson")
saveRDS(beats,"scripts/rds/areas.rds")

# BARE AREAS MAP JUST FOR TESTING PURPOSES
# CAN COMMENT OUT ONCE FINALIZED
# Set bins for beats pop map
popbins <- c(0,1000, 10000,25000,50000,100000, Inf)
poppal <- colorBin("YlOrRd", areas$population, bins = popbins)
poplabel <- paste(sep = "<br>", areas$area,prettyNum(beats$population, big.mark = ","))

chicago_areas_map <- leaflet(areas) %>%
  setView(-87.65, 41.83, zoom = 10) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "Stamen.TonerLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`))
chicago_areas_map
