# Load packages ---------------------------

library(shiny)
library(shinythemes)  # Allows access to bootswatch themes - https://bootswatch.com/
library(shinyjs)  # Provides additional JS functionality for Shiny (used here for enable/disabling buttons)
library(leaflet)  # Mapping library (this is a development version installed from Github to allow leaflet.extra tools to work, not from the official CRAN repository)
library(leaflet.extras)  # Among other things, adds access to OSM geocoding (and others, e.g., Google geocoder) - https://github.com/bhaskarvk/leaflet.extras
library(leaflet.esri)  # Allows for using ESRI MapServer layers - https://github.com/bhaskarvk/leaflet.esri
library(DT)  # DataTables library
library(sp)  # The classic Spatial package for R
library(rgdal)  # Works with ESRI shapefiles
library(sf)  # The new tidy spatial package -- very cool stuff in here!
library(mapview)  # To access the popupTable() function to list shapefile attributes in a popup
library(tidyverse)  # Loads up dplyr, readr, ggplot2
library(stringr)  # Tidy package for string manipulation (importantly, works with regular expressions)
library(lubridate)  # Tidy package for working with dates
library(grid)  # See comment for gridExtra
library(gridExtra)  # Important for lining up plots in the "ObsDate Histogram" tab

# Load custom map markers ---------------------------

azibaIcon <- makeIcon(
  iconUrl = 'www/img/markers/aziba.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

finsIcon <- makeIcon(
  iconUrl = 'www/img/markers/fins.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

ebird_breedingIcon <- makeIcon(
  iconUrl = 'www/img/markers/ebird-breeding.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

ebird_nonbreedingIcon <- makeIcon(
  iconUrl = 'www/img/markers/ebird-nonbreeding.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

nmcIcon <- makeIcon(
  iconUrl = 'www/img/markers/doesnt-meet-criteria.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

checkedIcon <- makeIcon(
  iconUrl = 'www/img/markers/checked.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

refugiaIcon <- makeIcon(
  iconUrl = 'www/img/markers/refugia.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

suspectIcon <- makeIcon(
  iconUrl = 'www/img/markers/suspect.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

yesIcon <- makeIcon(
  iconUrl = 'www/img/markers/yes.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

blueIcon <- makeIcon(
  iconUrl = 'www/img/markers/marker-blue.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

redIcon <- makeIcon(
  iconUrl = 'www/img/markers/marker-red.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

greenIcon <- makeIcon(
  iconUrl = 'www/img/markers/marker-green.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

blackIcon <- makeIcon(
  iconUrl = 'www/img/markers/marker-black.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

coordIcon <- makeIcon(
  iconUrl = 'www/img/markers/marker-coordinate-green.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

decadal_over_30 <- makeIcon(
  iconUrl = 'www/img/markers/decadal_over30.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

decadal_20_30 <- makeIcon(
  iconUrl = 'www/img/markers/decadal_20-30.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

decadal_15_20 <- makeIcon(
  iconUrl = 'www/img/markers/decadal_15-20.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

decadal_10_15 <- makeIcon(
  iconUrl = 'www/img/markers/decadal_10-15.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

decadal_05_10 <- makeIcon(
  iconUrl = 'www/img/markers/decadal_05-10.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

decadal_02_05 <- makeIcon(
  iconUrl = 'www/img/markers/decadal_02-05.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

decadal_00_02 <- makeIcon(
  iconUrl = 'www/img/markers/decadal_00-02.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

decadal_no_date <- makeIcon(
  iconUrl = 'www/img/markers/decadal_no_date.png',
  iconWidth = 15, iconHeight = 25,
  iconAnchorX = 7.5, iconAnchorY = 25,
  popupAnchorX = -1, popupAnchorY = -15
)

# Define popup content ---------------------------

# Popup content should have HTML formatted as below; do not use R wrappers like strong() or p()
# Alternatively, you could use mapview::popupTable() to generate table of all attributes

popup_marker <- quote(
  paste0(
    "<h4><strong> Point Observation Data </strong></h4>",
    "<strong>IdSource:</strong> ",IdSource, '<br/>',
    "<strong>PO_ID:</strong> ", PO_ID, '<br/>',
    "<strong>ObsDate:</strong> ", ObsDate, '<br/>',
    "<strong>NameUsed:</strong> ", NameUsed, '<br/>',
    "<strong>Count:</strong> ", Count, '<br/>',
    "<strong>County:</strong> ", County, '<br/>',
    "<strong>Directions:</strong> ", Directions, '<br/>',
    "<strong>X:</strong> ", Loc_E_Long, '       ',
    "<strong>Y:</strong> ", Loc_N_Lat, '<br/>',
    "<strong>General Notes:</strong> ", GeneralNotes, '<br/>',
    "<strong>Mapping Notes:</strong> ", MappingNotes, '<br/>',
    "<strong>In HDMS:</strong> ", InHDMS, '<br/>',
    "<strong>EO_ID:</strong> ", EO_ID, '<br/>',
    "<strong>Last Edited User:</strong> ", last_edited_user, '<br/>',
    "<strong>Last Edited Date:</strong> ", last_edited_date, '<br/>',
    actionButton("flag_pod", "Flag", icon=icon('flag'),
                 onclick = 'Shiny.onInputChange(\"flag_pod\",  Math.random())',
                 style="color: #fff; background-color: #5bc0de; border-color: #5bc0de;"),
    actionButton("move_pod", "Move", icon=icon('arrows'),
                 onclick = 'Shiny.onInputChange(\"move_pod\",  Math.random())',
                 style="color: #fff; background-color: #f0ad4e; border-color: #f0ad4e;")
  )
)

popup_eo <- quote(
  paste0(
    '<h4><strong> Element Occurrence </strong></h4>',
    "EO_ID: ", "<a href=", paste0("https://bioticsaz.natureserve.org/biotics/services/page/Eo/", EO_ID, ".html target=\"_blank\">"), EO_ID, '</a> <br/>',
    "Locality:", LOCALITY, '<br/>',
    "Site Name: ", SITENAME, '<br/>',
    "Survey Date: ", SURVEYDATE, '<br/>',
    "FirstObs: ", FIRSTOBS, '<br/>',
    "LastObs: ", LASTOBS, '<br/>',
    "OrigSource: ", ORIG_SOURC, '<br/>',
    "EO Type: ", EOTYPE, '<br/>',
    "EO Rank: ", EORANK, '<br/>',
    "Do Not Exchange?: ", DO_NOT_EXC, '<br/>'
  )
)

popup_abba <- quote(
  paste0(
    '<h4><strong> Arizona Breeding Bird Atlas </strong></h4>',
    BRD_CODE, ' (', BRD_CLASS, '): ', BRD_DESC, '</br>',
    'HAB1: ', HAB_DESC, '</br>',
    'HAB2: ', HAB2_DESC, '</br>',
    'ObsDate: ', OBS_DATE
  )
)

# Define basemap list ---------------------------
basemaps <- c(
  `ESRI World Topo Map` = providers$Esri.WorldTopoMap,
  `ESRI World Imagery` = providers$Esri.WorldImagery,
  `ESRI Imagery with labels` = 'http://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}',
  `NatGeo World Map` = providers$Esri.NatGeoWorldMap,
  `USA Topo Maps` = 'http://server.arcgisonline.com/ArcGIS/rest/services/USA_Topo_Maps/MapServer/tile/{z}/{y}/{x}',
  `Streets` = providers$Esri.WorldStreetMap,
  `Canvas` = providers$Esri.WorldGrayCanvas
)