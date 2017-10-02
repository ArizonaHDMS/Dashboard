install.packages('devtools')

devtools::install_github('rstudio/leaflet')
devtools::install_github('bhaskarvk/leaflet.extras')

install.packages('shiny')
install.packages('shinythemes')  # Allows access to bootswatch themes - https://bootswatch.com/
install.packages('shinyjs')  # Provides additional JS functionality for Shiny (used here for enable/disabling buttons)
install.packages('leaflet.esri')  # Allows for using ESRI MapServer layers - https://github.com/bhaskarvk/leaflet.esri
install.packages('DT')  # DataTables install.packages
install.packages('sp')  # The classic Spatial package for R
install.packages('rgdal')  # Works with ESRI shapefiles
install.packages('sf')  # The new tidy spatial package -- very cool stuff in here!
install.packages('mapview')  # To access the popupTable() function to list shapefile attributes in a popup
install.packages('tidyverse')  # Loads up dplyr, readr, ggplot2
install.packages('stringr')  # Tidy package for string manipulation (importantly, works with regular expressions)
install.packages('lubridate')  # Tidy package for working with dates
install.packages('grid')  # See comment for gridExtra
install.packages('gridExtra')  # Important for lining up plots in the "ObsDate Histogram" tab