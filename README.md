# Dashboard
A framework for natural heritage programs to create their own Dashboard in Shiny!

Do you have already Git installed? The best way to run this code is to clone this repository via RStudio, then call up `server.R` and click the "Run App" button (see screenshots below). You will need to have Git setup through RStudio. Check out this [excellent guide](http://happygitwithr.com/) for how to do it. It's somewhat painful but worth it. This repository uses `packrat` to manage all the external packages used in the Dashboard to help ensure this app will run properly.

If you do not want to have to install Git, you can just download this repository. Or, run:  
`shiny::runGitHub("Dashboard", "ArizonaHDMS")`

BUT! This assumes you have all necessary packages already installed on your computer (see `global.R` for a list). I've included a [script you can run](https://github.com/ArizonaHDMS/Dashboard/blob/master/scripts/install_packages.R) to install all of the correct packages. Currently, we are using the development versions of `leaflet` and `leaflet.extra` to use the OSM geocoding tool.

Have questions? Please feel free to contact us about any aspect of the Dashboard:
* Matt King (HDMS Systems Manager) mking [at] azgfd.gov
* Sabra Tonn (HDMS Program Supervisor) stonn [at] azgfd.gov

Plus, watch recordings of our webinars given at NatureServe Network Coordinator's Call:
* https://attendee.gotowebinar.com/recording/4085080807537594114 (day 1)
* https://attendee.gotowebinar.com/recording/3008437922130049538 (day 2)
