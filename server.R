# Read data ---------------------------

# Element Tracking table with status and tracking info, this is what popuplates the species drop-down
et <- read_csv('data/et.csv')
et$S_EO_TRACKING_COM %>% stringi::stri_trans_general('latin-ascii') -> et$S_EO_TRACKING_COM  # Need this line to deal with SNEAKY Unicode characters!

# Element Occurrence data
eo <- readOGR('data/eo.shp')
eo <- spTransform(eo, CRS('+proj=longlat'))  # Leaflet expects things to be in EPSG:4326
eo@data %>% mutate_if(is.factor, as.character) -> eo@data
eo@data <- data.frame(cbind(eo@data), update_year = as.numeric(str_sub(eo$LASTOBS,1,4)))

# This code chunk creates the symbology for the EOs
levels <- c(-Inf, 2, 5, 10, 15, 20, 30, Inf)
labels <- c('0-2', '2-5', '5-10', '10-15', '15-20', '20-30', '30+')
time.levels <- cut(2017 - eo$update_year, breaks = levels, labels = labels)
eo@data <- cbind(eo@data, time.levels)

# Color scale (discrete colors from blue to red) for the EOs and POD: Years = 0-2, 2-5, 5-10, 10-15, 15-20, 20-30, 30+
cols<- c('#000080','#0000d3','#ab64c7','#ffff00','#ffc400','#ff8200','#ff0000')

# Associated EO data (this is what we provided in the Access database for data requests)
eo_dump <- read_csv('data/eoDump.csv')

# Point Observation Data
pod <- st_read(dsn = 'data/PointObs.gdb', layer = 'PointObs', stringsAsFactors = F)
pod <- st_transform(pod, '+proj=longlat')  # Leaflet expects things to be in EPSG:4326
pod <- cbind(pod, lng = st_coordinates(pod)[,1], lat = st_coordinates(pod)[,2])
pod$last_edited_date %>% ymd_hms(tz = 'America/Phoenix') -> pod$last_edited_date
pod$created_date %>% ymd_hms(tz = 'America/Phoenix') -> pod$created_date
pod <- pod %>% mutate(update_year = str_sub(ObsDate, 1, 4))
pod <- as.data.frame(pod)

# Arizona Breeding Bird Atlas (example data)
abba_sectors <- st_read('data/abba.shp')

# Critical habitat (example data)
crithab <- readOGR('data/CritHab.shp')
crithab@data %>% mutate_if(is.factor, as.character) -> crithab@data

# SWAP species distribution models (example data)
models <- raster::readAll(raster::stack(list.files('data/models', pattern='.tif$', full.names = T)))

# Lookup tables to make some coding easier
abstract_lookup <- read_csv('data/lookup/abstract_lookup.csv')
taxon_group_lookup <- read_csv('data/lookup/taxon_group_lookup.csv')
coord_type <- read_csv('data/lookup/coord_type.csv')
user_names <- read_csv('data/lookup/username_lookup.csv')


# UI ---------------------------

source('ui_login.R')
source('ui_dashboard.R')

Logged = TRUE;  # Change this value to FALSE if you want to make users login
my_username <- c('username')  # Setting user names as vector
my_password <- c('password')  # Setting passwords as vector (this is NOT good practice. Should be using a package that can hide credentials from code)

ui = (htmlOutput("page"))

server <- function(input, output, session) {
  
  # This section needs to be cleaned up, not using some of these reactive values
  
  id_source_proxy <- dataTableProxy('id_source_table')  # To change to DT
  pod_map_proxy <- dataTableProxy('pod_map_table')  # To change to DT
  pod_active <- reactiveValues()  # RV for pod data
  pod_id <- reactiveValues()  # RV for IdSource table
  pod_bb <- reactiveValues()  # Bounding box for POD
  eo_data <- reactiveValues()
  fix <- reactiveValues(state = FALSE)
  symbolize <- reactiveValues(state = 'OFF')
  coord <- reactiveValues(df= st_sf(st_sfc(crs = 4326)))  # Create blank SF object to hold multiple user-input coordinates
  legend <- reactiveValues(group = c('EO', 'POD'))
  search <- reactiveValues()
  popup <- reactiveValues()
  restrict <- reactiveValues(user = NULL)
  Username <- reactiveValues(name = NULL)
  
  undo <- c(lng=0, lat=0)
  
  USER <- reactiveValues(Logged = Logged)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Username$name <<- input$userName
          print(input$userName)
          Id.username <- which(my_username %in% Username)
          Id.password <- which(my_password %in% Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
              search$list <- et %>%
                             dplyr::select(species = NAME)
            } 
          }
        } 
      }
    }    
  })
  

  observe({
    if (USER$Logged == FALSE) {
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("", ui_login())))
      })
    }
    if (USER$Logged == TRUE) 
    {
      search$list <- et %>%
        dplyr::select(species = NAME)
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("", ui_dashboard())))
      })
    }
  })
  

# Function declarations ---------------------------
  
  add_crithab <- function(){
    legend$group <- c(legend$group, 'CritHab')
    leafletProxy('map') %>%
      clearGroup('CritHab') %>%
      addPolygons(data = crithab[grepl(crithab$name, pattern=input$species), ], 
                  group = 'CritHab',
                  layerId = 40,
                  fillColor = '#f909f9', 
                  color='#f909f9',
                  fillOpacity = 0.3,
                  opacity = 0.5) %>%
      addLayersControl(overlayGroups = legend$group, options = layersControlOptions(collapsed = FALSE))
  }
  
  remove_crithab <- function(){
    legend$group <- legend$group[!legend$group %in% 'CritHab']
    leafletProxy('map') %>%
      clearGroup('CritHab') %>%
      addLayersControl(overlayGroups = legend$group, options = layersControlOptions(collapsed = FALSE))
  }
  
  update_eo <- function(x){
    cf <- colorFactor(rev(cols), x$time.levels)
    
    leafletProxy('map') %>%
      clearGroup('EO') %>%
      removeControl(layerId = 8888) %>%
      addPolygons(data = x,
                  layerId = x$EO_ID,
                  group = 'EO',
                  color=cf(x$time.levels), 
                  fillColor=cf(x$time.levels), 
                  weight=7, 
                  popup = ~eval(popup_eo),
                  fillOpacity = 0.6, 
                  opacity=0.7) %>%
      addLegend("bottomright", pal = cf, values = x$time.levels,
                title = "EO LastObs",
                labFormat = labelFormat(suffix=' yrs'),
                opacity = 1,
                layerId = 8888)
  }
  
  add_model <- function(x){
    legend$group <- c(legend$group, 'SWAP Model')
    leafletProxy('map') %>%
      clearGroup('SWAP Model') %>%
      addRasterImage(x, colors = 'blue', opacity = 0.5, project = F, group = 'SWAP Model') %>%
      hideGroup('SWAP Model') %>%
      addLayersControl(overlayGroups = legend$group, options = layersControlOptions(collapsed = FALSE))
  }
  
  remove_model <- function(){
    legend$group <- legend$group[!legend$group %in% 'SWAP Model']
    leafletProxy('map') %>%
      clearGroup('SWAP Model') %>%
      addLayersControl(overlayGroups = legend$group, options = layersControlOptions(collapsed = FALSE))
  }
  
  add_abba <- function(x){
    codes <- factor(abba_sectors$BRD_CODE, levels = c('CO', 'PR', 'PO', 'OB', 'CP'))
    
    library(RColorBrewer)
    pal <- colorFactor(palette = c('#000022', '#005e7c', '#ffffff', '#f442d4', '#000022'), domain = codes)
    
    legend$group <- c(legend$group, 'ABBA')
    leafletProxy('map') %>% 
      clearGroup('ABBA') %>%
      addPolygons(data = abba_sectors %>% filter(SPECIES == input$species), group = c('ABBA'),
                  stroke = T, weight = 0.5, opacity = 0.8,fillOpacity = 0.8, color = '#000000',fillColor = ~pal(BRD_CODE),
                  popup = ~eval(popup_abba)) %>%
      addLayersControl(overlayGroups = legend$group, options = layersControlOptions(collapsed = FALSE))
  }
  
  remove_abba <- function(){
    legend$group <- legend$group[!legend$group %in% 'ABBA']
    leafletProxy('map') %>%
      clearGroup('ABBA') %>%
      addLayersControl(overlayGroups = legend$group, options = layersControlOptions(collapsed = FALSE))
  }
  
  
update_marker <- function(x){
    
    # The following is a bit clunky, can be made more efficient
  
    if(nrow(x) > 0){
      marker <- character(nrow(x))
      marker[grepl(x$IdSource, pattern = 'eBird Breeding')] <- 'ebird_breeding'
      marker[grepl(x$IdSource, pattern = 'eBird Nonbreeding')] <- 'ebird_nonbreeding'
      marker[grepl(x$IdSource, pattern = 'AZIBA')] <- 'aziba'
      marker[grepl(x$IdSource, pattern = 'ABBA')] <- 'abba'
      marker[grepl(x$InHDMS, pattern = 'Y')] <- 'yes'
      marker[grepl(x$InHDMS, pattern = 'Checked')] <- 'checked'
      marker[grepl(x$InHDMS, pattern = 'Refugia')] <- 'refugia'
      marker[grepl(x$InHDMS, pattern = 'Suspect')] <- 'suspect'
      marker[grepl(x$InHDMS, pattern = 'NMC')] <- 'nmc'
      marker[marker == ''] <- 'other'
      levels <- c('ebird_breeding', 'ebird_nonbreeding', 'aziba', 'abba', 'yes', 'checked', 'refugia', 'suspect', 'nmc', 'other')
      marker <- factor(marker, levels = levels)
      
      myicons <- iconList(
        'ebird_breeding' = ebird_breedingIcon, 
        'ebird_nonbreeding' = ebird_nonbreedingIcon, 
        'aziba' = azibaIcon, 
        'abba' = blackIcon,
        'yes' = yesIcon,
        'checked' = checkedIcon,
        'refugia' = refugiaIcon,
        'suspect' = suspectIcon,
        'nmc' = nmcIcon,
        'other' = blueIcon
      )
      
      leafletProxy('map') %>%
        clearGroup('POD') %>%
        clearGroup('FIX') %>%
        clearPopups() %>%
        addMarkers(data = x,
                   lng=x$lng,
                   lat=x$lat,
                   icon = myicons[marker],
                   layerId = x$PO_ID,
                   group = 'POD',
                   clusterOptions = markerClusterOptions(maxClusterRadius = 1),
                   popup = ~eval(popup_marker))
      
    } else {
      leafletProxy('map') %>%
        clearGroup('POD') %>%
        clearMarkerClusters() %>%
        clearGroup('FIX') %>%
        clearPopups()
    }

  }
  
  update_marker_fix <- function(x){
    leafletProxy('map') %>%
      clearGroup('POD') %>%
      clearGroup('FIX') %>%
      clearPopups() %>%
      addMarkers(lng=x$lng[x$PO_ID != input$map_marker_click$id],
                 lat=x$lat[x$PO_ID != input$map_marker_click$id],
                 icon = blueIcon,
                 layerId = x$PO_ID[x$PO_ID != input$map_marker_click$id],
                 group = 'POD',
                 popup = popup$pod,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 1)) %>%
      
      addMarkers(lng=x$lng[x$PO_ID == input$map_marker_click$id],
                 lat=x$lat[x$PO_ID == input$map_marker_click$id],
                 icon = redIcon,
                 layerId = x$PO_ID[x$PO_ID == input$map_marker_click$id],
                 group = 'FIX',
                 options = markerOptions(draggable = TRUE, zIndexOffset = 1000),
                 popup = paste(sep=" ",
                               actionButton("confirm", "Confirm Move", icon=icon('thumbs-up'), width= '140px',
                                            onclick = 'Shiny.onInputChange(\"confirm_click\",  Math.random())',
                                            style="color: #fff; background-color: #449d44; border-color: #449d44;"), '<br/>',
                               actionButton("move_coord", "Move to Coord", icon=icon('map-marker', 'fa-lg'), width= '140px',
                                            onclick = 'Shiny.onInputChange(\"move_coord\",  Math.random())',
                                            style="color: #fff; background-color: #5bc0de; border-color: #5bc0de;"), '<br/>',
                               actionButton("datum_switch", "Shift Datum", icon=icon('map-marker', 'fa-lg'), width= '140px',
                                            onclick = 'Shiny.onInputChange(\"datum_switch\",  Math.random())',
                                            style="color: #fff; background-color: #9842f4; border-color: #9842f4;"), '<br/>',
                               actionButton("cancel", "Cancel", icon=icon('exclamation-triangle'), width= '140px',
                                            onclick = 'Shiny.onInputChange(\"cancel_click\",  Math.random())',
                                            style="color: #fff; background-color: #d9534f; border-color: #d9534f;")))
  }
  
  clearTable <- function(proxy){
    selectRows(proxy, selected = NULL)
    pod_active$pod <- pod %>% filter(Species == input$species)
    update_marker(pod_active$pod)
  }
  
  output$select <- renderUI({
    selectInput('species', 'Species:', choices = search$list$species)
  })
  
  # Render Status and Tracking Info ---------------------------
  
  observeEvent(input$species,{
    output$comname <- renderUI({
      if(is.null(input$name_choice)){
        tagList(
          strong('Common Name'),
          p(et$COMMON_NAME[et$NAME == input$species])
        )
      } else if(input$name_choice == 'Scientific Name'){
        tagList(
          strong('Common Name'),
          p(et$COMMON_NAME[et$NAME == input$species])
        )
      } else if(input$name_choice == 'Common Name') {
        tagList(
          strong('Scientific Name'),
          p(paste(unlist(et %>% filter(COMMON_NAME == input$species) %>% dplyr::select(NAME)), collapse = ', '))
        )
      }
    })
    output$status <- renderUI({
      tagList(
        strong('Status'),
        p(unique(eo@data %>% filter(COMNAME == input$species | NAME == input$species) %>% dplyr::select(STATUS)))
      )
    })
    output$elcode <- renderUI({
      tagList(
        strong('ELCODE'),
        p(et %>% filter(COMMON_NAME == input$species | NAME == input$species) %>% dplyr::select(ELCODE_BCD))
      )
    })
    output$esa <- renderUI({
      tagList(
        strong('ESA'),
        p(et %>% filter(COMMON_NAME == input$species | NAME == input$species) %>% dplyr::select(ESA))
      )
    })
    output$usfs <- renderUI({
      tagList(
        strong('USFS'),
        p(et %>% filter(COMMON_NAME == input$species | NAME == input$species) %>% dplyr::select(USFS))
      )
    })
    output$blm <- renderUI({
      tagList(
        strong('BLM'),
        p(et %>% filter(COMMON_NAME == input$species | NAME == input$species) %>% dplyr::select(BLM))
      )
    })
    output$sgcn <- renderUI({
      tagList(
        strong('SGCN'),
        p(et %>% filter(COMMON_NAME == input$species | NAME == input$species) %>% dplyr::select(SGCN))
      )
    })
    output$npl <- renderUI({
      tagList(
        strong('NPL'),
        p(et %>% filter(COMMON_NAME == input$species | NAME == input$species) %>% dplyr::select(STATE))
      )
    })
    output$grank <- renderUI({
      tagList(
        strong('GRANK'),
        p(et %>% filter(COMMON_NAME == input$species | NAME == input$species) %>% dplyr::select(G_RANK))
      )
    })
    output$srank <- renderUI({
      tagList(
        strong('SRANK'),
        p(et %>% filter(COMMON_NAME == input$species | NAME == input$species) %>% dplyr::select(SRANK))
      )
    })
    output$tracking <- renderUI({
      tagList(
        strong('Tracking - '),
        span(style = 'font-size: 10pt',
             paste(unlist(
               et %>% filter(COMMON_NAME == input$species | NAME == input$species) %>% 
                 dplyr::select(EO_TRACK_STATUS_CD)), collapse = '/ '), ': ',
             paste(unlist(
               et %>% filter(COMMON_NAME == input$species | NAME == input$species) %>% 
                 dplyr::select(S_EO_TRACKING_COM)), collapse = '/ ')
        )
      )
    })
    output$num_records <- renderUI({
      tagList(
        div(style = 'vertical-align: top; display: inline-block;', span(style = 'vertical-align: top; display: inline-block; font-family: \"Open Sans\",\"Helvetica Neue\",Helvetica,Arial,sans-serif; font-size: 24px;', 
           'EO: ', nrow(eo_data$reps), ' POD: ', nrow(pod_active$pod)))
      )
    })
    
    output$id_source_table <- renderDataTable({
      datatable(
        # pod_tab_id(),
        pod_id$table,
        rownames = F,
        height = '600px',
        options = list(stateSave = TRUE, search = list(regex = TRUE))
      )
    })
    
    output$pod_map_table <- renderDataTable({
      pod_active$pod %>%
        filter(lng < input$map_bounds$east,
               lng > input$map_bounds$west,
               lat > input$map_bounds$south,
               lat < input$map_bounds$north) %>%
        dplyr::select(PO_ID, IdSource, ObsDate, NameUsed, Count, InHDMS, Directions, GeneralNotes, everything()) -> pod_active$pod_map_data
      datatable(
        pod_active$pod_map_data,
        options = list(pageLength = 5),
        rownames = F,
        extensions = c('Responsive'),
        height = '600px',
        selection = 'single'
      )
    })
  })
  
  # INITIALIZE DATA TABLES

  output$eo_map_table <- DT::renderDataTable({
    datatable(
      eo_data$dump,
      rownames = F,
      extensions = c('Responsive'),
      selection = 'single'
    )
  })

  output$eo_rank_table <- renderDataTable({
    datatable(
      eo@data %>% filter(NAME == input$species) %>% group_by(EORANK) %>% summarize(count = length(EORANK)),
      rownames = F,
      selection = 'none'
    )
  })

  output$full_pod <- renderDataTable({
    datatable(
      pod_active$pod,
      rownames = F,
      extensions = c('ColReorder', 'Buttons'),
      options = list(dom = 'Blfrtip', buttons = I('colvis'),
                     colReorder = list(realtime = FALSE))
    )
  })

  observeEvent(input$id_source_table_rows_selected,{
    pod_active$pod <- pod %>% filter(Species == input$species)
    pod_active$pod <-
      subset(pod_active$pod,
             pod_active$pod$IdSource %in% pod_id$table$IdSource[input$id_source_table_rows_selected])
    if(!(is.null(input$status_filter_inhdms))){
      if('NA' %in% input$status_filter_inhdms){
        pod_active$pod <-
          pod_active$pod %>% filter(is.na(InHDMS) | InHDMS %in% input$status_filter_inhdms)
      } else {
        pod_active$pod <-
          pod_active$pod %>%
          filter(InHDMS %in% input$status_filter_inhdms)
      }
    }
    update_marker(pod_active$pod)
  })
  
  # INITIALIZE MAP
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -111.5, lat = 34, zoom = 6) %>%
      addMeasure(position = 'topright', primaryLengthUnit = 'meters', activeColor = '#0000ff', completedColor = '#000000') %>%
      addSearchOSM(options = searchOptions(moveToLocation = FALSE, hideMarkerOnCollapse = TRUE)) %>%
      addEasyButton(
        easyButton(
          icon('map-marker'),
          title='Show Marker Legend',
          onClick=JS("function(){ Shiny.onInputChange(\"legend_click\",  Math.random()); }"))) %>%
      addEasyButton(
        easyButton(
          icon('eye'),
          title='Symbolize POD by Time',
          onClick=JS("function(){ Shiny.onInputChange(\"symbolize_click\",  Math.random()); }"))) %>%
      addEasyButton(
        easyButton(
          icon('circle-o'),
          title='Zoom to State',
          onClick=JS("function(){ Shiny.onInputChange(\"snapEO_click\",  Math.random()); }"))) %>%
      addEasyButton(
        easyButton(
          icon('crosshairs'),
          title='Snap to POD',
          onClick=JS("function(){ Shiny.onInputChange(\"snapPOD_click\",  Math.random()); }")))
    
  })
  
  
  # MAIN OBSERVER FOR SPECIES SELECTION!
  
  observeEvent(input$species,{
    symbolize$state = 'OFF'
    
    # filter data
    eo_active <- subset(eo, eo$NAME == input$species)
    eo_data$reps <- eo_active@data
    eo_data$dump <- eo_dump %>% filter(SNAME == input$species) %>% dplyr::select(EO_ID, SITENAME, DIRECTIONS, SURVEYDATE, FIRSTOBS, LASTOBS, NS_EORANK, INDATE_AZ, UPDATE_AZ, everything())
    pod_active$pod <- subset(pod, pod$Species == input$species)
    pod_id$table <- pod %>% 
      filter(Species == input$species) %>% 
      group_by(IdSource) %>% 
      summarize(Count=length(IdSource))

    # Clear table search
    updateSearch(id_source_proxy, keywords = list(global='', columns=''))
    
    # check SWAP model
    try(
      if(any(grepl(names(models), pattern = et$ELCODE_BCD[et$NAME == input$species]))){
        add_model(models[[which(grepl(names(models), pattern = et$ELCODE_BCD[et$NAME == input$species]))]])
      } else {
        remove_model()
      }
    )
    
    # check/update critical habitat
    crithab_check <- grepl(crithab$name, pattern=input$species)
    if(any(crithab_check) == TRUE & length(search$list$species) > 0){
      add_crithab()
    } else {
      remove_crithab()
    }
    
    try(
      if(nrow(search$list) > 0){
        if(grepl(et$ELCODE_BCD[et$NAME == input$species], pattern='^AB') & input$species %in% abba_sectors$SPECIES){
          add_abba(input$species)
        } else {
          remove_abba()
        }
      }
    )

    update_eo(eo_active)
    update_marker(pod_active$pod)
      
    output$eo_hist <- renderPlot(height = 550, {

      p1 <-
        ggplot(eo_active@data, aes(update_year, group='EO')) +
        geom_histogram(binwidth = 1, fill='blue', color='white', alpha=0.6, show.legend = T) +
        scale_x_continuous(limits = c(1975, 2020), breaks = seq(1975, 2020, 5)) +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title = element_text(size=14),
              axis.text.y = element_text(size=14)) +
        labs(title='EO')
      p2 <-
        ggplot(pod_active$pod, aes(as.numeric(update_year))) +
        geom_histogram(binwidth = 1, fill='red', color='white', alpha=0.6) +
        scale_x_continuous(limits = c(1975, 2020), labels = seq(1975, 2020, 5), breaks = seq(1975, 2020, 5)) +
        labs(title='POD', x='Update Year') +
        theme(axis.text = element_text(size=14), axis.title = element_text(size=14))

      grid.newpage()
      if(nrow(eo_active) > 0){
        grid.draw(gtable_rbind(ggplotGrob(p1), ggplotGrob(p2)))
      } else {
        grid.arrange(p2)
      }
    })
  })
  
  observeEvent(input$basemap, {
    showModal(modalDialog(
      title = "Choose Basemap",
      radioButtons('basemaps_select', label = 'CHOICES',
                   choices=basemaps,
                   selected = character(0)
      ),
      footer = NULL,
      easyClose = T
    ))
  })
  
  observeEvent(input$refresh_table, {
    clearTable(id_source_proxy)
    updateSearch(id_source_proxy, keywords = list(global='', columns=''))
    pod_id$table <- pod_active$pod %>%
      group_by(IdSource) %>%
      summarize(Count=length(IdSource))
  })
  
  observeEvent(input$pod_map_table_row_last_clicked, {
    x <- pod_active$pod[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected],]
    leafletProxy('map') %>%
      clearPopups() %>%
      addPopups(data = x, lng = x$lng, lat = x$lat,
                popup = ~eval(popup_marker)
      )
  })
  
  observeEvent(input$eo_map_table_row_last_clicked, {
    x <- eo_data$reps[input$eo_map_table_rows_selected, ]
    temp <- coordinates(eo[eo$EO_ID == x$EO_ID, ])
    leafletProxy('map') %>%
      clearPopups() %>%
      addPopups(
        data = x,
        lng = temp[1],
        lat = temp[2],
        options = popupOptions(autoPan = FALSE),
        popup = ~eval(popup_eo)
      )
  })
  
  observeEvent(input$basemaps_select, {
    if(input$basemaps_select == basemaps[5]){
      leafletProxy('map') %>% 
        clearTiles() %>% 
        addTiles(input$basemaps_select, options = tileOptions(maxZoom = 15))
    } else if (input$basemaps_select == basemaps[3]){
      leafletProxy('map') %>% 
        clearTiles() %>% 
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addTiles(input$basemaps_select)
    } else {
      leafletProxy('map') %>% 
        clearTiles() %>%
        addProviderTiles(input$basemaps_select)
    }
    removeModal()
  })
  
  observeEvent(input$legend_click,{
    showModal(modalDialog(
      title = 'Marker Legend',
      
      tagList(
        div(style = 'vertical-align: top;',
            span(style='display: inline-block; vertical-align: top', 
               h3('POD Data'),
               tags$table(
                 tags$tr(
                   tags$td(img(src='img/markers/marker-blue.png')),
                   tags$td(h5('Unevaluated data'))
                 ),
                 tags$tr(
                   tags$td(img(src='img/markers/yes.png')),
                   tags$td(h5('Yes, in Biotics'))
                 ),
                 tags$tr(
                   tags$td(img(src='img/markers/checked.png')),
                   tags$td(h5('Checked, NOT in Biotics'))
                 ),
                 tags$tr(
                   tags$td(img(src='img/markers/suspect.png')),
                   tags$td(h5('Questionable data (Suspect)'))
                 ),
                 tags$tr(
                   tags$td(img(src='img/markers/refugia.png')),
                   tags$td(h5('Refugium site'))
                 ),
                 tags$tr(
                   tags$td(img(src='img/markers/doesnt-meet-criteria.png')),
                   tags$td(h5('Doesn\'t meet tracking criteria (NMC)'))
                 )     
              )
            ),
          span(style='display: inline-block; vertical-align: top', 
             h3('Other Data Sources'),
             tags$table(
               tags$tr(
                 tags$td(img(src='img/markers/ebird-breeding.png')),
                 tags$td(h5('eBird Breeding Record'))
               ),
               tags$tr(
                 tags$td(img(src='img/markers/ebird-nonbreeding.png')),
                 tags$td(h5('eBird Non-Breeding Record'))
               ),
               tags$tr(
                 tags$td(img(src='img/markers/aziba.png')),
                 tags$td(h5('Audubon Important Bird Area Counts'))
               ),
               tags$tr(
                 tags$td(img(src='img/markers/fins.png')),
                 tags$td(h5('FINS - Fish Database'))
               ),
               tags$tr(
                 tags$td(img(src='img/markers/marker-green.png')),
                 tags$td(h5('Tortoise Database'))
               )
             )
          )
        )
      )
    ))
  })
  
  observeEvent(input$symbolize_click,{
    if(symbolize$state == 'OFF' & fix$state == F & nrow(pod_active$pod) > 0){
      symbolize$state <- 'ON'
      x <- pod_active$pod
      update_year <- as.numeric(str_sub(pod_active$pod$ObsDate, 1, 4))
      levels <- c(-Inf, 2, 5, 10, 15, 20, 30, Inf)
      labels <- c('0-2', '2-5', '5-10', '10-15', '15-20', '20-30', '30+')
      time_levels <- cut(2017-update_year, breaks = levels, labels=labels)
      time_levels <- addNA(time_levels)
      levels(time_levels)[8] <- 'no_date'
      
      cols<- c('#888888', '#000080','#0000d3','#ab64c7','#ffff00','#ffc400','#ff8200','#ff0000')
      cf <- colorFactor(rev(cols), time_levels)
      
      myicons <- iconList(
        '0-2' = decadal_00_02, 
        '2-5' = decadal_02_05, 
        '5-10' = decadal_05_10, 
        '10-15' = decadal_10_15,
        '15-20' = decadal_15_20,
        '20-30' = decadal_20_30,
        '30+' = decadal_over_30,
        'no_date' = decadal_no_date
      )
      
      leafletProxy('map') %>%
        clearGroup('POD') %>%
        clearGroup('FIX') %>%
        clearPopups() %>%
        addMarkers(data = x,
                   lng=x$lng,
                   lat=x$lat,
                   icon = myicons[time_levels],
                   layerId = x$PO_ID,
                   group = 'POD',
                   clusterOptions = markerClusterOptions(maxClusterRadius = 1),
                   popup = ~eval(popup_marker)) %>%
        addLegend("bottomright", pal = cf, values = time_levels,
          title = "POD LastObs",
          labFormat = labelFormat(suffix=' yrs'),
          opacity = 1,
          layerId = 7777)
      
    } else if(symbolize$state == 'ON' & fix$state == F){
      symbolize$state <- 'OFF'
      leafletProxy('map') %>% removeControl(layerId = 7777)
      update_marker(pod_active$pod)
      
    } else if(fix$state == T){
      showModal(modalDialog(
        title = 'Cannot alter symbology while editing a marker',
        p('Finish your edits and try again')
      ))
    }
    
  })
  
  observeEvent(input$snapEO_click,{
    leafletProxy('map') %>%
      setView(lng = -111.5, lat = 34, zoom = 6)
  })
  
  observeEvent(input$snapPOD_click,{
    leafletProxy('map') %>%
      fitBounds(lng1=pod_bb$bounds[1],
                lat1=pod_bb$bounds[2],
                lng2=pod_bb$bounds[3],
                lat2=pod_bb$bounds[4])
  })
  
  observeEvent(pod_active$pod,{
    pod_bb$bounds <- c(min(pod_active$pod$lng),
                       min(pod_active$pod$lat),
                       max(pod_active$pod$lng),
                       max(pod_active$pod$lat))
  })
  
  observeEvent(input$move_pod,{
    fix$state <- TRUE
    undo['lng'] <<- pod_active$pod$lng[pod_active$pod$PO_ID == input$map_marker_click$id]
    undo['lat'] <<- pod_active$pod$lat[pod_active$pod$PO_ID == input$map_marker_click$id]
  })
  
  observeEvent(input$move_pod_from_table,{
    showModal(modalDialog(
      'Please close the current map popup, actually click on the point you want to move, and follow the subsequent popup. At present, moving from a table click popup is not supported'
    ))
  })
  
  observeEvent(fix$state,{
    if(fix$state == TRUE){
      update_marker_fix(pod_active$pod)
    } else {
      leafletProxy('map') %>% clearPopups()
    }
  })
  
  observeEvent(input$move_coord,{
    print(nrow(coord$df))
    if(nrow(coord$df) == 0){
      showModal(
        modalDialog(
          title = 'Define a coordinate first!',
          p('To use this functionality, you must first make a coordinate'),
          p('Use the \'Coord\' button above the map')
        )
      )
    } else {
      showModal(
        modalDialog(
          title = 'Select Defined Coordinate',
          selectInput(inputId = 'select_defined_coord', label='Coordinate', choices = coord$df$name),
          easyClose = TRUE,
          footer = tagList(
            actionButton('move_coord_cancel', 'Cancel', icon = icon('ban'),
                         style = "color: #000; background-color: #fff;"),
            actionButton('move_coord_confirm', 'Confirm', icon = icon('thumbs-o-up'),
                         style = "color: #000; background-color: #fff;")
          )
        )
      )
    }
  })
  
  observeEvent(input$move_coord_cancel,{
    removeModal()
  })
  
  observeEvent(input$datum_switch,{
    showModal(modalDialog(
      title='Datum Shift',
      selectInput('datum_type_from_to',
                  label='Choose a conversion',
                  choices = c('Convert to NAD27', 'Convert to NAD83')),
      easyClose = T,
      footer = tagList(
        actionButton('datum_shift_confirm', 'Confirm', icon = icon('thumbs-o-up'),
                     style = "color: #000; background-color: #fff;")
      )
    ))
  })
  
  observeEvent(input$datum_shift_confirm, {
    if(input$datum_type_from_to == 'Convert to NAD27'){
      temp <- pod_active$pod$Shape[pod_active$pod$PO_ID == input$map_marker_click$id] %>% 
        st_transform(crs = '+init=epsg:26912') %>% 
        st_coordinates() %>% 
        st_point(dim = 'XY') %>% 
        st_sfc(crs = '+init=epsg:26712') %>% 
        st_transform(crs = '+init=epsg:4326')
      
      pod_active$pod$lng[pod_active$pod$PO_ID == input$map_marker_click$id] <- st_coordinates(temp)[1]
      pod_active$pod$lat[pod_active$pod$PO_ID == input$map_marker_click$id] <- st_coordinates(temp)[2]
      
    } else {
      temp <- pod_active$pod$Shape[pod_active$pod$PO_ID == input$map_marker_click$id] %>% 
        st_transform(crs = 26712) %>% 
        st_coordinates() %>% 
        st_point(dim = 'XY') %>% 
        st_sfc(crs = 26912) %>% 
        st_transform(crs = 4326)
      
      pod_active$pod$lng[pod_active$pod$PO_ID == input$map_marker_click$id] <- st_coordinates(temp)[1]
      pod_active$pod$lat[pod_active$pod$PO_ID == input$map_marker_click$id] <- st_coordinates(temp)[2]
    }
    
    update_marker_fix(pod_active$pod)
    removeModal()
  })
  
  observeEvent(input$move_coord_confirm,{
    new_coords <- as.vector(st_geometry(coord$df[coord$df$name == input$select_defined_coord, ])[[1]])
    pod_active$pod$lng[pod_active$pod$PO_ID == input$map_marker_click$id] <- new_coords[1]
    pod_active$pod$lat[pod_active$pod$PO_ID == input$map_marker_click$id] <- new_coords[2]
    update_marker_fix(pod_active$pod)
    removeModal()
  })
  
  observeEvent(input$cancel_click, {
    fix$state <- FALSE
    pod_active$pod$lng[pod_active$pod$PO_ID == input$map_marker_click$id] <- undo['lng']
    pod_active$pod$lat[pod_active$pod$PO_ID == input$map_marker_click$id] <- undo['lat']
    update_marker(pod_active$pod)
  })
  
  observeEvent(input$confirm_click,{
    showModal(modalDialog(
      size = 's',
      title = paste('Permanently move PO_ID ', input$map_marker_click$id, '?', sep=''),
      selectInput('user_select', 'User', choices = user_names$name),
      textAreaInput('mapping_notes', 'Mapping Notes (do NOT leave blank)', height = 225, resize = 'none',
                    value = pod_active$pod$MappingNotes[pod_active$pod$PO_ID == input$map_marker_click$id]),
      footer = tagList(
        actionButton('move_no', 'No', icon=icon('ban'),
                     style = "color: #000; background-color: #fff;"),
        actionButton('move_yes', 'Yes', icon=icon('thumbs-up'),
                     style = "color: #000; background-color: #fff;")
  
      )
    ))
    
  })
  
  observeEvent(input$move_no,{
    removeModal()
    leafletProxy('map') %>% clearPopups()
  })
  
  observeEvent(input$move_yes,{
    if(nchar(input$mapping_notes) == 0 | input$mapping_notes == ' ' | input$mapping_notes == '' | input$mapping_notes == 'NA'){
      showModal(modalDialog(
        title = 'Mapping Notes was left blank!',
        strong('Tell us why you moved this point, for example:'),
        p(code('Coords given in NAD27. Changed to NAD83')),
        p(code('Coords given not on Cienega Creek. Moved approx. 200m west to place on Creek')),
        p(code('Northing given should be 35xxxxx, not 53xxxxx. Fixed.')),
        p(code('Given a \'suspect\' tag due to this observer having mis-identified ranid frogs in another report, 
               and the fact that this occurrence is quite a ways off from the normal distribution. 
               Doesn\'t agree with Reptiles of AZ map either.')),
        p(code('Coord way off from other observations from the same collector/day. Moved to same location as others'))
      ))
    } else {
      pod_active$pod$lng[pod_active$pod$PO_ID == input$map_marker_click$id] <- input$map_marker_click$lng
      pod_active$pod$lat[pod_active$pod$PO_ID == input$map_marker_click$id] <- input$map_marker_click$lat
      pod_active$pod$MappingNotes[pod_active$pod$PO_ID == input$map_marker_click$id] <- input$mapping_notes
      pod_active$pod$last_edited_user[pod_active$pod$PO_ID == input$map_marker_click$id] <- input$user_select
      pod_active$pod$last_edited_date[pod_active$pod$PO_ID == input$map_marker_click$id] <- now()
      pod[pod$PO_ID == input$map_marker_click$id, ] <<- pod_active$pod[pod_active$pod$PO_ID == input$map_marker_click$id, ]
      write_csv(pod_active$pod[pod_active$pod$PO_ID == input$map_marker_click$id, ],'data/log/log.csv', append = TRUE)
      fix$state <- FALSE
      update_marker(pod_active$pod)
      removeModal()
    }
  })
  
  observeEvent(input$flag_pod,{
    showModal(modalDialog(
      size = 's',
      title = paste('Set values for PO_ID', input$map_marker_click$id),
      selectInput('user_select', 'User', choices = user_names$name, selected = pod_active$pod$last_edited_user[pod_active$pod$PO_ID == input$map_marker_click$id]),
      selectInput('inhdms_select', 'Value', choices=c('NA', 'Y', 'NMC', 'Checked', 'Refugia', 'Suspect'), selected = pod_active$pod$InHDMS[pod_active$pod$PO_ID == input$map_marker_click$id]),
      textInput('eo_id', label = 'EO_ID', value = pod_active$pod$EO_ID[pod_active$pod$PO_ID == input$map_marker_click$id]),
      textAreaInput('mapping_notes', 'Mapping Notes (optional)', height = 225, resize = 'none', 
                    value = pod_active$pod$MappingNotes[pod_active$pod$PO_ID == input$map_marker_click$id]),
      footer = tagList(
        actionButton('move_no', 'Cancel', icon=icon('ban'),
                     style = "color: #000; background-color: #fff;"),
        actionButton('flag_confirm', 'Set Flag', icon=icon('thumbs-up'),
                     style = "color: #000; background-color: #fff;")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$flag_pod_from_table,{
    showModal(modalDialog(
      size = 's',
      title = paste('Set values for PO_ID', pod_active$pod$PO_ID[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]]),
      selectInput('user_select', 'User', choices = user_names$name, selected = pod_active$pod$last_edited_user[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]]),
      selectInput('inhdms_select', 'Value', choices=c('NA', 'Y', 'NMC', 'Checked', 'Refugia', 'Suspect'), selected = pod_active$pod$InHDMS[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]]),
      textInput('eo_id', label = 'EO_ID', value = pod_active$pod$EO_ID[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]]),
      textAreaInput('mapping_notes', 'Mapping Notes (optional)', height = 225, resize = 'none', 
                    value = pod_active$pod$MappingNotes[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]]),
      footer = tagList(
        actionButton('move_no', 'Cancel', icon=icon('ban'),
                     style = "color: #000; background-color: #fff;"),
        actionButton('flag_confirm_from_table', 'Set Flag', icon=icon('thumbs-up'),
                     style = "color: #000; background-color: #fff;")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$flag_confirm,{
    if(input$inhdms_select == 'NA'){
      pod_active$pod$InHDMS[pod_active$pod$PO_ID == input$map_marker_click$id] <- NA
      pod_active$pod$EO_ID[pod_active$pod$PO_ID == input$map_marker_click$id] <- NA
      pod_active$pod$MappingNotes[pod_active$pod$PO_ID == input$map_marker_click$id] <- input$mapping_notes
      pod_active$pod$last_edited_user[pod_active$pod$PO_ID == input$map_marker_click$id] <- input$user_select
      pod_active$pod$last_edited_date[pod_active$pod$PO_ID == input$map_marker_click$id] <- now()
      pod[pod$PO_ID == input$map_marker_click$id, ] <<- pod_active$pod[pod_active$pod$PO_ID == input$map_marker_click$id, ]
    } else {
      pod_active$pod$InHDMS[pod_active$pod$PO_ID == input$map_marker_click$id] <- input$inhdms_select
      pod_active$pod$EO_ID[pod_active$pod$PO_ID == input$map_marker_click$id] <- input$eo_id
      pod_active$pod$MappingNotes[pod_active$pod$PO_ID == input$map_marker_click$id] <- input$mapping_notes
      pod_active$pod$last_edited_user[pod_active$pod$PO_ID == input$map_marker_click$id] <- input$user_select
      pod_active$pod$last_edited_date[pod_active$pod$PO_ID == input$map_marker_click$id] <- now()
      pod[pod$PO_ID == input$map_marker_click$id, ] <<- pod_active$pod[pod_active$pod$PO_ID == input$map_marker_click$id, ]
    }
    write_csv(pod_active$pod[pod_active$pod$PO_ID == input$map_marker_click$id, ],'data/log/log.csv', append = TRUE)
    update_marker(pod_active$pod)
    removeModal()
    leafletProxy('map') %>% clearPopups()
  })
  
  observeEvent(input$flag_confirm_from_table,{
    if(input$inhdms_select == 'NA'){
      pod_active$pod$InHDMS[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]] <- NA
      pod_active$pod$EO_ID[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]] <- NA
      pod_active$pod$MappingNotes[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]] <- input$mapping_notes
      pod_active$pod$last_edited_user[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]] <- input$user_select
      pod_active$pod$last_edited_date[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]] <- now()
      pod[pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected], ] <<- pod_active$pod[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected], ]
    } else {
      pod_active$pod$InHDMS[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]] <- input$inhdms_select
      pod_active$pod$EO_ID[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]] <- input$eo_id
      pod_active$pod$MappingNotes[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]] <- input$mapping_notes
      pod_active$pod$last_edited_user[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]] <- input$user_select
      pod_active$pod$last_edited_date[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected]] <- now()
      pod[pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected], ] <<- pod_active$pod[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected], ]
    }
    write_csv(pod_active$pod[pod_active$pod$PO_ID == pod_active$pod_map_data$PO_ID[input$pod_map_table_rows_selected], ],'data/log/log.csv', append = TRUE)
    update_marker(pod_active$pod)
    removeModal()
    leafletProxy('map') %>% clearPopups()
  })
  
  observeEvent(input$batch,{
    showModal(modalDialog(
      size ='m',
      title = strong(h3('Batch Edit Visible Points')),
      h4(strong('This will modify ALL VISIBLE POINTS - BE CAREFUL!')),
      #checkboxInput('visible_only', 'Edit only visible'),
      selectInput('idsource_select', 'Select all or specific IdSource', choices = c('ALL POINTS ON THE MAP', unique(pod_active$pod_map_data$IdSource)), width = 500),
      div(style = 'vertical-align: top;',
          span(style='display: inline-block; vertical-align: top', 
            selectInput('inhdms_select', 'InHDMS Value', choices=c('NA', 'Y', 'NMC', 'Checked', 'Refugia', 'Suspect'), selected = NULL, width = 110)),
          span(style='display: inline-block; vertical-align: top',
               textInput('eo_id_batch', label = 'EO_ID (only if single EO)', width = 150)),
          span(style='display: inline-block; vertical-align: top',
               selectInput('user_select', 'User', choices = user_names$name, width = 235))
          ),
           
      textAreaInput('mapping_notes_batch', 'Mapping Notes (this will ADD to any existing mapping notes)', height = 80, width = 530, resize = 'none', value = 'NA'),
      selectInput('batch_coord_move', label='MOVE ALL POINTS TO COORDINATE?', choices = c('Do Not Move', levels(coord$df$name))),
      footer = tagList(
        actionButton('move_no', 'Cancel', icon=icon('ban'),
                     style = "color: #000; background-color: #fff;"),
        actionButton('batch_confirm_gut_check', 'Confirm', icon=icon('thumbs-up'),  # ask users to REALLY confirm their choice
                     style = "color: #000; background-color: #fff;")
      )
    ))
  })
  
  observeEvent(input$batch_confirm_gut_check, {
    if(input$idsource_select != 'ALL POINTS ON THE MAP'){
      num_rows_affected <- nrow(pod_active$pod[pod_active$pod$IdSource == input$idsource_select & pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID, ])
    } else {
      num_rows_affected <- nrow(pod_active$pod[pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID, ])
    }
    
    showModal(modalDialog(
      title = 'ARE YOU REALLY SURE??',
      p('You are going to change ', num_rows_affected, ' points'),
      p(strong('You will NOT be able to undo')),
      p(em('If you make a mistake, let Matt know ASAP so he can undo')),
      footer = tagList(
        actionButton('batch_confirm_gut_check_no', 'Cancel', icon=icon('ban'),
                     style = "color: #000; background-color: #fff;"),
        actionButton('batch_confirm', 'Confirm', icon=icon('thumbs-up'),  # ask users to REALLY confirm their choice
                     style = "color: #000; background-color: #fff;")
      )
    ))
  })
  
  observeEvent(input$batch_confirm_gut_check_no, {
    removeModal()
  })
  
  observeEvent(input$batch_confirm,{
    print(input$idsource_select)
    if(input$idsource_select != 'ALL POINTS ON THE MAP'){
      if(input$inhdms_select == 'NA'){
        pod_active$pod$InHDMS[pod_active$pod$IdSource == input$idsource_select & pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- NA
      } else {
        pod_active$pod$InHDMS[pod_active$pod$IdSource == input$idsource_select & pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- input$inhdms_select
      }
      if(!is.null(input$eo_id_batch)){
        pod_active$pod$EO_ID[pod_active$pod$IdSource == input$idsource_select & pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- input$eo_id_batch
      }
      if(!is.null(input$mapping_notes_batch) & input$mapping_notes_batch != 'NA' & input$mapping_notes_batch != ''){
        temp <- pod_active$pod$MappingNotes[pod_active$pod$IdSource == input$idsource_select & pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID]
        temp[!is.na(temp) & temp != ''] <- unlist(map(temp[!is.na(temp)], paste, input$mapping_notes_batch, sep = '; '))
        temp[is.na(temp) | temp == ''] <- input$mapping_notes_batch
        pod_active$pod$MappingNotes[pod_active$pod$IdSource == input$idsource_select & pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- temp
      }
      pod_active$pod$last_edited_user[pod_active$pod$IdSource == input$idsource_select & pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- input$user_select
      pod_active$pod$last_edited_date[pod_active$pod$IdSource == input$idsource_select & pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- now()
      if(input$batch_coord_move != 'Do Not Move'){
        new_coords <- as.vector(st_geometry(coord$df[coord$df$name == input$batch_coord_move, ])[[1]])
        pod_active$pod$lng[pod_active$pod$IdSource == input$idsource_select & pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- 
          new_coords[1]
        pod_active$pod$lat[pod_active$pod$IdSource == input$idsource_select & pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- 
          new_coords[2]
        pod_active$pod$Shape[pod_active$pod$IdSource == input$idsource_select & pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <-
          st_geometry(coord$df[coord$df$name == input$batch_coord_move, ])
      }
      pod[pod$IdSource == input$idsource_select & pod$Species == input$species & pod$PO_ID %in% pod_active$pod_map_data$PO_ID, ] <<- 
        pod_active$pod[pod_active$pod$IdSource == input$idsource_select & pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID, ]
      
      write_csv(pod_active$pod[pod_active$pod$IdSource == input$idsource_select & pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID, ],
                'data/log/log.csv', append = TRUE)
      update_marker(pod_active$pod)
      removeModal()
    } else {
      if(input$inhdms_select == 'NA'){
        pod_active$pod$InHDMS[pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- NA
      } else {
        pod_active$pod$InHDMS[pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- input$inhdms_select
      }
      if(!is.null(input$eo_id_batch)){
        pod_active$pod$EO_ID[pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- input$eo_id_batch
      }
      if(!is.null(input$mapping_notes_batch) & input$mapping_notes_batch != 'NA' & input$mapping_notes_batch != ''){
        temp <- pod_active$pod$MappingNotes[pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID]
        temp[!is.na(temp) & temp != ''] <- unlist(map(temp[!is.na(temp)], paste, input$mapping_notes_batch, sep = '; '))
        temp[is.na(temp) | temp == ''] <- input$mapping_notes_batch
        pod_active$pod$MappingNotes[pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- temp
      }
      pod_active$pod$last_edited_user[pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- input$user_select
      pod_active$pod$last_edited_date[pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- now()
      if(input$batch_coord_move != 'Do Not Move'){
        new_coords <- as.vector(st_geometry(coord$df[coord$df$name == input$batch_coord_move, ])[[1]])
        pod_active$pod$lng[pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- 
          new_coords[1]
        pod_active$pod$lat[pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <- 
          new_coords[2]
        pod_active$pod$Shape[pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID] <-
          st_geometry(coord$df[coord$df$name == input$batch_coord_move, ])
      }
      pod[pod$Species == input$species & pod$PO_ID %in% pod_active$pod_map_data$PO_ID, ] <<- 
        pod_active$pod[pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID, ]
      
      write_csv(pod_active$pod[pod_active$pod$PO_ID %in% pod_active$pod_map_data$PO_ID, ], 'data/log/log.csv', append = TRUE)
      update_marker(pod_active$pod)
      removeModal()
      leafletProxy('map') %>% clearPopups()
    }
  })
  
  observeEvent(input$coordinate,{
    showModal(
      modalDialog(
        title='Input Coordinate',
        textInput('coord_name', label = 'Name', value = ''),
        selectInput('coord_type',
                    label='Choose coordinate type',
                    choices = coord_type$coord_type),
        em('use spaces for DMS or DDM'),
        p(em('if no seconds are given, put 0')),
        p(em('do NOT use negative sign for longitude')),
        checkboxInput('coord_center', label = 'Add to center of map'),
        span(style='display: inline-block', textInput('coord_x', label = 'Long/Easting', width = 225, placeholder = input$coord_x)),
        span(style='display: inline-block', textInput('coord_y', label = 'Lat/Northing', width = 225, placeholder = input$coord_y)),
        br(),
        # span(style='display: inline-block', textInput('coord_x', label = coord$x_label, width = 125)),
        # span(style='display: inline-block', textInput('coord_y', label = coord$y_label, width = 125)),
        easyClose = T,
        footer = tagList(
          # actionButton('coord_remove', 'Remove', icon = icon('ban'),
          #              style = "color: #000; background-color: #fff;"),
          actionButton('coord_button', 'Submit Coordinate', icon = icon('thumbs-o-up'),
                       style = "color: #000; background-color: #fff;")
        )
      )
    )
  })
  
  observeEvent(input$coord_button,{
    removeModal()
    print(nrow(input$coord_name))
    print(input$coord_name)
    print(is.na(input$coord_name))
    print(is.null(input$coord_name))
    if((input$coord_x == "" | input$coord_y == "") & input$coord_center == FALSE){
      print('User entered nothing')
      
    } else if(input$coord_center == TRUE){
      temp_x <- mean(c(input$map_bounds$west, input$map_bounds$east))
      temp_y <- mean(c(input$map_bounds$north, input$map_bounds$south))
      df <- st_sf(id = nrow(coord$df) + 1,
                  name = ifelse(input$coord_name != "", input$coord_name, paste0('Point ', now())),
                  coord_type = "WGS84",
                  x = as.character(temp_x),
                  y = as.character(temp_y),
                  geometry = st_sfc(st_point(cbind(temp_x, temp_y)),
                                    crs = 4326))
      
      coord$df <- rbind(coord$df, df)
      print(coord$df)
      
      leafletProxy('map') %>%
        clearGroup(group = 'Coord') %>%
        addMarkers(data = coord$df,
                   icon = coordIcon,
                   layerId = coord$df$id,  # can't just use `id`, have to be explicit about what data.frame `id` comes from, weird...
                   popup = ~paste0( 
                     '<h5><strong>', name, '</h5></strong></br>',
                     actionButton("coord_remove", "Remove", icon=icon('ban'),
                                  onclick = 'Shiny.onInputChange(\"coord_remove\",  Math.random())',
                                  style="color: #fff; background-color: #5bc0de; border-color: #5bc0de;")
                   ),
                   options = markerOptions(draggable = T),
                   group='Coord')
      
    } else if(input$coord_type == 'Decimal degrees'){
      df <- st_sf(id = nrow(coord$df) + 1,
                  name = ifelse(input$coord_name != "", input$coord_name, paste0('Point ', now())),
                  coord_type = input$coord_type,
                  x = input$coord_x,
                  y = input$coord_y,
                  geometry = st_sfc(st_point(cbind(-as.numeric(input$coord_x), as.numeric(input$coord_y))),
                         crs = 4326))
      
      
      coord$df <- rbind(coord$df, df)
      print(coord$df)
      
      leafletProxy('map') %>%
        clearGroup(group = 'Coord') %>%
        addMarkers(data = coord$df,
          icon = coordIcon,
          layerId = coord$df$id,  # can't just use `id`, have to be explicit about what data.frame `id` comes from, weird...
          popup = ~paste0( 
            '<h5><strong>', name, '</h5></strong></br>',
            actionButton("coord_remove", "Remove", icon=icon('ban'),
                         onclick = 'Shiny.onInputChange(\"coord_remove\",  Math.random())',
                         style="color: #fff; background-color: #5bc0de; border-color: #5bc0de;")
            ),
          options = markerOptions(draggable = T),
          group='Coord')
      
    } else if(input$coord_type == 'DMS'){
      temp_x <- str_split(input$coord_x, ' ', simplify = T)
      temp_y <- str_split(input$coord_y, ' ', simplify = T)
      temp_x <- -(as.numeric(temp_x[,1]) + as.numeric(temp_x[,2])/60 + as.numeric(temp_x[,3])/3600)
      temp_y <- (as.numeric(temp_y[,1]) + as.numeric(temp_y[,2])/60 + as.numeric(temp_y[,3])/3600)
      
      df <- st_sf(id = nrow(coord$df) + 1,
                  name = ifelse(input$coord_name != "", input$coord_name, paste0('Point ', now())),
                  coord_type = input$coord_type,
                  x = temp_x,
                  y = temp_y,
                  geometry = st_sfc(st_point(cbind(-as.numeric(temp_x), as.numeric(temp_y))),
                                    crs = 4326))
      
      
      coord$df <- rbind(coord$df, df)
      print(coord$df)
      
      leafletProxy('map') %>%
        clearGroup(group = 'Coord') %>%
        addMarkers(data = coord$df,
                   icon = coordIcon,
                   layerId = coord$df$id,  # can't just use `id`, have to be explicit about what data.frame `id` comes from, weird...
                   popup = ~paste0( 
                     '<h5><strong>', name, '</h5></strong></br>',
                     actionButton("coord_remove", "Remove", icon=icon('ban'),
                                  onclick = 'Shiny.onInputChange(\"coord_remove\",  Math.random())',
                                  style="color: #fff; background-color: #5bc0de; border-color: #5bc0de;")
                   ),
                   options = markerOptions(draggable = T),
                   group='Coord')
      
    } else if(input$coord_type == 'Degrees decimal minutes'){
      temp_x <- str_split(input$coord_x, ' ', simplify = T)
      temp_y <- str_split(input$coord_y, ' ', simplify = T)
      temp_x <- -(as.numeric(temp_x[,1]) + as.numeric(temp_x[,2])/60)
      temp_y <- (as.numeric(temp_y[,1]) + as.numeric(temp_y[,2])/60)
      
      df <- st_sf(id = nrow(coord$df) + 1,
                  name = ifelse(input$coord_name != "", input$coord_name, paste0('Point ', now())),
                  coord_type = input$coord_type,
                  x = temp_x,
                  y = temp_y,
                  geometry = st_sfc(st_point(cbind(-as.numeric(temp_x), as.numeric(temp_y))),
                                    crs = 4326))
      
      
      coord$df <- rbind(coord$df, df)
      print(coord$df)
      
      leafletProxy('map') %>%
        clearGroup(group = 'Coord') %>%
        addMarkers(data = coord$df,
                   icon = coordIcon,
                   layerId = coord$df$id,  # can't just use `id`, have to be explicit about what data.frame `id` comes from, weird...
                   popup = ~paste0( 
                     '<h5><strong>', name, '</h5></strong></br>',
                     actionButton("coord_remove", "Remove", icon=icon('ban'),
                                  onclick = 'Shiny.onInputChange(\"coord_remove\",  Math.random())',
                                  style="color: #fff; background-color: #5bc0de; border-color: #5bc0de;")
                   ),
                   options = markerOptions(draggable = T),
                   group='Coord')
      
    } else {
      temp <- SpatialPoints(coords = cbind(as.numeric(input$coord_x), as.numeric(input$coord_y)), 
                            proj4string = CRS(coord_type$proj4string[coord_type$coord_type == input$coord_type]))
      temp <- spTransform(temp, CRS('+init=epsg:4326'))
      temp <- coordinates(temp)
      
      df <- st_sf(id = nrow(coord$df) + 1,
                  name = ifelse(input$coord_name != "", input$coord_name, paste0('Point ', now())),
                  coord_type = input$coord_type,
                  x = input$coord_x,
                  y = input$coord_y,
                  geometry = st_sfc(st_point(cbind(as.numeric(temp[1]), as.numeric(temp[2]))),
                                    crs = 4326))
      
      
      coord$df <- rbind(coord$df, df)
      print(coord$df)
      
      leafletProxy('map') %>%
        clearGroup(group = 'Coord') %>%
        addMarkers(data = coord$df,
                   icon = coordIcon,
                   layerId = coord$df$id,  # can't just use `id`, have to be explicit about what data.frame `id` comes from, weird...
                   popup = ~paste0( 
                     '<h5><strong>', name, '</h5></strong></br>',
                     actionButton("coord_remove", "Remove", icon=icon('ban'),
                                  onclick = 'Shiny.onInputChange(\"coord_remove\",  Math.random())',
                                  style="color: #fff; background-color: #5bc0de; border-color: #5bc0de;")
                   ),
                   options = markerOptions(draggable = T),
                   group='Coord')
    }
  })
  
  observeEvent(input$coord_remove,{
    coord$df <- coord$df[-input$map_marker_click$id, ]
    if(nrow(coord$df) == 0){
      leafletProxy('map') %>%
        clearGroup('Coord')
    } else {
      coord$df$id <- seq(1, nrow(coord$df), 1)
      
      leafletProxy('map') %>%
        clearGroup(group = 'Coord') %>%
        addMarkers(data = coord$df,
                   icon = coordIcon,
                   layerId = coord$df$id,  # can't just use `id`, have to be explicit about what data.frame `id` comes from, weird...
                   popup = ~paste0( 
                     '<h5><strong>', name, '</h5></strong></br>',
                     actionButton("coord_remove", "Remove", icon=icon('ban'),
                                  onclick = 'Shiny.onInputChange(\"coord_remove\",  Math.random())',
                                  style="color: #fff; background-color: #5bc0de; border-color: #5bc0de;")
                   ),
                   options = markerOptions(draggable = T),
                   group='Coord')
    }
    # print(paste0('Marker click = ', input$map_marker_click))
    # print(coord$df[coord$df$id == input$map_marker_click$id, ])
  })
  
  observeEvent(input$map_marker_mouseout,{
    # Mouseout event on a user-created coordinate triggers the update of it's actual geometry
    if('group' %in% names(input$map_marker_mouseout)){
      if(input$map_marker_mouseout$group == 'Coord'){
        coord$df$geometry[coord$df$id == input$map_marker_mouseout$id] <-
          st_sfc(st_point(cbind(
            input$map_marker_mouseout$lng,
            input$map_marker_mouseout$lat)),
            crs = 4326
          )
      }
    }
  })
  
  observeEvent(input$search_params,{
    showModal(modalDialog(
      size = 'l',
      title = 'Select Search Parameters',
      p(style = 'color: #f00;',
        em('Common Name does NOT work with filters currently, use only for reference to scientific names')),
      radioButtons('name_choice', label='Species Selection', choices = c('Scientific Name', 'Common Name'), inline = FALSE),
      checkboxGroupInput('taxon_filter', label='Taxonomic group', choices = taxon_group_lookup$taxon_group, inline = TRUE),
      checkboxGroupInput('status_filter_sgcn', label='SGCN Status', choices = c('1A', '1B', '1C'), inline = TRUE),
      checkboxGroupInput('status_filter_esa', label='ESA Status', choices = sort(unique(et$ESA)), inline = TRUE),
      checkboxGroupInput('status_filter_npl', label='NPL Status', choices = sort(unique(et$STATE)), inline = TRUE),
      checkboxGroupInput('status_filter_blm', label='BLM Sensitive', choices = sort(unique(et$BLM))),
      checkboxGroupInput('status_filter_usfs', label='USFS Sensitive', choices = sort(unique(et$BLM))),
      checkboxGroupInput('status_filter_track', label='Tracking', choices = sort(unique(et$EO_TRACK_STATUS_CD)), inline = TRUE),
      footer = actionButton('search_confirm', 'Update Search Parameters', icon = icon('thumbs-o-up'),
                            style = "color: #000; background-color: #fff;"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$search_confirm,{
    removeModal()
    
    if(input$name_choice == 'Scientific Name'){
      search$list = 
        et %>% 
        mutate(species = NAME) %>%
        dplyr::select(species) %>%
        distinct(species) %>%
        arrange(species)
    } else{
      search$list = 
        et %>% 
        mutate(species = COMMON_NAME) %>%
        dplyr::select(species) %>%
        distinct(species) %>%
        arrange(species)
    }
    
    if(!(is.null(input$taxon_filter))){
      search$list <-  
        search$list %>%
        filter(species %in% et$NAME[grepl(et$ELCODE_BCD, 
                                          pattern = paste(taxon_group_lookup$pattern[
                                            taxon_group_lookup$taxon_group %in% input$taxon_filter], collapse='|'))])
    }
    
    if(!(is.null(input$status_filter_sgcn))){
      search$list <-
        search$list %>%
        filter(species %in% et$NAME[et$SGCN %in% input$status_filter_sgcn])
    }
    
    if(!(is.null(input$status_filter_esa))){
      search$list <-
        search$list %>%
        filter(species %in% et$NAME[et$ESA %in% input$status_filter_esa])
    }
    
    if(!(is.null(input$status_filter_npl))){
      search$list <-
        search$list %>%
        filter(species %in% et$NAME[et$STATE %in% input$status_filter_npl])
    }
    
    if(!(is.null(input$status_filter_blm))){
      search$list <-
        search$list %>%
        filter(species %in% et$NAME[et$BLM %in% input$status_filter_blm])
    }
    
    if(!(is.null(input$status_filter_usfs))){
      search$list <-
        search$list %>%
        filter(species %in% et$NAME[et$USFS %in% input$status_filter_usfs])
    }
    
    if(!(is.null(input$status_filter_track))){
      search$list <-
        search$list %>%
        filter(species %in% et$NAME[et$EO_TRACK_STATUS_CD %in% input$status_filter_track])
    }

    print(search$list)
  })
  
  observeEvent(input$filter_inhdms,{
    pod_active$pod <- pod %>% filter(Species == input$species)
    showModal(modalDialog(
      title = 'Filter InHDMS Status / AZIBA',
      checkboxGroupInput('status_filter_inhdms', label='In HDMS', 
                         choices = c('NA', 'Y', 'NMC', 'Checked', 'Refugia', 'Suspect'), inline = TRUE,
                         selected = c('NA', 'Checked')),
      checkboxInput('filter_aziba', label = 'Exclude AZIBA?', value = TRUE),
      sliderInput('month_filter', label = 'Month Range', min = 1, max = 12, value = c(1, 12)),
      footer = actionButton('inhdms_confirm', 'Confirm Search'),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$inhdms_confirm, {
    if(!(is.null(input$status_filter_inhdms))){
      if('NA' %in% input$status_filter_inhdms){
        pod_active$pod <-
          pod_active$pod %>% filter(is.na(InHDMS) | InHDMS %in% input$status_filter_inhdms)
      } else {
        pod_active$pod <-
          pod_active$pod %>%
          filter(InHDMS %in% input$status_filter_inhdms)
      }
      
      
    } 
    
    if(input$filter_aziba == TRUE){
      pod_active$pod <-
        pod_active$pod %>% filter(str_detect(IdSource, pattern = 'AZIBA') == F)
    }
    
    print(input$month_filter[1])
    print(input$month_filter[2])
    print(input$month_filter[1] != 1 | input$month_filter[2] != 12)
    
    if(input$month_filter[1] != 1 | input$month_filter[2] != 12){
      pod_active$pod <-
        pod_active$pod %>% filter(month(as.Date(ObsDate)) >= input$month_filter[1] & month(as.Date(ObsDate)) <= input$month_filter[2])
    }
    
    pod_id$table <- pod_active$pod %>%
      group_by(IdSource) %>%
      summarize(Count=length(IdSource))
    
    update_marker(pod_active$pod)
    removeModal()
  })
  
  observeEvent(input$abstract,{
    # Modified for demo code. I think this could be more elegant, but the try statment works for now
    filename <- list.files('www/abstracts', full.names = F)
    if(input$species %in% c('Empidonax traillii extimus', 'Strix occidentalis lucida')){
      try(
        runjs(paste0("window.open('abstracts/",
                     filename[grep(x = tolower(filename),
                                   pattern = tolower(abstract_lookup$code[abstract_lookup$name == input$species]))],
                     "')"))
      )
    } else {
      showModal(modalDialog(
        title = 'No abstract found'
      ))
    }
  })
  
  observeEvent(input$library,{
    showModal(modalDialog(
      size = 's',
      checkboxGroupInput(
        'library_selection', 'Add layer', 
        choices = c('BLM National Monument',
                    'Counties',
                    'TRS',
                    'Watersheds HUC8'
        )
      ),
      footer = actionButton('library_confirm', 'Confirm'),
      easyClose = T
    ))
  })
  
  observeEvent(input$library_confirm,{
    leafletProxy('map') %>% clearGroup('Library')
    
    if('BLM National Monument' %in% input$library_selection){
      leafletProxy('map') %>%
        addEsriFeatureLayer('http://phx-gis/arcgis/rest/services/wmhb_hdms/BLM_NationalMonument/MapServer/0', 
                            useServiceSymbology = TRUE,
                            group = 'Library',
                            labelProperty = 'NLCS_NAME')
    } else {
      leafletProxy('map') %>% clearGroup('Library')
    }
    
    if('Counties' %in% input$library_selection){
      leafletProxy('map') %>%
        addEsriFeatureLayer('http://phx-gis/arcgis/rest/services/wmhb_hdms/county/MapServer/0', 
                            useServiceSymbology = TRUE,
                            group = 'Library',
                            labelProperty = 'NAME')
    } else {
      leafletProxy('map') %>% clearGroup('Library')
    }
    
    if('TRS' %in% input$library_selection){
      leafletProxy('map') %>%
        addEsriFeatureLayer('http://phx-gis/arcgis/rest/services/wmhb_hdms/TRS/MapServer/0',
                            useServiceSymbology = TRUE,
                            group = 'Library',
                            labelProperty = 'label',
                            labelOptions = labelOptions(textsize = '12px'),
                            options = featureLayerOptions(minZoom = 9, maxZoom = 12)
        ) %>%
        
        addEsriFeatureLayer('http://phx-gis/arcgis/rest/services/wmhb_hdms/TRS/MapServer/1',
                            useServiceSymbology = TRUE,
                            group = 'Library',
                            labelProperty = 'Section',
                            labelOptions = labelOptions(textsize = '12px'),
                            options = featureLayerOptions(minZoom = 12, maxZoom = 16)
        ) %>%
        
        addEsriFeatureLayer('http://phx-gis/arcgis/rest/services/wmhb_hdms/TRS/MapServer/2',
                            useServiceSymbology = TRUE,
                            group = 'Library',
                            labelProperty = 'qqsection',
                            labelOptions = labelOptions(textsize = '12px'),
                            options = featureLayerOptions(minZoom = 15)
        )
      
    } else {
      leafletProxy('map') %>% clearGroup('Library')
    }
    
    if('Watersheds HUC8' %in% input$library_selection){
      leafletProxy('map') %>%
        addEsriFeatureLayer('http://phx-gis/arcgis/rest/services/wmhb_hdms/watersheds/MapServer/0', 
                            useServiceSymbology = TRUE,
                            group = 'Library',
                            labelProperty = 'SUBBASIN')
    } else {
      leafletProxy('map') %>% clearGroup('Library')
    }
    
    removeModal()
  })
  
  observeEvent(input$import_shape,{
    showModal(modalDialog(
      title = 'Select ZIPPED Shapefile to import',
      fileInput('file_select', 'File', accept = 'application/zip'),
      textInput('layer_name', 'Layer Name', placeholder = 'My Shapefile'),
      footer = tagList(
        actionButton('import_clear', 'Remove All Imported', icon = icon('ban'),
                     style = "color: #000; background-color: #fff;"),
        actionButton('import_confirm', 'Confirm', icon = icon('thumbs-o-up'),
                     style = "color: #000; background-color: #fff;")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$import_clear, {
    legend$group <- legend$group[!legend$group %in% legend$added]
    leafletProxy('map') %>%
      clearGroup(legend$added) %>%
      addLayersControl(overlayGroups = legend$group, options = layersControlOptions(collapsed = FALSE))
    
    legend$added <- c()
    removeModal()
  })
  
  observeEvent(input$import_confirm,{
    shinyjs::disable('import_confirm')
    shinyjs::disable('import_clear')
    legend$group <- c(legend$group, input$layer_name)
    legend$added <- c(legend$added, input$layer_name)
    leafletProxy('map') %>% clearGroup(input$layer_name)
    cacheDir <- paste0('data/cache/upload_', format(now(), '%Y%m%d_%H%M%S'))
    file <- unzip(input$file_select$datapath, exdir = cacheDir)
    shp <- readOGR(paste0(cacheDir,'/', list.files(cacheDir, pattern='.shp$')))
    # Force shapefile to be in Web Mercator
    shp <- spTransform(shp, CRS('+proj=longlat'))
    # Detect if Polygon, then plot on leaflet map appropriately
    if(str_detect(class(shp), 'Polygons')){
      leafletProxy('map') %>%
        addPolygons(data=shp, group=input$layer_name, weight = 15, 
                    fillColor = '#00ffff', color = '#00ffff', opacity = 0.4, fillOpacity = 0.2,
                    noClip = TRUE, popup = popupTable(shp)) %>%
        addLayersControl(overlayGroups = legend$group, options = layersControlOptions(collapsed = FALSE))
    }
    # Detect if Point, then plot on leaflet map appropriately
    if(str_detect(class(shp), 'Point')){
      leafletProxy('map') %>%
        addMarkers(data=shp, icon = decadal_no_date, group=input$layer_name, popup = popupTable(shp)) %>% 
        addLayersControl(overlayGroups = legend$group, options = layersControlOptions(collapsed = FALSE))
    }
    
    if(str_detect(class(shp), 'Line')){
      leafletProxy('map') %>%
        addPolylines(data=shp, group=input$layer_name, popup = popupTable(shp)) %>% 
        addLayersControl(overlayGroups = legend$group, options = layersControlOptions(collapsed = FALSE))
    }
    removeModal()
    shinyjs::enable('import_confirm')
    shinyjs::enable('import_clear')
  })
  
  observeEvent(input$export,{
      showModal(modalDialog(
        title = 'Export currently filtered POD to CSV or SHP',
        textInput('export_name', 'Enter Filename', value = paste('Export', format(now(), format='%Y%m%d_%H%M'))),
        downloadButton('export_csv', 'Export CSV'),
        downloadButton('export_shp', 'Export SHP'),
        easyClose = TRUE,
        footer = NULL
      ))
  })
  
  output$export_csv <- {downloadHandler(
    filename = function() { paste(input$export_name, '.csv', sep='') },
    content = function(file) {
      write_csv(pod_active$pod %>% dplyr::select(-lng, -lat, -update_year, -Shape), file)
    }
  )}
  
  output$export_shp <- {downloadHandler(
    filename = function() { paste(input$export_name, '.zip', sep='') },
    content = function(file) {
      shp <- SpatialPointsDataFrame(
        coords = cbind(pod_active$pod$lng, pod_active$pod$lat), 
        data = as.data.frame(pod_active$pod %>% dplyr::select(-lng, -lat, -update_year, -Shape)),
        proj4string = CRS('+proj=longlat')
      )
      shp <- spTransform(shp, CRS('+init=epsg:26912'))
      writeOGR(shp, dsn = tempdir(), layer=input$export_name, driver='ESRI Shapefile')
      fs <- list.files(tempdir(), pattern = input$export_name)
      setwd(tempdir())
      zip(zipfile=file, files=fs)
      setwd('~/Dashboard')
    },
    contentType = "application/zip"
  )}
  
}
