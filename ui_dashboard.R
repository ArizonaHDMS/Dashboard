ui_dashboard <- function(){
  fluidPage(theme = shinytheme('yeti'), shinyjs::useShinyjs(),
                 tags$head(tags$link(rel='shortcut icon', href='./www/favicon.ico')),
                 tags$style(type='text/css', 'div.info.legend.leaflet-control br {clear:both;}'),
                 navbarPage('HDMS Dashboard',
                            tabPanel('EO/POD Explorer',
                                     fluidRow(
                                       column(width=3, uiOutput('select')),
                                       column(width=9, offset = 0, 
                                              fluidRow(
                                                column(width=2, uiOutput('comname')),
                                                column(width=2, uiOutput('elcode')),
                                                column(width=1, uiOutput('status')),
                                                column(width=1, uiOutput('esa')),
                                                column(width=1, uiOutput('usfs')),
                                                column(width=1, uiOutput('blm')),
                                                column(width=1, uiOutput('sgcn')),
                                                column(width=1, uiOutput('npl')),
                                                column(width=1, uiOutput('grank')),
                                                column(width=1, uiOutput('srank'))
                                              ),
                                              fluidRow(
                                                column(width= 11, uiOutput('tracking'))
                                              )
                                       )
                                     ),
                                     fluidRow(
                                       column(width = 5,
                                              actionButton('refresh_table', label='Clear Selected', icon = icon('refresh'),
                                                           style="color: #000; background-color: #fff;"),
                                              actionButton('search_params', label='Modify Search', icon = icon('search'),
                                                           style="color: #000; background-color: #fff;"),
                                              actionButton('abstract', 'View Abstract', icon=icon('info-circle'),
                                                           style="color: #000; background-color: #fff;")),
                                              
                                       column(width = 7,
                                              actionButton('basemap', 'Basemaps', icon=icon('globe', 'fa-lg'),
                                                           style="color: #000; background-color: #fff;"),
                                              actionButton('library', 'Map Lib', icon=icon('book'),
                                                           style="color: #000; background-color: #fff;"),
                                              actionButton('coordinate', 'Coord', icon=icon('map-marker', 'fa-lg'),
                                                           style="color: #000; background-color: #fff;"),
                                              actionButton('import_shape', label='Shapefile', icon = icon('download'),
                                                           style="color: #000; background-color: #fff;"),
                                              actionButton('export', label='Export', icon = icon('upload'),
                                                           style="color: #000; background-color: #fff;"),
                                              uiOutput('num_records', inline = TRUE)
                                              
                                       )
                                     ),
                                     fluidRow(
                                       column(width=5, 
                                              tabsetPanel(
                                                tabPanel('Id Source',
                                                         verticalLayout(
                                                           dataTableOutput('id_source_table'),
                                                           span(
                                                             # actionButton('filter_inhdms', 'Filter In HDMS'),
                                                             # actionButton('batch', 'Batch')
                                                           )
                                                         )
                                                ),
                                                tabPanel('POD Data',
                                                         verticalLayout(
                                                           span(
                                                             actionButton('filter_inhdms', label = 'Filter', icon=icon('filter'),
                                                                          style="color: #fff; background-color: #000;"),
                                                             actionButton('batch', label = 'Batch', icon=icon('tachometer'),
                                                                          style="color: #fff; background-color: #000;")
                                                           ),
                                                           dataTableOutput('pod_map_table')
                                                         )
                                                ),
                                                tabPanel('EO Data', DT::dataTableOutput('eo_map_table')),
                                                tabPanel('ObsDate Histogram',
                                                         verticalLayout(
                                                           plotOutput('eo_hist'),
                                                           plotOutput('pod_hist')
                                                         )
                                                ),
                                                tabPanel('EO Rank', dataTableOutput('eo_rank_table'))
                                              )
                                       ),
                                       column(width=7, leafletOutput('map', height = 600))
                                     )
                            ),
                            tabPanel('Spreadsheet View',
                                     fluidRow(
                                       column(width=12, DT::dataTableOutput('full_pod')),
                                       htmlOutput('pdfviewer')
                                     )
                            ),
                            tabPanel('Biotics Style Guide',
                                     sidebarLayout(
                                       sidebarPanel(fixedPanel(width=3, 
                                                               p(a(href='#Identifiers', 'Identifiers')), 
                                                               p(a(href='#Site/Directions', 'Site/Directions')),
                                                               p(a(href='#Locators', 'Locators')),
                                                               p(a(href='#Mapping', 'Mapping')),
                                                               p(a(href='#Representation_Extent', 'Representation_Extent')),
                                                               p(a(href='#Source_Features', 'Source_Features')),
                                                               p(a(href='#Survey_Information', 'Survey_Information')),
                                                               p(a(href='#EO_Rank', 'EO_Rank')),
                                                               p(a(href='#Description', 'Description')),
                                                               p(a(href='#Protection', 'Protection')),
                                                               p(a(href='#MA/Ownership', 'MA/Ownership')),
                                                               p(a(href='#Additional_Topics', 'Additional_Topics')),
                                                               p(a(href='#Documentation', 'Documentation')),
                                                               p(a(href='#Version/QC', 'Version/QC')),
                                                               p(a(href='#Other_Spatial_Attributes', 'Other_Spatial_Attributes')),
                                                               p(a(href='#Location', 'Location')),
                                                               p(a(href='#EO_EXT_AZ_1', 'EO_EXT_AZ_1')),
                                                               p(a(href='#Misc_EO', 'Misc_EO_Indicators')),
                                                               p(a(href='#Population_Size_Estimates', 'Population_Size_Estimates')),
                                                               p(a(href='#Record_Maintenance', 'Record_Maintenance'))
                                       )),
                                       mainPanel(
                                         h3(a(name='Identifiers', 'Identifiers')),
                                         p(strong('Identification Confirmed:'),'If the data is from a collection or ID by a known repuatable researcher
                                           (such as found in collection records) or from a knowledgeable observer, then select \'Y\'; if an existing
                                           record has come under question by experts about the identification or if it has been determined by HDMS Staff
                                           or experts that an uncertain identification of a species still has enough conservation value to be created as a
                                           new EO, then mark this as \'?\' If the identification has not been confirmed, then keep blank.'),
                                         p(strong('Data Sensitive EO:'),'If this EO is determined to be data sensitive, check the box to the right of it.'),
                                         h3(a(name='Site/Directions', 'Site/Directions')),
                                         p(strong('Survey Site:'), 'This is the site locality that is geographically tied to the site on an EO,
                                           this information can be distributed to the public.'),
                                         p('Click the green \"Add\" button to add a survey site. In the \"Details for Selected Row\" text field, refer back to
                                           the map viewer to determine an appropriate survey site. Start with the broadest landmark and end more specific, separated
                                           by a colon (:)'),
                                         tags$ul(tags$li('Must be ', em('Natural Features')),
                                                 tags$li('Large area can be a mountain range, valley, river, etc...'),
                                                 tags$li('Small area can be lake, mountain, wash, etc... CANNOT be a spring or tank (too specific)')
                                         ),
                                         p(code('ex. Huachuca Mountains: Gardner Canyon')),
                                         p(strong('Directions:'), 'Begin the directions by repeating the survey site information from above, insert colon (:),
                                           followed by the information provided by the original source (directions, locality, UTMs, maps, lat/longs, TRS, elevation, etc).'),
                                         p('General rules to remember when populating the directions field:'),
                                         tags$ul(tags$li('If there is only a single date associated with the observation/collection of this element, then the date does not need
                                                         to be included in the directions field. If there are multiple dates, then you will enter the data chronologically by date,
                                                         writing the date, colon (:), then listing the information provided in the original source; end with a period (.).'),
                                                 tags$li('If there are multiple Source Features in the EO, list the directions by Source Feature then date(s) chronologically',
                                                         p(code('White Mountains: Mount Baldy: (1st Source Feature): 1961-08-05: Sheep Crossing Campground. 1978-05-28: West
                                                                Fork Little Colorado River, Sheeps Crossing Campground, TRS T07N R27E Sec 32, elevation 9200 ft. (2nd SF):
                                                                1996-06-06: Mount Baldy, West Fork Little Colroado River: Sheep Crossing Trail, lat/long given 33.9468N -109.5299W,
                                                                TRS and elevation T06N R27E Sec 6 SE4NE4, 9360 ft.'))),
                                                 tags$li('If you updated a long-existing record with several dates and specific directions given, and when you edited it the
                                                         shape did not change at all, then you do not need to add the new UTMs that were given, it is ok to write \"Date: See EMF
                                                         for list of coordinates.\"'),
                                                 tags$li('If the observation is at a tank, but there are multiple years giving different coordinates for the tank (e.g. the
                                                         surveyor stood on one side of the tank one year and the other side the next year,), you do not need to change the record
                                                         spatially nor do you need to add the new coordinates given.'),
                                                 tags$li('.	If your record update included lumping an older, general location into the EO with a precise locations, write
                                                         \"Date: General location at _______.\"')
                                                 ),
                                         
                                         h3(a(name='Locators', 'Locators')),
                                         p('Add any additional sections or quarter section information to the \"TRS Notes\" by clicking on the row under the \"Township Range Section\"
                                           heading. If the EO is large and has multiple source features over a large area, then add \"Multiple TRS.\"'),
                                         h3(a(name='Mapping', 'Mapping')),
                                         p(strong('Representation Accuracy:'), 'Estimate the accuracy of how the EO was mapped (if multiple SFs of different accuracy, set the EO to the
                                           highest rep. acc. of the SFs), based on the information you were given.'),
                                         p(code('Very High, High, Medium, Low, Very Low')),
                                         p(strong('Representation Accuracy Comments:'), 'State what was used to map the EO.'),
                                         p(code('Directions, UTMs, and elevation given.')),
                                         h3(a(name='Representation_Extent', 'Representation Extent')),
                                         p(strong('Confidence Extent:'), 'Select from the dropdown menu to describe how confident you are that we know the full extent of the population
                                           of the species. Generally ', code('N - Confident full extent of EO is NOT known'),' unless situations such as a highly monitored reintroduction site
                                           (i.e. Black-footed Ferets), in which case the selection should be ', code('Y - Confident full extent of EO is known')),
                                         p('In the past, this was often marked as \"?\", in which case you can update it to be \"N\"'),
                                         h3(a(name='Source_Features', 'Source Features')),
                                         p('Rather than going through the map viewer as described above, the tabular information for each SF can be accessed by clicking on the SF ID# listed under this tab.
                                           If these are blank, then use this path to fill in the necessary information. The tab should read as follows:'),
                                         p(strong('Single Source Feature')),
                                         img(src='img/sf1.png', style='width: 90%; height: 90%'),
                                         p(strong('Multiple Source Features')),
                                         img(src='img/sf2.png', style='width: 90%; height: 90%'),
                                         h3(a(name='Survey_Information', 'Survey Information')),
                                         p(strong('Survey Date:'), 'The date there was last a formal survey at this EO, Date format yyyy-mm-dd;
                                           if only year-month given yyyy-mm-; if only year is given yyyy--'),
                                         p(strong('Last Observation Date:'), 'The most recent date the element was observed at this EO. See format above'),
                                         p(strong('First Observation Date:'), 'The oldest date the element was observed at this EO. See format above'),
                                         p(strong('EO Data:'), 'Starting with the date, colon (:), followed by data collected on the biology of the EO, (the number of individuals, sex, age,
                                           peculiar characteristics, etc) and what was done with the element (observed, collected, netted etc).'),
                                         p('For multiple dates, enter in chronological order.'),
                                         p('For multiple Source Features, add the SF identifier after the biological information between parentheses'),
                                         p(code('2001-10-02: 2 male adults caught and released alive.')),
                                         p(code('1989-02-04: 6 adults, 2 juveniles, 1 egg mass observed (1st SF). 2010: 1 adult
                                                male collected (2nd SF).')),
                                         h3(a(name='EO_Rank', 'EO Rank')),
                                         p('EO Rank indicates the relative value of the EO with respect to other occurrences of the Element. It is based on an assessment of estimated viability,
                                           i.e. the probability of persistence (based on condition, size, and landscape context) of occurrences of a given Element. In other words, EO ranks provide
                                           an assessment of the likelihood that if current conditions persist, the occurrence will likely endure for a defined period of time, typically 20-100 years.'),
                                         p(strong('EO Rank:'),'If a rank is given by the researcher, this can be populated with that viability estimate. If there is a long history of data or you
                                           can make logical judgement based on what is known about that species and what type of EO data is available, you can give a specific rank,
                                           such as A, AB, C, CD etc. Otherwise, follow the general guidelines:'),
                                         tags$ul(
                                           tags$li(code('E'),' for Verified Extant if single occurrence within past 15 years'),
                                           tags$li(code('U'),' for Unverifiable if last observation between 16-30 years'),
                                           tags$li(code('H'),' for Historical if last observation older than 30 years.')
                                         ),
                                         p(strong('Origin Subrank if known:'), 'Leave blank unless the population is introduced,
                                           possibly introduced, reintroduced, or possibly reintroduced.'),
                                         p(strong('EO Rank Date:'), 'For new records this will be the transcribed date, for updated records this will be the date that the rank was changed.
                                           This does not include the initials of the mapper/transcriber.'),
                                         p(strong('EO Rank Comments:'), 'Add if given by the surveyor, or qualifying the rank by HDMS personnel, or the reproductive mapping criteria for
                                           some species, especially raptors (check EOSpecs).'),
                                         h3(a(name='Description', 'Description')),
                                         p(strong('EO Type:'), 'Describe what type of element occurrence this is (Observation, Collection, Roost, Maternity Colony, etc.)
                                           Can be single or multiple types as more EO data are recorded. Format as follows:'),
                                         p(code('Observation')),
                                         p(code('Observation/Collection')),
                                         p(code('Netting/Bat Foraging Area/Anabat')),
                                         
                                         p(strong('General Description:'), 'This field should be used to describe the general landscape surrounding the EO, including soils, habitat types
                                           and vegetative species (male sure an additional species names given are capitalized properly). Any info added to this field should come directly from the source.'),
                                         p(strong('Minimum Elevation Feet'), 'Use available topos in Map Viewer to determine minimum the elevation an EO mapped at. This is NOT always the same
                                           as the elevation given by the original source!'),
                                         p(strong('Maximum Elevation Feet'),'Use for those source that survey along, i.e. a stretch of creek finding species along that stretch, or if you change the Source Feature
                                           of an EO from a Point to Line based on another nearby collection or observation.'),
                                         
                                         h3(a(name='Protection', 'Protection')),
                                         p('Currently, nothing is entered under this tab.'),
                                         h3(a(name='MA/Ownership', 'MA/Ownership')),
                                         p('Look in the "Other Spatial Attributes" section in order to find the information necessary to populate these fields. If you are updating an older record,
                                           you will have to go back to the EO details box in map viewer and recalculate spatial attributes in order for the most current information to display.'),
                                         img(src='img/maown1.png', style='width: 50%; height: 50%'),
                                         p(strong('Managed Areas:'),'Select the green \"Add\" button to bring up the managed area search box.'),
                                         img(src='img/maown2.png', style='width: 80%; height: 80%'),
                                         p('In the search box, type the search criteria, hit search, select the matching managed area by clicking on the row from the list at the bottom.'),
                                         img(src='img/maown3.png', style='width: 80%; height: 80%'),
                                         p('The managed areas that you should be adding to this table include the following fields listed in the \"Other Spatial Attributes\" tab:'),
                                         tags$ul(
                                           tags$li('Owners'), tags$li('Managed Areas'), tags$li('Grazing Allotments'), tags$li('Ranger Districts'), tags$li('Wilderness Areas')
                                         ),
                                         p('The populated Managed Areas table will look something like this:'),
                                         img(src='img/maown4.png', style='width: 80%; height: 80%'),
                                         p(strong('Management Comments:'),'Fill in any pertinent comments concerning the management of the site or species found there.'),
                                         p(strong('Ownership:'),'Select the green \"Add\" button to add an ownership row to the table.'),
                                         img(src='img/maown5.png', style='width: 80%; height: 80%'),
                                         p('Select the owner type from the dropdown menu, fill in the owner name and any owner notes (if applicable)'),
                                         img(src='img/maown6.png', style='width: 80%; height: 80%'),
                                         p('Examples of possible Managed Areas/Ownership combinations will read as follows:'),
                                         tags$ul(
                                           tags$li(strong('BLM Phoenix Field Office')),
                                           p('Managed Area (MA) = BLM Field Office'),
                                           p('Ownership: Owner Type = BLM; Owner Name = Phoenix Field Office'),
                                           tags$li(strong('State Trust Land')),
                                           p('MA = State Trust Land'),
                                           p('Ownership: Owner Type = State/provincial govt; Owner Name = State Land Department'),
                                           tags$li(strong('Parks and Recreation')),
                                           p('MA = ', em('<The name of the park as defined in the other spatial attributes>')),
                                           p('Ownership: Owner Type = L-Local Govt; Owner Name = Parks and Recreation; Owner Notes = ', em('<Name of park>'))
                                         ),
                                         h3(a(name='Additional_Topics', 'Additional Topics')),
                                         p(strong('General Comments:'),'General comments concerning the EO that have not been addressed in other fields in this record.
                                           Comments that can be entered here can be:'),
                                         tags$ul(
                                           tags$li('An explanation of any discrepancies in information, observations from personal experience relating to the validity of the
                                                   record, or notes such as whether the occurrence is a new record for a particular county, etc.'),
                                           tags$li('The number of source features tied to this EO (always entered first in the general comments)'),
                                           tags$li('Comments provided from the Source (the observer/collector) that do not fit anywhere else (i.e. temps and weather
                                                   conditions, photos taken, etc.)'),
                                           tags$li('Comments from the person processing the information, including initials and date of comments (this would be any HDMS personnel)'),
                                           tags$li('For bats: where multiple species have been observed or netted, include the 4 -letter species abbreviation and the EO_Num (.xxx),
                                                   and if applicable, the Bat Colony or Bat Foraging Area and corresponding EO_Num')
                                           ),
                                         p(code('12 Source Features in AGFD Biotics. 2011-05-24: Small pool with HYAR tads. 2012-03-22,24: Heard no calling, several recent egg clutches were observed.')),
                                         p(code('1994-10-12: Other species caught include AGCH and POOC. 1999-10-21: 4 dead chub with fungus (cause of death unknown).')),
                                         h3(a(name='Documentation', 'Documentation')),
                                         p(strong('Specimens:'),'Citation of old specimens collected or newer vouchers for the entity tied to the particular EO.'),
                                         p(em('Format')),
                                         p('Last name of collector, 1st initial.2nd initial. (record # or SN if none), additional collectors 1st Initial.2nd Initial. Last Name.
                                           Date Collected YYYY-MM-DD. Institution code with catalog number.'),
                                         p(code('Taylor, M.C. (459), J.K. Rayme. 1985-07-09. ASU 15648.')),
                                         p(code('Harrison, M.D. (D-4667), et al. 1982-05-05. NAVA 33897, DES 0001148.')),
                                         h3(a(name='Version/QC', 'Version/QC')),
                                         p('Includes information related to the version of the record and the Quality Control (QC) status of the EL.'),
                                         p(em('If this is a NEW record:')),
                                         p(strong('Transcription Date:'),'Select the date you are processing the data (today)'),
                                         p(strong('Transcribed By:'),'Enter your 3-letter initials in all CAPS'),
                                         p(em('If this is an update:')),
                                         p('Do not change the transcription date or the transcribed by initials. These are always entered the first date the EO is transcribed
                                           and will always stay the same.'),
                                         p(strong('Map/Data QC'),'The name of the person that evaluated the digitizing and/or tabular portion of the EO according to QC requirements.
                                           Filled in by the Data Manager.'),
                                         h3(a(name='Other_Spatial_Attributes', 'Other Spatial Attributes')),
                                         p('The table in this section is auto-populated during the digitization process. It is calculated based on the available reference data (e.g. counties, watersheds,
                                           ecoregions, AGFD Regions and Units, TRS, lat/long, Quad Name, Managed Areas, etc.).'),
                                         p('If this table is blank, try saving and re-opening the EO. If that doesn\'t work, close the EO, go back into the EO details box in map viewer and recalculate spatial attributes.
                                           When they are done recalculating, a black box noting its completion will appear in the top left corner of the map, it is then safe to open the EO and continue populating the fields.'),
                                         p(em('*Note: if you make changes to the EO before the spatial attributes are done calculating, you may not be able to save your work.')),
                                         h3(a(name='Location', 'Location')),
                                         p('Displays a map of the source features included in the EO.'),
                                         h3(a(name='EO_EXT_AZ_1', 'EO_EXT_AZ_1')),
                                         p(strong('Site Name:'),'Fill it in if the site name is given by the person/group providing the data.
                                           This may include names, numbers or a combination of both, and can include multiple site names
                                           (may have several entities working at the site)'),
                                         p(strong('Owner Comments:'),'Any comments important or that clarifies the Ownership. For example: \"Private In-holding within forest.\"'),
                                         p(strong('Original Source:'),'Put the acronym of organization who provided the original data for this location. If not know and the EO is on Federal Land, then put the acronym of that Federal entity'),
                                         p(strong('AGFD Region:'),'Found in the \"Other Spatial Attributes,\" and is to be populated when creating a new record only.'),
                                         p(strong('AGFD Unit:'),'Found in the "Other Spatial Attributes," and is to be populated when creating a new record only.'),
                                         p(strong('EO Rank (AZ):'),'Alphabetic rank that is generally tied to the reproductive ranks given for Birds and a few Reptiles. These ranks
                                           are tied back to the comments section under the EO Rank Tab (NatureServe Ranks). Check the appropriate EO Specs spreadsheet
                                           to see if the Element has an AZ EO Rank H:/WMHB/HDMS/EO_Specs'),
                                         p(strong('Source:'),'Source of the original data including dates of each record, listed in chronological order. For example: \"1961: E Lehto (SN), ARIZ Collection Record, in SEINet Data printout, 07/20/2016.\"
                                           See H:/WMHB/HDMS/HDMS Training Manual/EO Conventions/Sources_EX.doc for examples on how to write different sources.'),
                                         p(strong('Update:'),'The last date that the EO was changed or updated in biotics. Includes the most recent date (YYYY-MM-DD), a space, and the 3 initials in all CAPs of the person making the change.',
                                           em('Example: 2016-05-05 JRK')),
                                         p(strong('Indate:'),'The date and the initials of the person entering the New EO record for the first time. The formatting is the same as the Update. This will be entered the first time, and will not change over time.'),
                                         h3(a(name='Misc_EO', 'Misc EO Indicators')),
                                         p(strong('Multi-year population estimated available?:'),'Use the drop-down to select Yes or No.'),
                                         tags$ul(
                                           tags$li('No = only one year of EO data'),
                                           tags$li('Yes = multiple years of EO data')
                                         ),
                                         h3(a(name='Population_Size_Estimates','Population Size Estimates')),
                                         p('Add a row for every year of EO Data included in this EO'),
                                         tags$ul(
                                           tags$li('Click the green Add button to add a row.'),
                                           tags$li(strong('Year of Estimate:'),'Enter the year of the population estimate, do not include months or days.'),
                                           tags$li(strong('Mimimum population size:'), 'Enter the minimum amount of the Element documented in the EO Data, this
                                                   may be a sum of information from multiple dates.'),
                                           tags$li(strong('Maximum population size:'), 'If there is a number range (such as 100-500) observed, then put 100 in the \"Min\" and 500 in the \"Max\"'),
                                           tags$li(strong('Precision:'), 'Select, from the drop-down menu, whether the estimate is Qualitative (a general or approximate number, e.g., \"many\",
                                                   \"few\", \"some\", \"more than 1\", range, etc.) or Quantitative (exact count given).')
                                           ),
                                         h3(a(name='Record_Maintenance', 'Record Maintenance')),
                                         p(strong('Do Not Exchange:'), 'Check this box for all records completely contained within tribal lands. This will ensure when we do any data exchange with
                                           NatureServe that these records are not included. Only do this for records completely contained within tribal lands.')
                                         
                                         )
                                     )
                            ),
                            tabPanel('Useful References',
                                     h2(strong("Federal")), 
                                     p(a(href='https://www.fws.gov/southwest/es/arizona/', 'https://www.fws.gov/southwest/es/arizona/', target='_blank')),
                                     p(a(href='https://www.fws.gov/endangered/', 'https://www.fws.gov/endangered/', target='_blank')),
                                     p(a(href='https://www.fs.usda.gov/r3', 'https://www.fs.usda.gov/r3', target='_blank')),
                                     p(a(href='https://www.nps.gov/index.htm', 'https://www.nps.gov/index.htm', target='_blank')),
                                     p(a(href='https://www.blm.gov/arizona', 'https://www.blm.gov/arizona', target='_blank')),
                                     p(a(href='http://wetland_plants.usace.army.mil', 'http://wetland_plants.usace.army.mil', target='_blank')),
                                     
                                     br(),
                                     h2(strong("NatureServe")), 
                                     p(a(href='http://explorer.natureserve.org/', 'http://explorer.natureserve.org/', target='_blank')),
                                     p(a(href='http://www.natureserve.org/', 'http://www.natureserve.org/', target='_blank')),
                                     
                                     br(),
                                     h2(strong("General Sites")), 
                                     p(a(href='http://madrean.org/symbfauna/collections/index.php', 'http://madrean.org/symbfauna/collections/index.php', target='_blank'), 'Vertebrate collections including ASU, UA, etc...'),
                                     p(a(href='https://www.itis.gov/advanced_search.html', 'https://www.itis.gov/advanced_search.html', target='_blank')),
                                     p(a(href='http://nhc.asu.edu/', 'http://nhc.asu.edu/', target='_blank')),
                                     p(a(href='http://www.bison-m.org/speciesbooklet.aspx', 'http://www.bison-m.org/speciesbooklet.aspx', target='_blank')),
                                     p(a(href='http://www.gbif.org/', 'http://www.gbif.org/', target='_blank')),
                                     p(a(href='https://www.iucn.org/resources/conservation-tools/iucn-red-list-threatened-species', 'https://www.iucn.org/resources/conservation-tools/iucn-red-list-threatened-species', target='_blank')),
                                     p(a(href='https://www.invasivespeciesinfo.gov/index.shtml', 'https://www.invasivespeciesinfo.gov/index.shtml', target='_blank')),
                                     p(a(href='http://www.vertnet.org/index.html', 'http://www.vertnet.org/index.html', target='_blank')),
                                     p(a(href='http://www.mapress.com/j/zt/', 'http://www.mapress.com/j/zt/', target='_blank'), '(Zootaxa)'),
                                     p(a(href='https://www.idigbio.org/portal/search', 'https://www.idigbio.org/portal/search', target='_blank')),
                                     
                                     br(),
                                     h2(strong("Amphibians/Reptiles")), 
                                     p(a(href='https://ssarherps.org/', 'https://ssarherps.org/', target='_blank')),
                                     p(a(href='http://www.reptilesofaz.org/', 'http://www.reptilesofaz.org/', target='_blank')),
                                     p(a(href='https://tucsonherpsociety.org/', 'https://tucsonherpsociety.org/', target='_blank')),
                                     p(a(href='http://www.naherp.com/', 'http://www.naherp.com/', target='_blank')),
                                     p(a(href='http://www.cnah.org/', 'http://www.cnah.org/', target='_blank')),
                                     p(a(href='http://www.vertnet.org/index.html', 'http://www.vertnet.org/index.html', target='_blank')),
                                     p(a(href='http://vertebrates.si.edu/herps/', 'http://vertebrates.si.edu/herps/', target='_blank')),
                                     p(a(href='http://msb.unm.edu/divisions/amphibians-reptiles/index.html', 'http://msb.unm.edu/divisions/amphibians-reptiles/index.html', target='_blank')),
                                     p(a(href='http://mczbase.mcz.harvard.edu/', 'http://mczbase.mcz.harvard.edu/', target='_blank')),
                                     p(a(href='http://mvz.berkeley.edu/Herp_Collection.html', 'http://mvz.berkeley.edu/Herp_Collection.html', target='_blank')),
                                     p(a(href='http://lsa.umich.edu/ummz/herps.html', 'http://lsa.umich.edu/ummz/herps.html', target='_blank')),
                                     p(a(href='http://peabody.yale.edu/collections/vertebrate-zoology/herpetology', 'http://peabody.yale.edu/collections/vertebrate-zoology/herpetology', target='_blank')),
                                     
                                     br(),
                                     h2(strong("Birds")), 
                                     p(a(href='http://abc.azfo.org/', 'http://abc.azfo.org/', target='_blank')),
                                     p(a(href='http://avibase.bsc-eoc.org/avibase.jsp?lang=EN', 'http://avibase.bsc-eoc.org/avibase.jsp?lang=EN', target='_blank')),
                                     p(a(href='http://ebird.org/content/ebird/', 'http://ebird.org/content/ebird/', target='_blank')),
                                     p(a(href='http://lsa.umich.edu/ummz/birds.html', 'http://lsa.umich.edu/ummz/birds.html', target='_blank')),
                                     p(a(href='https://birdsna.org/Species-Account/bna/home', 'https://birdsna.org/Species-Account/bna/home', target='_blank')),
                                     p(a(href='http://www.americanornithology.org/', 'http://www.americanornithology.org/', target='_blank')),
                                     
                                     br(),
                                     h2(strong("Fish")), 
                                     p(a(href='http://nanfa.org/checklist.shtml', 'http://nanfa.org/checklist.shtml', target='_blank')),
                                     p(a(href='http://www.fishnet2.net/index.html', 'http://www.fishnet2.net/index.html', target='_blank')),
                                     p(a(href='http://vertebrates.si.edu/fishes/index.html', 'http://vertebrates.si.edu/fishes/index.html', target='_blank')),
                                     p(a(href='http://lsa.umich.edu/ummz/fishes.html', 'http://lsa.umich.edu/ummz/fishes.html', target='_blank')),
                                     p(a(href='http://mczbase.mcz.harvard.edu/', 'http://mczbase.mcz.harvard.edu/', target='_blank')),
                                     
                                     br(),
                                     h2(strong("Invertebrates")), 
                                     p(a(href='http://www.invertebase.org/portal/index.php', 'http://www.invertebase.org/portal/index.php', target='_blank')),
                                     p(a(href='http://www.amnh.org/our-research/invertebrate-zoology/', 'http://www.amnh.org/our-research/invertebrate-zoology/', target='_blank')),
                                     p(a(href='http://clade.acnatsci.org/malacology/', 'http://clade.acnatsci.org/malacology/', target='_blank')),
                                     p(a(href='http://bohart.ucdavis.edu/', 'http://bohart.ucdavis.edu/', target='_blank')),
                                     p(a(href='http://www.butterfliesandmoths.org/', 'http://www.butterfliesandmoths.org/', target='_blank')),
                                     p(a(href='http://www.mcz.harvard.edu/Departments/InvertZoo/researchcoll.html', 'http://www.mcz.harvard.edu/Departments/InvertZoo/researchcoll.html', target='_blank')),
                                     p(a(href='http://southwestdragonflies.net/damsels/swzygoptera.html#eiseni', 'http://southwestdragonflies.net/damsels/swzygoptera.html#eiseni', target='_blank')),
                                     p(a(href='http://entomology.si.edu/', 'http://entomology.si.edu/', target='_blank')),
                                     p(a(href='http://ip.nhm.org/nhmsearch/findlots.php', 'http://ip.nhm.org/nhmsearch/findlots.php', target='_blank')),
                                     p(a(href='http://bugguide.net/node/view/15740', 'http://bugguide.net/node/view/15740', target='_blank')),
                                     p(a(href='http://wwx.inhs.illinois.edu/collections/', 'http://wwx.inhs.illinois.edu/collections/', target='_blank')),
                                     p(a(href='http://butterfliesofamerica.com/intro.htm', 'http://butterfliesofamerica.com/intro.htm', target='_blank')),
                                     p(a(href='http://www.mcz.harvard.edu/Departments/Malacology/', 'http://www.mcz.harvard.edu/Departments/Malacology/', target='_blank')),
                                     p(a(href='http://www.odonatacentral.org/', 'http://www.odonatacentral.org/', target='_blank')),
                                     p(a(href='https://www.ars.usda.gov/pacific-west-area/logan-ut/pollinating-insect-biology-management-systematics-research/docs/us-national-pollinating-insects-collection/', 'https://www.ars.usda.gov/pacific-west-area/logan-ut/pollinating-insect-biology-management-systematics-research/docs/us-national-pollinating-insects-collection/', target='_blank')),
                                     p(a(href='http://www.sbcollections.org/iz/', 'http://www.sbcollections.org/iz/', target='_blank')),
                                     p(a(href='http://xerces.org/', 'http://xerces.org/', target='_blank')),
                                     p(a(href='http://lsa.umich.edu/ummz/mollusks.html', 'http://lsa.umich.edu/ummz/mollusks.html', target='_blank')),
                                     p(a(href='http://peabody.yale.edu/collections/entomology', 'http://peabody.yale.edu/collections/entomology', target='_blank')),
                                     
                                     br(),
                                     h2(strong("Mammals")), 
                                     p(a(href='http://www.mammalsociety.org/', 'http://www.mammalsociety.org/', target='_blank')),
                                     p(a(href='http://www.batcon.org/', 'http://www.batcon.org/', target='_blank')),
                                     p(a(href='http://vertebrates.si.edu/mammals/mammals_databases.html', 'http://vertebrates.si.edu/mammals/mammals_databases.html', target='_blank')),
                                     p(a(href='http://www.departments.bucknell.edu/biology/resources/msw3/browse.asp', 'http://www.departments.bucknell.edu/biology/resources/msw3/browse.asp', target='_blank'), 'We have hardcopies in our library'),
                                     p(a(href='http://msb.unm.edu/divisions/mammals/index.html', 'http://msb.unm.edu/divisions/mammals/index.html', target='_blank')),
                                     p(a(href='https://naturalhistory.si.edu/mna/about.cfm', 'https://naturalhistory.si.edu/mna/about.cfm', target='_blank')),
                                     p(a(href='http://mczbase.mcz.harvard.edu/', 'http://mczbase.mcz.harvard.edu/', target='_blank')),
                                     p(a(href='http://mvz.berkeley.edu/Mammal_Collection.html', 'http://mvz.berkeley.edu/Mammal_Collection.html', target='_blank')),
                                     
                                     br(),
                                     h2(strong("Plants")), 
                                     p(a(href='http://www.bonap.org/', 'http://www.bonap.org/', target='_blank')),
                                     p(a(href='http://nhc.asu.edu/vpherbarium/', 'http://nhc.asu.edu/vpherbarium/', target='_blank')),
                                     p(a(href='http://nmrareplants.unm.edu/', 'http://nmrareplants.unm.edu/', target='_blank')),
                                     p(a(href='http://swbiodiversity.org/seinet/collections/index.php', 'http://swbiodiversity.org/seinet/collections/index.php', target='_blank')),
                                     p(a(href='http://www.aznps.com/backissues.php', 'http://www.aznps.com/backissues.php', target='_blank')),
                                     p(a(href='http://www.calflora.org/species/index.html', 'http://www.calflora.org/species/index.html', target='_blank')),
                                     p(a(href='http://www.canotia.org/', 'http://www.canotia.org/', target='_blank')),
                                     p(a(href='https://www.dbg.org/', 'https://www.dbg.org/', target='_blank')),
                                     p(a(href='http://www.efloras.org/flora_page.aspx?flora_id=1', 'http://www.efloras.org/flora_page.aspx?flora_id=1', target='_blank')),
                                     p(a(href='http://www.e-journals.org/botany/', 'http://www.e-journals.org/botany/', target='_blank')),
                                     p(a(href='http://botany.si.edu/colls/collect/', 'http://botany.si.edu/colls/collect/', target='_blank')),
                                     p(a(href='http://www.plantsystematics.org/reveal/', 'http://www.plantsystematics.org/reveal/', target='_blank')),
                                     p(a(href='http://www.iapt-taxon.org/nomen/main.php', 'http://www.iapt-taxon.org/nomen/main.php', target='_blank')),
                                     p(a(href='http://www.missouribotanicalgarden.org/plant-science/plant-science/resources/research-links.aspx', 'http://www.missouribotanicalgarden.org/plant-science/plant-science/resources/research-links.aspx', target='_blank')),
                                     p(a(href='http://www.missouribotanicalgarden.org/', 'http://www.missouribotanicalgarden.org/', target='_blank')),
                                     p(a(href='http://sweetgum.nybg.org/science/vh/', 'http://sweetgum.nybg.org/science/vh/', target='_blank')),
                                     p(a(href='http://www.rsabg.org/research', 'http://www.rsabg.org/research', target='_blank')),
                                     p(a(href='http://rmh.uwyo.edu/data/search.php', 'http://rmh.uwyo.edu/data/search.php', target='_blank')),
                                     p(a(href='http://collections.nmnh.si.edu/search/botany/', 'http://collections.nmnh.si.edu/search/botany/', target='_blank')),
                                     p(a(href='http://tnrs.iplantcollaborative.org/TNRSapp.html', 'http://tnrs.iplantcollaborative.org/TNRSapp.html', target='_blank')),
                                     p(a(href='https://www.fieldmuseum.org/science/research/area/plants-fungi', 'https://www.fieldmuseum.org/science/research/area/plants-fungi', target='_blank')),
                                     p(a(href='http://www.tropicos.org/', 'http://www.tropicos.org/', target='_blank')),
                                     p(a(href='http://ucjeps.berkeley.edu/', 'http://ucjeps.berkeley.edu/', target='_blank')),
                                     p(a(href='https://cals.arizona.edu/herbarium/', 'https://cals.arizona.edu/herbarium/', target='_blank')),
                                     p(a(href='http://msb.unm.edu/divisions/herbarium/index.html', 'http://msb.unm.edu/divisions/herbarium/index.html', target='_blank')),
                                     p(a(href='http://www.utahrareplants.org/', 'http://www.utahrareplants.org/', target='_blank')),
                                     p(a(href='https://plants.usda.gov', 'https://plants.usda.gov', target='_blank')),
                                     p(a(href='https://cals.arizona.edu/herbarium/', 'https://cals.arizona.edu/herbarium/', target='_blank')),
                                     p(a(href='http://nau.edu/Merriam-Powell/Biodiversity-Center/Deaver-Herbarium/Research/', 'http://nau.edu/Merriam-Powell/Biodiversity-Center/Deaver-Herbarium/Research/', target='_blank')),
                                     p(a(href='http://goorchids.northamericanorchidcenter.org/', 'http://goorchids.northamericanorchidcenter.org/', target='_blank')),
                                     p(a(href='http://northamericanorchidcenter.org/', 'http://northamericanorchidcenter.org/', target='_blank')),
                                     p(a(href='http://www.calacademy.org/scientists/botany', 'http://www.calacademy.org/scientists/botany', target='_blank')),
                                     p(a(href='http://nmnh.typepad.com/the_plant_press/', 'http://nmnh.typepad.com/the_plant_press/', target='_blank')),
                                     p(a(href='http://nansh.org/portal/collections/misc/collprofiles.php?collid=105', 'http://nansh.org/portal/collections/misc/collprofiles.php?collid=105', target='_blank'))     
                            )
                 )
)}