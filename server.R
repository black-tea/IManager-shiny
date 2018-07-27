######################################
# Infrastructure Manager Server Code #
######################################

##### Setup
library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) 
library(sp)
library(rgeos)
library(sf)
library(mapview)
library(webshot)
library(htmlwidgets)
library(units)
library(xtable)
library(gmodels)
library(rgdal)
library(tidyr)
library(ggplot2)
library(tibble)
##### Packages from IManager #####
library(RPostgreSQL)
library(shiny)
library(shinyjs)
library("googlesheets")
library(R6)

### TODO
### 1. Switch Else / If to Case When on bottom w/ UI
### 2. Finish creating another R6 object, move R6 objects to separate module
### 3. fix google sheet funcionality - add row or import table

# Mandatory fields that must be filled before a user can add data
fieldsMandatory <- c("treatment_type", "int")

# Treatment Objects
infrastructure_vars <- c(
  'Speed Feedback Sign' = 'sfs',
  'Pedestrian-Activated Flashing Beacon' = 'fb',
  'Pedestrian Refuge Island' = 'rfg',
  'Signal Phasing' = 'sigPhase'
)
# perhaps automatically generate it based on class objects I've already created. See below:
# https://stackoverflow.com/questions/37303552/r-r6-get-full-class-name-from-r6generator-object

# Load BOE network
streets <- read_sf('data/streets/Streets.shp')
intersections <- read_sf('data/intersections/Intersections.shp')

# Google Sheets Workbook Key
countermeasureKey <- '1pss11b0ArdwxXT9jJ9HU6psQ3cRNgBfDVL0gNZcQAM8'
# focus on traffic signals, signal phasing, uncontrolled x-walks / beacons, bus enhancements

# Define infrastructure objects
sfs <- list(id = 'sfs',
            name_lbl = 'Speed Feedback Sign',
            data_flds = c('sfs_status','sfs_distance','sfs_streetside','sfs_facestraffic','sfs_serial','sfs_solar','sfs_activationdate','sfs_notes'),
            data_names = c('Status','Dist (ft.)','Street Side', 'Faces Traffic', 'Serial', 'Activation Date','Solar?', 'Notes'),
            tbl_flds = c('int_name','sfs_status','sfs_distance','sfs_streetside','sfs_facestraffic','sfs_serial','sfs_solar','sfs_activationdate','sfs_notes'),
            tbl_names = c('Intersection','Status','Dist (ft.)','Street Side', 'Faces Traffic', 'Serial', 'Activation Date', 'Solar?', 'Notes' )
            )
fb <- list(id = 'fb',
           name_lbl = 'Pedestrian-Activated Flashing Beacon',
           data_flds = c('fb_status','fb_roadside','fb_beaconstatus','fb_flashdur','fb_xwalk','fb_tcr','fb_curb','fb_notes'),
           data_names = c('Status','Roadside Only','Beacon Status','Flash Duration','X-Walk','TCR','Curb Ramps Both Approaches','Pole Notes'),
           tbl_flds = c('int_name','fb_status','fb_roadside','fb_beaconstatus','fb_flashdur','fb_xwalk','fb_tcr','fb_curb','fb_notes'),
           tbl_names = c('Intersection','Status','Roadside Only','Beacon Status','Flash Duration','X-Walk','TCR','Curb Ramps Both Approaches','Pole Notes')
           )
rfg <- list(id = 'rfg',
            name_lbl = 'Pedestrian Refuge Island',
            data_flds = c('rfg_status','rfg_hsip','rfg_designstart','rfg_designfinish','rfg_constructdate','rfg_url','rfg_notes'),
            data_names = c('Status','HSIP Survey','Design Start Date','Design Completion Date','Construction Completion Date','Design Plan URL','Notes'),
            tbl_flds = c('int_name','rfg_status','rfg_hsip','rfg_designstart','rfg_designfinish','rfg_constructdate','rfg_url','rfg_notes'),
            tbl_names = c('Intersection','Status','HSIP Survey','Design Start Date','Design Completion Date','Construction Completion Date','Design Plan URL','Notes')
            )

PointTreatment <- R6Class("PointTreatment", list(
  intName = list(
    choices = c('choice 1', 'choice 2', 'choice 3'),
    display = "Intersection Name",
    value = 232),
  intCLNodeID = NA_real_,
  streetName = NA_character_,
  X = NA_real_,
  Y = NA_real_))

sigPhase <- R6Class("sigPhase", inherit = PointTreatment, list(
  display = "Signal Phasing",
  nb = list(display = "N/B", choices = c(NA, "PORT", "PPLT", "POLT", "POLT & PORT"), value = NA_character_),
  sb = list(display = "S/B", choices = c(NA, "PORT", "PPLT", "POLT", "POLT & PORT"), value = NA_character_),
  eb = list(display = "E/B", choices = c(NA, "PORT", "PPLT", "POLT", "POLT & PORT"), value = NA_character_),
  wb = list(display = "W/B", choices = c(NA, "PORT", "PPLT", "POLT", "POLT & PORT"), value = NA_character_),
  tcr = list(display = "TCR", value = NA_character_),
  ewo = list(display = "EWO#", value = NA_character_),
  wo = list(display = "WO#", value = NA_character_),
  status = list(display = "Status", choices = c('In Design', 'Design Completed'), value = NA_character_),
  team = list(display = "Team", choices = c('Scott Brown', 'Adam Driscoll'), value = NA_character_),
  priority = list(display = "Priority", choices = c(1,2,3,4,5,6), value = NA_integer_),
  totalcost = list(display = "Est. Cost (Total)", value = NA_real_),
  designcost = list(display = "Est. Cost (Design)", value = NA_real_),
  matcost = list(display = "Est. Cost (Materials)", value = NA_real_),
  labcost = list(display = "Est. Cost (Labor)", value = NA_real_),
  delivery = list(display = "Construction Delivery Method", value = NA_character_),
  plandate = list(display = "Date Planned", value = NA_integer_),
  installdate = list(display = "Date Installed", value = NA_integer_),
  notes = list(display = "Notes", value = NA_character_)
))

##### End Prep Code from IManager #####

function(input, output, session) {
  
  ##### Functions #####
  ChoosePt <- function(startPt, seg, segFraction) {
    # Ensure interpolation along line returns correct point
    # using PostGIS function ST_LineLocatePoint for verification
    #
    # Args:
    #   startPt: A point value at one end of the segment, used to establish origin for distance
    #   seg: A polyline segment to interpolate the distance along
    #   segFraction: Fractional value of the segment where the point is located
    #
    # Returns:
    #   The sf point that matches the distance specified in DistToPt
    #
    Pt1 <- st_line_sample(x = seg, n = 1, sample = segFraction) %>% st_cast("POINT")
    Pt2 <- st_line_sample(x = seg, n = 1, sample = (1 - segFraction)) %>% st_cast("POINT")
    
  }
  
  DistToPt <- function(startPt, seg, distFt) {
    # Generate a point along a line
    #
    # Args:
    #   startPt: A point value at one end of the segment, used to establish origin for distance
    #   seg: A polyline segment to interpolate the distance along
    #   distFt: Distance value for interpolation along the polyline segment
    #
    # Returns:
    #   A sf point at the precise distance specified along the line
    #
    # Need some error handling if distance > length of line
    seg <- seg %>%
      st_cast("LINESTRING") %>%
      st_set_crs(4326) %>%
      st_transform(2229)
    segDistFt <- st_length(seg)
    segFraction <- distFt/segDistFt
    infrastructurePt <- st_line_sample(x = seg, n = 1, sample = segFraction) %>%
      st_cast("POINT") %>%
      st_transform(4326)
    return(infrastructurePt)
    # Error handling
    #if distFt > segDist
  }
  
  UpdateGS <- function(table, flds) {
    # Update linked google sheet
    #
    # Args:
    #   table: Name of the table in PostgreSQL db to copy to excel sheet
    #   flds: List of fields to query in the table
    #
    # Returns:
    #   Copies the table data to a google sheet
    # Register test google sheet that already exists
    test_s <- gs_key('1bgkKWcvrMJXP3XOUAi_8-Z4K7k3msUYdS-yCZd_qP50')
    # Query the db for updated table information
    result <- QueryTblSQL(table, flds)
    # Update google sheet with result
    test_s <- test_s %>%
      gs_edit_cells(ws = table, input = result)
  }
  
  ReadGS <- function(wbKey, wsName) {
    # Read Google Sheet
    #
    # Args:
    #   wbKey: workbook key (make sure wb is published to web)
    #   wsName: Name of the worksheet w/ data
    #
    # Returns:
    #   Copies the table data to a google sheet
    # Register test google sheet that already exists
    test_s <- gs_key(wbKey)
    # Query the db for updated table information
    result <- QueryTblSQL(table, flds)
    # Update google sheet with result
    test_s <- test_s %>%
      gs_edit_cells(ws = table, input = result)
  }
  
  CreateIcon <- function(color) {
    # Create icon for mapping, using the awesomeIcons library
    #
    # Args:
    #   color: desired color for the map marker
    #
    # Returns:
    #   Map marker with the 'circle-o' icon in the desired color
    custom_icon <- awesomeIcons(
      icon = 'circle-o',
      iconColor = '#ffffff',
      library = 'fa',
      # The markercolor is from a fixed set of color choices
      markerColor = color
    )
    return(custom_icon)
  }
  
  # Formatting for Mandatory Code
  labelMandatory <- function(label) {
    tagList(
      label,
      span("*", class = "mandatory_star")
    )
  }
  
  BuffGeom <- function(boundary, ft) {
    # Buffer boundary by a distance in ft, return to wgs84
    #
    # Args:
    #   boundary: sf shape for the buffer
    #   ft: distance to buffer, in ft
    #
    # Returns:
    #   sf shape object, buffered, in wgs84
    geomNAD83 <- st_transform(boundary, 2229) # Convert to NAD83
    geomNAD83 <- st_buffer(geomNAD83, ft) # Buffer
    geomWGS84 <- st_transform(geomNAD83, 4326) # Convert back to wgs84
    return(geomWGS84)
  }
  
  SelectSegment <- function(segID) {
    # Select a polyline based on the row ID and draw on map
    #
    # Args:
    #   segID: Row ID of the line
    #
    # Returns:
    #   Adds Selected Line to the Map
    if(!is.null(xstreetR())){
      # Filter polylines based on the click
      polylineSelected <- xstreetR() %>%
        filter(rownames(.) == segID )
      # Add selected line information to RV
      locationRV$Segment <- polylineSelected
      # Add selected line on top as another color
      proxy <- leafletProxy("infrastructureManagerMap") %>%
        # Add selected line shape to map
        addPolylines(
          data = polylineSelected,
          layerId = "selected",
          color = "#0066a1",
          opacity = 1
        )
      # Once user has selected the street segment, becomes NULL
      msgRV$msg <- c(' ')
    } 
  }
  
  # Clip to selected boundary
  geomClip <- function(segment) {
    st_intersection(segment,geographyR())
  }
  
  ##### Reactive Objects #####
  # RV for location objects
  locationRV <- reactiveValues(Intersection=list(), Segment=list())
  
  # RV storing UI message variable
  msgRV <- reactiveValues()
  
  # # RV storing data for each table
  # dbtblRV <- reactiveValues(sfs = QueryTblSQL('sfs', sfs$tbl_flds),
  #                            rfg = QueryTblSQL('rfg', rfg$tbl_flds),
  #                            fb = QueryTblSQL('fb', fb$tbl_fields)
  # )
  
  dbtblRV <- reactiveValues(sfs = NULL,
                            rfg = NULL,
                            fb = NULL
  )
  
  # Capture form input values
  inputDataR <- reactive({
    fields <- get(input$treatment_type)[['data_flds']]
    formData <- sapply(fields, function(x) input[[x]])
  })
  
  # Reactive expression to grab intersection data based on user selection
  intersectionR <- reactive({
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      intersectionR <- intersections %>%
        filter(TOOLTIP == input$int)
      return(intersectionR)
    } else {return(NULL)}
  })
  
  # Capture fields for display in tables
  tbl_fields <- reactive({
    fields <- get(input$treatment_type)[['tbl_flds']]
  })
  
  # Reactive expression to grab cross streets from selected intersection
  xstreetR <- reactive({
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      # Grab selected intersection information
      intersectionR <- intersectionR()
      # # Query for streets related to the intersection
      # xstreet_query <- paste0("SELECT *
      #                        FROM streets 
      #                        WHERE int_id_fro=", intersectionR$cl_node_id, " OR int_id_to=", intersectionR$cl_node_id)
      # xstreet <- QuerySQL(xstreet_query, type = 'spatial')
      xstreetR <- streets %>%
        filter(INT_ID_FRO == intersectionR$CL_NODE_ID | INT_ID_TO == intersectionR$CL_NODE_ID)
    } else {return(NULL)}
  })

  ##### Infrastructure Manger Map & Observers #####
  output$infrastructureManagerMap <- renderLeaflet({
    # Map object
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 10, maxZoom = 18)) %>%
      setView(lng = -118.329327,
              lat = 34.0546143,
              zoom = 11)
  })
  
  # Infrastructure Manager Map observer that updates based on the intersection
  observeEvent(input$int, {
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      
      # Get intersection reactive var, clear markers, clear RV
      intersectionR <- intersectionR()
      locationRV$Segment <- NULL
      proxy <- leafletProxy("infrastructureManagerMap") %>%
        clearMarkers() %>%
        clearShapes()
      
      # If there is one marker in the query, it is blue
      if(nrow(intersectionR) == 1 && length(intersectionR) > 0) {
        # Add intersection to RV object
        locationRV$Intersection <- intersectionR
        # Get cross streets
        xstreetR <- xstreetR()
        # Update message to choose a street
        msgRV$msg <- c('Select a Cross Street')
        # Add intersection marker to map
        proxy %>% addAwesomeMarkers(
          data = intersectionR,
          icon = CreateIcon('darkblue')
        )
        
        # If there is at least one related segment, add it
        if(length(xstreetR) > 0) {
          proxy %>% addPolylines(
            data = xstreetR,
            layerId = as.numeric(rownames(xstreetR)),
            color = "gray"
          )
        }
        # If there is >1 marker, gray initially
      } else if(nrow(intersectionR) > 1) {
        proxy %>% addAwesomeMarkers(
          data = intersectionR,
          icon = CreateIcon("gray")
        )
        msgRV$msg <- c('Select One Intersection Node')
        
      }
      # Update the map zoom bounds
      proxy %>% fitBounds(lng1 = as.double(st_bbox(intersectionR)[1]),
                          lat1 = as.double(st_bbox(intersectionR)[2]),
                          lng2 = as.double(st_bbox(intersectionR)[3]),
                          lat2 = as.double(st_bbox(intersectionR)[4]))
    }
  })
  
  # Infrastructure Manager Map Observer based on the polyline selection
  observeEvent(input$infrastructureManagerMap_shape_click, {
    # Add Selection to Map
    SelectSegment(input$infrastructureManagerMap_shape_click$id)
    
  })
  
  # Observe
  observe({
    # For SFS, add point
    if(!any(sapply(list(locationRV$Intersection, locationRV$Segment, input$sfs_distance), is.null)) && input$treatment_type == 'sfs'){
      infrastructurePt <- DistToPt(locationRV$Intersection, locationRV$Segment, input$sfs_distance)
      # Update Infrastructure Manager Map
      leafletProxy("infrastructureManagerMap") %>%
        removeMarker(layerId = "infrastructurePt") %>%
        addMarkers(data = infrastructurePt, layerId = "infrastructurePt")
    }
  })
  
  # Observer controlling submission of information to database
  observeEvent(input$submit, {
    
    # Create a progress notification
    progress <- shiny::Progress$new(style = 'notification')
    progress$set(message = "Submitting...", value = NULL)
    on.exit(progress$close())
    
    # Temporarily disable submit button
    shinyjs::disable('submit')
    
    # maybe add switch variable here, (for example, whether it has a segment or not)
    
    # Int AssetID, Int Cl_Node_ID, Int Name, Seg AssetID, Form Data, Int Geom 
    data <- c(int_assetid = locationRV$Intersection$assetid,
              int_clnodeid = locationRV$Intersection$cl_node_id,
              int_name = locationRV$Intersection$tooltip,
              seg_assetid = locationRV$Segment$assetid,
              inputDataR(),
              geom_4326 = st_as_text(locationRV$Intersection$geom))
    
    # Add to DB, update progress bar
    InsertSQL(input$treatment_type, data)
    
    # Update linked spreadsheet, update progress bar
    progress$set(detail = "Updating linked Google Sheets.")
    UpdateGS(input$treatment_type, tbl_fields())
    
    # Reset form & map objects, map view back to LA
    shinyjs::reset("form")
    locationRV$Segment <- NULL
    proxy <- leafletProxy("infrastructureManagerMap") %>%
      clearMarkers() %>%
      clearShapes() %>%
      setView(lng = -118.329327,
              lat = 34.0546143,
              zoom = 11)
    
    # Update DT
    dbtblRV[[input$treatment_type]] <- QueryTblSQL(input$treatment_type, tbl_fields())
    
  })
  
  ### Maps
  # Map Object for Project Delivery Tab
  output$projectmap <- renderLeaflet({
    
    mapR() %>%
      setView(lng = -118.329327,
              lat = 34.0546143,
              zoom = 12) 
    
  })
  
  # Map Object for Area Filter
  output$filterMap <- renderLeaflet({

    mapR()

  })
    
  ##### Non-Map Output #####
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(dbtblRV$sfs, file)
    }
  )

  output$treatment_type <- renderUI({
    
    # Selection Input
    selectInput("treatment_type",
                labelMandatory("Treatment"),
                c(infrastructure_vars))
  })
  
  # Intersection Selection
  output$intSelect <- renderUI({
    
    # Selection Input
    selectizeInput(inputId = "int",
                   labelMandatory('Intersection'),
                   # choices = intRV$intlist$tooltip,
                   choices = intersections$TOOLTIP,
                   selected = NULL,
                   multiple = FALSE)
  })
  
  # Planned Status Selection
  output$treatment_status <- renderUI({
    if(!is.null(input$treatment_type)){
      if(input$treatment_type == 'rfg'){
        selectInput(inputId = "rfg_status", label = "Status", choices = list('Planned', 'Completed'))
      } else if(input$treatment_type == 'sfs'){
        selectInput(inputId = "sfs_status", label = "Status", choices = list('Planned', 'Completed'))
      } else if(input$treatment_type == 'fb'){
        selectInput(inputId = "fb_status", label = "Status", choices = list('Planned', 'Completed'))
      }
    }
  })
  
  # Second UI Bin
  output$treatment_info1 <- renderUI({
    if(!is.null(input$treatment_type)){
      if(input$treatment_type == 'Leading Pedestrian Interval'){
        sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE)
      } else if(input$treatment_type == 'rfg'){
        tagList(
          selectInput("rfg_hsip", label = "HSIP Survey", choices = list("","Yes", "No")),
          dateInput("rfg_designstart", label = "Design Start Date", value = ""),
          dateInput("rfg_designfinish", label = "Design Completion Date (Plan Sent to BSS)", value = ""),
          dateInput("rfg_constructdate", label = "Construction Completion Date", value = "")
        )
      } else if (input$treatment_type == 'fb'){
        tagList(
          selectInput("fb_roadside", label = "Roadside-Only RRFB Candidate", choices = c('','No','Yes','Yes + Mast Arm','Yes + Mast Arm or Median')),
          selectInput("fb_beaconstatus", label = "Beacon Status", choices = c('','Field Assessment Completed', 'RRFB Activated','RRFB Activated - Mast Arm still needed')),
          textInput("fb_flashdur", label = "Flash Duration"),
          selectInput("fb_xwalk", label = "Existing or Proposed Crosswalk", choices = c('','Existing','Proposed'))
        )
      } else if (input$treatment_type == 'sfs'){
        tagList(
          numericInput("sfs_distance", label = "Distance from Intersection (ft)", value = 0),
          selectInput("sfs_streetside", label = "Side of Street", choices = c('','N','S','E','W')),
          selectInput("sfs_facestraffic", label = "Faces Traffic", choices = c('','N','S','E','W'))
        )
      } else if (input$treatment_type == 'sigPhase') {
        tagList(
          selectInput('sigPhase_NB', label = sigPhase$new()$nb$display, choices = sigPhase$new()$nb$choices),
          selectInput('sigPhase_SB', label = sigPhase$new()$sb$display, choices = sigPhase$new()$sb$choices),
          selectInput('sigPhase_EB', label = sigPhase$new()$eb$display, choices = sigPhase$new()$eb$choices),
          selectInput('sigPhase_WB', label = sigPhase$new()$wb$display, choices = sigPhase$new()$wb$choices)
        )
      }
    }
  })
  
  # Third UI Bin
  output$treatment_info2 <- renderUI({
    if(!is.null(input$treatment_type)){
      if(input$treatment_type == 'Leading Pedestrian Interval'){
        sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE)
      } else if(input$treatment_type == 'rfg'){
        tagList(
          dateInput("rfg_url", label = "URL to Design Plan", value = ""),
          textAreaInput("rfg_notes", "Notes")
        ) 
      } else if(input$treatment_type == 'fb'){
        tagList(
          selectInput("fb_tcr", label = "TCR", choices = c('','No',"Yes")),
          selectInput("fb_curb", label = "Curb Ramps Both Approaches", choices = c('','TBD','Yes')),
          textAreaInput("fb_notes", label = "Poles")
        )
      } else if(input$treatment_type == 'sfs'){
        tagList(
          textInput("sfs_serial","Serial No."),
          selectInput('sfs_solar', 'Solar?', choices = c('','Yes','No')),
          dateInput('sfs_activationdate', 'Activation Date', value = ''),
          textAreaInput('sfs_notes', label = 'Comments')
        )
      }
    }
  })
  
  # Message Object
  output$message <- renderText({msgRV$msg})
  
  # DT
  output$sfs <- renderDT(dbtblRV$sfs, colnames = sfs$tbl_names)
  output$rfg <- renderDT(dbtblRV$rfg, colnames = rfg$tbl_names)
  output$fb <- renderDT(dbtblRV$fb, colnames = fb$tbl_names)
  
  # Observer focused on the input form
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled, length(locationRV$Segment) > 0)

    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  
  
  ##### Reports #####
  output$report <- downloadHandler(
    filename = 'report.html',
    content = function(file) {
      
      src <- normalizePath('Report.Rmd')
      
      # Copy the report file to a temporary directory before processing it, in 
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'Report.Rmd', overwrite = TRUE)
      
      # Getting errors; need to figure this out.
      #webshot::install_phantomjs()
      saveWidget(map(), "temp.html", selfcontained=FALSE)
      webshot("temp.html", file = 'mapPlot.png', cliprect='viewport')
      
      
      # Setup parameters to pas to Rmd document
      params <- list(goegraphy_type = input$geography_type,
                     geographyName = input$geographyName,
                     map = geographyR(),
                     hin = geomClip(hin),
                     pc = geomClip(pc)
      )
      
      # Knit the document, passing in the 'params' list, and eval it in a
      # chile of the global environment (this isolates the code in the doucment
      # from the code in this app)
      out <- rmarkdown::render('Report.Rmd',
                               params = params
      )
      file.rename(out,file)
      
    }
  )
}

