#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# library(shiny)
source("global.R", local = T)

shinyServer(function(input, output, session) {
  ##-----------------------------OVERVIEW TEXT-------------------------------------------------
  output$overview <- renderText(
    "<h1>Project Overview:</h1><br>User-customised Geographically Weighted Regression Model for HDB Resale Prices Data."
  )
  
  ##---------UPLOAD TAB----------Use myData() to access user-uploaded sf data------------------
  featureTitle <- eventReactive(input$uploadSubmit, {input$variableName})
  
  output$featTitle <- renderUI({h3(featureTitle())})
  
  myData <- eventReactive(input$uploadSubmit, {
    ##----------------PROCESS CSV-----------------------
    if(!is.null(input$csvfile)){
      temp <- read_delim(input$csvfile$datapath,
                 col_names = input$header,
                 delim = input$delim,
                 quote = input$quote)
      temp <- st_as_sf(temp, coords = c("X", "Y"), crs = as.numeric(input$epsg))
      if(input$epsg != 3414){
        temp <- st_transform(temp, 3414)
      }
      temp #RETURN
    ##----------------PROCESS SHAPEFILE-----------------
    } else if (!is.null(input$shapefile)){
      prevWD <- getwd()
      shpDF <- input$shapefile
      uploadDir <- dirname(shpDF$datapath[1])
      setwd(uploadDir)
      for(i in 1:nrow(shpDF)){
        file.rename(shpDF$datapath[i], shpDF$name[i])
      }
      shpName <- shpDF$name[grep(x = shpDF$name, pattern = "*.shp")]
      shpPath <- paste(uploadDir, shpName, sep = "/")
      cat(shpPath)
      setwd(prevWD)
      temp <- st_read(shpPath, crs = as.numeric(input$epsg))
      if(input$epsg != 3414){
        temp <- st_transform(temp, 3414)
      }
      temp #RETURN
    } else {
      return(NULL)
    }
    }, ignoreNULL = FALSE)
  ##---------UPLOAD TAB----------Render myData() into dataTable------------------
  output$userdata <- DT::renderDataTable(
    DT::datatable(myData(),
                  class = "nowrap hover row-border",
                  escape = FALSE,
                  options = list(
                    scrollY = TRUE,
                    sScrollX = "100%",
                    scrollX = TRUE,
                    server = FALSE
                  ))
  )
  ##---------UPLOAD TAB----------Render myData() into leaflet map------------------
  output$userMap <- renderLeaflet({
    tmap_mode("view")
    userDotMap <- 
      tm_shape(mpsz) +
      tm_polygons(id = "SUBZONE_N", alpha = 0) +
      tm_view(
        set.zoom.limits = c(10, 14),
        text.size.variable = TRUE
      ) +
      tmap_options(basemaps = "OpenStreetMap")
    if(is.null(myData())) return(tmap_leaflet(userDotMap))
    else{
    userDotMap <- userDotMap + 
      tm_shape(mpsz) + tm_polygons(id = "SUBZONE_N") +
      tm_shape(myData()) + tm_dots(col="red")

    tmap_leaflet(userDotMap)}
  })
  ##---------PRELOADED DATA----------Use myData() to access user-uploaded sf data------------------
  preloaded_data <- reactive({
    input$preload


  })
  
  output$values <- renderTable({preloaded_data()})
  
  ##------------FILTER HDB DATA BY YEAR------------------------------------------
  hdb_filtered <- eventReactive(
    input$yrFilterBtn,
    {filter(hdb, hdb$YEAR %in% c(input$fromYr:input$toYr))},
    ignoreNULL = FALSE
  )
  
  ##------------MUTATE HDB DATA ACCORDING TO HOW USER DEF VARS-------------------
  hdb_withVars <- reactive({
    ##THIS IS PLACEHOLDER CODE; EDIT ACCORDINGLY
    hdb_filtered()
  })
  
  ##------------RENDER HDB DATA--------------------------------------------------
  output$hdbWithVarsDT <- renderDataTable(
    {datatable(hdb_withVars(),
               class = "nowrap hover row-border",
               escape = FALSE,
               options = list(
                 scrollY = TRUE,
                 sScrollX = "100%",
                 scrollX = TRUE,
                 server = FALSE
               ))}
  )
  
  ##-----------------------TRANSFORM VARS----------------------------------------
  
  
  ##TESTING CODE ONLY
  output$test <- renderText(
    testfxn("Hello")
  )
  
  
})
