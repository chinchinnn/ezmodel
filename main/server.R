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

shinyServer(function(input, output) {
  ##-----------------------------OVERVIEW TEXT-------------------------------------------------
  output$overview <- renderText(
    "<h1>Project Overview:</h1><br>User-customised Geographically Weighted Regression Model for HDB Resale Prices Data."
  )
  
  ##---------UPLOAD TAB----------Use myData() to access user-uploaded sf data------------------
  myData <- reactive({
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
    })
  ##---------UPLOAD TAB----------Render myData() into dataTable------------------
  output$userdata <- DT::renderDataTable(
    DT::datatable(myData(),
                  class = "nowrap hover row-border",
                  escape = FALSE,
                  options = list(
                    scrollY = TRUE,
                    server = FALSE
                  ))
  )
  ##---------UPLOAD TAB----------Render myData() into leaflet map------------------
  output$userMap <- renderLeaflet({
    tmap_mode("view")
    userDotMap <- 
      tm_shape(mpsz) +
      tm_polygons(id = "SUBZONE_N") +
      tm_view(
        set.zoom.limits = c(10, 14),
        text.size.variable = TRUE
      )
    if(is.null(myData())) return(tmap_leaflet(userDotMap))
    else{
    userDotMap <- userDotMap + 
      tm_shape(mpsz) + tm_polygons(id = "SUBZONE_N") +
      tm_shape(myData()) + tm_dots(col="red")

    tmap_leaflet(userDotMap)}
  })
  
  ##TESTING CODE ONLY: NOT USED
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  ##TESTING CODE ONLY
  output$test <- renderText(
    testfxn("Hello")
  )
  
  
})
