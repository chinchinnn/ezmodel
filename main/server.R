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
      # tm_shape(mpsz) + tm_polygons(id = "SUBZONE_N", alpha = 0) +
      tm_shape(myData(), name = input$variableName) + tm_dots(col="red")

    tmap_leaflet(userDotMap)}
  })
  ##---------PRELOADED DATA----------Use myData() to access user-uploaded sf data------------------
  preloaded_data <- reactive({
    input$preload


  })
  
  ##testing dynamic checkgroup
  observe({
    x <- input$preload
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateCheckboxGroupInput(session, "inCheckboxGroup2",
                             label = paste("Datasets Selected for Analysis:"),
                             choices = x,
                             selected = x
    )
  })
  
  ##dynamic table to output selected data
  output$values <- renderTable({preloaded_data()})
  
  ##------------FILTER HDB DATA BY YEAR------------------------------------------
  hdb_filtered <- reactive(
    # input$yrFilterBtn,
    return(c(input$fromYr, input$toYr))
  )
  
  ##------------MUTATE HDB DATA ACCORDING TO HOW USER DEF VARS-------------------
  hdb_withVars <- reactive({
    ##THIS IS PLACEHOLDER CODE; EDIT ACCORDINGLY
    years <- hdb_filtered()
    temp <- filter(hdb, hdb$YEAR %in% c(years[1]:years[2]))
    #SOME MUTATION HERE
    
    st_write(temp, "data/temp.csv", layer_options = "GEOMETRY=AS_XY", delete_dsn = TRUE)
    cat("FILE WRITTEN")
    temp
  })
  
  ##------------RENDER HDB DATA--------------------------------------------------
  masterData <- read_csv("data/temp.csv") %>% st_as_sf(coords = c("X", "Y"), crs = 3414)
  masterDataGeom <- masterData$geometry
  
  observeEvent(input$refreshData, {
  masterData <- read_csv("data/temp.csv") %>% st_as_sf(coords = c("X", "Y"), crs = 3414)
  masterDataGeom <- masterData$geometry
  cat("FILE RE-READ")
  })
  
  output$hdbWithVarsDT <- renderDataTable(
    {datatable({hdb_withVars()},
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
  staged_data <- reactiveValues(value = as_data_frame(masterData %>% st_drop_geometry()))
  
  observeEvent(input$refreshData, ignoreNULL = FALSE, {
  staged_data <- reactiveValues(value = as_data_frame(masterData %>% st_drop_geometry()))
  cat("UPDATED")
  })

  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }

  staged_data_transformed <- reactiveValues(value = data_frame())
  transform_variable_list <- reactiveValues(value = data_frame())

  observeEvent(input$refreshData, ignoreNULL = FALSE, {
    staged_data_transformed$value <- staged_data$value
    cat("CHANGED")
    transform_variable_list$value <- data_frame(
      `Variable List` = colnames(staged_data$value)[!(colnames(staged_data$value) %in% nonlmVars)],
      `Transform Status` = rep("None", ncol(staged_data$value)-10)
    )

    updateSelectInput(session, inputId = "variableTrf_gwr", label = "Select Variable to Transform",
                      choices = colnames(staged_data$value)[!(colnames(staged_data$value) %in% nonlmVars)])
  })

  output$transformationTable <- renderDataTable({

    cbind(transform_variable_list$value,
          data.frame(Actions = shinyInput(actionButton, nrow(transform_variable_list$value),
                                          'button_', label = "Plot Histogram",
                                          onclick = 'Shiny.onInputChange(\"plothist_button\",  this.id)'))
    ) %>%
      datatable(
        class = "nowrap hover row-border",
        escape = FALSE,
        options = list(
          dom = 'tip',
          scrollY = TRUE,
          server = FALSE
        )
      )
  })

  transformVariable <- function(var, command) {
    if(command == "None")
      return(var)
    else if(command == "Log") {
      minVar <- min(var)
      if (minVar < 0)
        return(log(var + abs(minVar) + 1))
      else
        return(log(var))
    } else if(command == "Sqrt") {

      minVar <- min(var)
      if (minVar < 0)
        return(sqrt(var + abs(minVar)))
      else
        return(sqrt(var))
    } else if(command == "Exp") {
      return(exp(var))
    }
  }

  variableNameTranslation <- function(command) {
    switch(command,
           "None"="",
           "Log"="LOG_",
           "Sqrt"="SQRT_",
           "Exp"="EXP_")
  }

  observeEvent(input$btnTransform, {
    trfMode <- as.character(input$trfMode_gwr)
    
    # cat(trfMode)
    varSelected <- as.character(input$variableTrf_gwr)
    # cat(varSelected)
    
    transform_variable_list$value <- transform_variable_list$value %>%
      mutate(`Transform Status` = ifelse(`Variable List` == varSelected, trfMode, `Transform Status`))

    transTrfMode <- variableNameTranslation(trfMode)
    # cat(transTrfMode)
    transformColumn <- transformVariable(as.numeric(unlist(staged_data$value[,input$variableTrf_gwr])), trfMode)
    infpresented <- is.infinite(transformColumn)
    # cat(length(transformColumn))
    staged_data_transformed$value[,paste0(transTrfMode, input$variableTrf_gwr)] <-
      replace(transformColumn, infpresented, 0)

    if(sum(infpresented) > 0)
      showNotification("The transformation produced INF values. Coerced to 0", type="warning")

  })


  observeEvent(input$plothist_button, {
    selectedRow <- as.numeric(strsplit(input$plothist_button, "_")[[1]][2])
    selectedVar <- as.character(transform_variable_list$value[selectedRow,"Variable List"])
    selectedTrf <- variableNameTranslation(as.character(transform_variable_list$value[selectedRow,"Transform Status"]))

    output$varhistPlot <- renderPlot({
      ggplot(data=staged_data_transformed$value, aes(x= UQ(rlang::sym(paste0(selectedTrf, selectedVar))))) +
        geom_histogram(bins = 20, fill = "#0000ff", colour = "grey60") +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16, face = 'bold')) +
        ylab("")
    })

    session$sendCustomMessage(type = 'resetInputValue', message =  "plothist_button")
    toggleModal(session, "varHistModal", toggle = "open")
  })
  
  ##TESTING CODE ONLY
  output$test <- renderText(
    testfxn("Hello")
  )
  
  
  ##-----------------------SELECT VARS----------------------------------------
  
  
  
  
})
