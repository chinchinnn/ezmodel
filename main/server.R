#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


shinyServer(function(input, output, session) {
  ##-----------------------------OVERVIEW TEXT-------------------------------------------------
  output$overview <- renderText(
    "<h1>Application Overview:</h1>"
    #User-customised Geographically Weighted Regression Model for HDB Resale Prices Data.
  )
  
  ##--------------Reactive values object for storing all datasets--------------------------------------------
  datasets <- reactiveValues()
  
  ##---------POPULATING REACTIVE VALUES OBJECT WITH PRELOADED DATA------------------
  datasets$"CBD_RafflesPlacePark" <- rpp
  datasets$"MRT_LRT_Stations" <- mrt
  datasets$"Preschools" <- presch
  datasets$"Primary_Schools" <- prisch
  datasets$"Secondary_Schools" <- secsch
  datasets$"Food_Centres" <- foodctr
  datasets$"Parks" <- park
  datasets$"Sports_Facilities" <- sport
  datasets$"Shopping_Malls" <- shopping
  
  ##---------UPLOAD TAB--------------------------------------------------------------
  # featureTitle <- eventReactive(input$uploadSubmit, {input$variableName})
  # output$featTitle <- renderUI({h3(featureTitle())})
  
  output$resettableInput <- renderUI({
    input$resetUploads
    list(
      fileInput("shapefile", "Upload Shapefile Here:", multiple = TRUE,
                accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
      fileInput("csvfile", "Upload CSV File Here:",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    )
  })
  
  observeEvent(input$uploadSubmit, {
    datasetsList <- reactiveValuesToList(datasets)
    
    ##----------------PROCESS CSV-----------------------
    if(!is.null(input$csvfile) & !(input$variableName %in% names(datasetsList))){
      temp <- read_delim(input$csvfile$datapath,
                         col_names = input$header,
                         delim = input$delim,
                         quote = input$quote)
      temp <- st_as_sf(temp, coords = c("X", "Y"), crs = as.numeric(input$epsg))
      if(input$epsg != 3414){
        temp <- st_transform(temp, 3414)
      }
      datasets[[input$variableName]] <- temp
      
      ##----------------PROCESS SHAPEFILE-----------------
    } 
    if (!is.null(input$shapefile) & !(input$variableName %in% names(datasetsList))){
      prevWD <- getwd()
      shpDF <- input$shapefile
      uploadDir <- dirname(shpDF$datapath[1])
      setwd(uploadDir)
      for(i in 1:nrow(shpDF)){
        file.rename(shpDF$datapath[i], shpDF$name[i])
      }
      shpName <- shpDF$name[grep(x = shpDF$name, pattern = "*.shp")]
      shpPath <- paste(uploadDir, shpName, sep = "/")
      setwd(prevWD)
      temp <- st_read(shpPath, crs = as.numeric(input$epsg))
      
      if(input$epsg != 3414){
        temp <- st_transform(temp, 3414)
      }
      
      datasets[[input$variableName]] <- temp
    }
  })
  
  
  ##---------UPLOAD TAB-------------------------------------------------------
  observe({
    datasetsNames <- names(reactiveValuesToList(datasets))
    userdatasetsNames <- datasetsNames[!(datasetsNames %in% preloadDataNames)]
    
    if (length(userdatasetsNames) > 0){
      updateSelectInput(session, "viewDatasetChoice",
                        label = "Select Uploaded Dataset to View",
                        choices = userdatasetsNames,
                        selected = isolate(input$variableName) #userdatasetsNames[1]
      )
    }
    
    output$userdata <- DT::renderDataTable({
      req(input$variableName)
      req(input$uploadSubmit)
      req(input$viewDatasetChoice)
      DT::datatable(reactiveValuesToList(datasets)[[input$viewDatasetChoice]],
                    class = "nowrap hover row-border",
                    escape = FALSE,
                    options = list(
                      scrollY = TRUE,
                      sScrollX = "100%",
                      scrollX = TRUE,
                      server = FALSE
                    ))}
    )
  })
  ##---------UPLOAD TAB-------------------------------------------------------
  
  output$userMap <- renderLeaflet({
    datasetsList <- reactiveValuesToList(datasets) 
    datasetsNames <- names(datasetsList)
    userdatasetsNames <- datasetsNames[!(datasetsNames %in% preloadDataNames)]
    tmap_mode("view")
    userDotMap <-
      tm_shape(mpsz, name = "SUBZONE_MASTERPLAN") +
      tm_polygons(id = "SUBZONE_N", alpha = 0) +
      tm_view(
        set.zoom.limits = c(11, 14),
        text.size.variable = TRUE
      ) +
      tmap_options(basemaps = "OpenStreetMap")
    if(length(userdatasetsNames) == 0){
      return (tmap_leaflet(userDotMap))
    }
    else{
      for (userdataName in userdatasetsNames){
        userDotMap <- userDotMap +
          tm_shape(datasetsList[[userdataName]], name = userdataName) + tm_dots(col="red") 
      }
      
      tmap_leaflet(userDotMap)}
  })
  
  ##---------POPULATING DYNAMIC INPUT CHECKBOX------------------
  observe({
    data_choices <- names(reactiveValuesToList(datasets))
    
    # Can use character(0) to remove all choices
    if (is.null(data_choices))
      data_choices <- character(0)
    
    # Can also set the label and select items
    updateCheckboxGroupInput(session, "inCheckboxGroup2",
                             label = paste("Include:"),
                             choices = data_choices,
                             selected = isolate(input$variableName)
    )
  })
  
  
  ##-----------------------Dynamic SliderInput for Define Variable Tab----------------------------------------
  observe({
    data_choices <- input$inCheckboxGroup2
    
    # Can use character(0) to remove all choices
    if (is.null(data_choices))
      data_choices <- character(0)
    
    output$Dynamic_Define <- renderUI({
      LL <- vector("list",10)
      for(i in data_choices){
        if (i != "CBD_RafflesPlacePark"){
          LL[[i]] <- list(sliderInput(inputId = paste0(i), label = paste0("No. of ",i, " in X meters radius from HDB"), min=100, step = 10, max=1000, value=500))
        }
      }
      return(LL)                      
    })
  })
  
  
  
  ##dynamic table to output selected data
  output$values <- renderTable({preloaded_data()})
  
  ##------------FILTER HDB DATA BY YEAR------------------------------------------
  ##UPDATES CHOICES IN DROPDOWN LIST TO PREVENT ERROR
  observeEvent(input$fromMth,
               {
                 if(input$fromYr == input$toYr){
                   updateSelectInput(session, "toMth", choices = c(input$fromMth:12), selected = 12)
                 }
               })
  
  hdb_filtered <- reactive(
    return(c(input$fromMth, input$toMth, input$fromYr, input$toYr))
  )
  
  observeEvent(input$sampleNum,
               {
                 if(input$sampleNum == "All"){
                   output$allWarning <- renderUI(HTML("<div style = 'color:red'>Warning: Calculation using all data points will result in long processing times when running the GWR model (>10min)</div>"))
                 } else {
                   output$allWarning <- NULL
                 }
               }
  )
  
  ##------------MUTATE HDB DATA ACCORDING TO HOW USER DEF VARS-------------------
  hdbSample <- reactive({
    ##THIS IS PLACEHOLDER CODE; EDIT ACCORDINGLY
    timePeriod <- hdb_filtered()
    if(timePeriod[3] == timePeriod[4]){
      temp <- filter(hdb, hdb$YEAR %in% c(timePeriod[3]:timePeriod[4])) %>%
        filter(as.numeric(.$MONTH) %in% c(timePeriod[1]:timePeriod[2]))
    } else {
      temp <- filter(hdb, hdb$YEAR %in% c(timePeriod[3]:timePeriod[4]))
      temp <- filter(temp, !as.numeric(temp$MONTH) %in% c(0:timePeriod[1]-1) | temp$YEAR != timePeriod[3]) %>%
        filter(!as.numeric(.$MONTH) %in% c(timePeriod[2]:13) | .$YEAR != timePeriod[4])
    }
    temp <- filter(temp, FLAT_TYPE == input$flatType)

    if (input$sampleNum != "All"){
      if(nrow(temp) > input$sampleNum){
        result <- temp[sample(nrow(temp), input$sampleNum), ]
      }else {
        result <- temp
      }
    } else {
      result <- temp
    }
    result
  })
  
  ##------------RENDER HDB DATA--------------------------------------------------
  staged_data <- reactiveValues(value = hdb,
                                geom = st_as_sf(hdb, coords = c("X", "Y"), crs = "+init=epsg:3414") %>% .$geometry)
  #GEOM is just to store geometry column for 
  observeEvent({input$refreshData | input$yrFilterBtn | input$calcVar}, {
    temp <- hdbSample()
    staged_data$value <- temp
    staged_data$geom <- st_as_sf(staged_data$value, coords = c("X", "Y"), crs = "+init=epsg:3414") %>% .$geometry
    data_choices <- input$inCheckboxGroup2
    for(i in data_choices){
      if (i == "CBD_RafflesPlacePark"){
        result <- dist2nearest_only(staged_data$geom, datasets[[i]], input[[i]], i)
        staged_data$value <- cbind(staged_data$value, result)
      } else {
        result <- process_variables(staged_data$geom, datasets[[i]], input[[i]], i)
        staged_data$value <- cbind(staged_data$value, result)
      }
      
    }
    
  })
  
  output$hdbWithVarsDT <- renderDataTable(
    {datatable({staged_data$value},
               class = "nowrap hover row-border",
               escape = FALSE,
               options = list(
                 scrollY = TRUE,
                 sScrollX = "100%",
                 scrollX = TRUE,
                 server = FALSE
               ))}
  )
  
  ##-----------------------Dynamic Variable Computation for Define Variable Tab----------------------------------------
  # test_process <- reactive(
  #   return(process_variables(staged_data$geom, datasets$"Preschools", 300, "Preschools"))
  # )
  
  
  ##-----------------------TRANSFORM VARS----------------------------------------
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  staged_data_transformed <- reactiveValues(value = data_frame())
  transform_variable_list <- reactiveValues(value = data_frame())
  
  observeEvent(input$refreshData | input$calcVar, ignoreNULL = FALSE, {
    staged_data_transformed$value <- staged_data$value
    transform_variable_list$value <- data_frame(
      `Variable List` = colnames(staged_data$value)[!(colnames(staged_data$value) %in% nonlmVars)],
      `Transform Status` = rep("None", ncol(staged_data$value)-12)
    )
    
    updateSelectInput(session, inputId = "variableTrf_gwr", label = "Select Variable to Transform",
                      choices = colnames(staged_data$value)[!(colnames(staged_data$value) %in% nonlmVars)])
  })
  
  output$transformationTable <- renderDataTable({
    
    cbind(transform_variable_list$value,
          data_frame(Actions = shinyInput(actionButton, nrow(transform_variable_list$value),
                                          'button_', label = "Plot Histogram",
                                          onclick = 'Shiny.onInputChange(\"plothist_button\",  this.id)',
                                          icon("chart-bar"), 
                                          style="color: #fff; background-color: #068587")
          )
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
        geom_histogram(bins = 20, fill = "#068587", colour = "white") +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16, face = 'bold')) +
        ylab("")
    })
    
    session$sendCustomMessage(type = 'resetInputValue', message =  "plothist_button")
    toggleModal(session, "varHistModal", toggle = "open")
  })
  
  
  
  ##-----------------------SELECT VARS----------------------------------------
  corr_variable_list <- reactiveValues(value = data.frame())
  
  observe({
    nameTranslationTrf <- c(sapply( 1:nrow(transform_variable_list$value), 
                                    function(i) variableNameTranslation(as.character(transform_variable_list$value[i, "Transform Status"]))))
    
    corr_variable_list$value <- data.frame(
      var_list = paste0(nameTranslationTrf,
                        unlist(transform_variable_list$value %>% dplyr::select(`Variable List`))),
      includeexclude = c(1, rep(0, nrow(transform_variable_list$value)-1))
    )
  })
  
  
  gwrAllVariable_DisplayList <- data_frame()
  gwrSelectedVariable_DisplayList <- data_frame()
  gwrGlobalVariable_DisplayList <- data_frame()
  
  #RENDER ALL VARS DATA TABLE - TO INCLUDE GLOBAL/LOCAL BUTTONS
  output$gwrAllVariables <- renderDataTable({
    gwrAllVariable_DisplayList <<- corr_variable_list$value %>% filter(includeexclude == 0) %>% dplyr::select(-includeexclude)
    
    plotData <- cbind(gwrAllVariable_DisplayList,
                      data.frame(Local = shinyInput(actionButton, nrow(gwrAllVariable_DisplayList), #c() of actions buttons?                                                        
                                                    'button_', label = "Local",
                                                    onclick = 'Shiny.onInputChange(\"include_button\",  this.id)'),
                                 Global = shinyInput(actionButton, nrow(gwrAllVariable_DisplayList),
                                                     'button_', label = "Global",
                                                     onclick = 'Shiny.onInputChange(\"global_button\",  this.id)')
                      )
    ) 
    colnames(plotData) <- c("Variable List", "Local", "Global")
    
    plotData  %>%
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
  
  #RENDER SELECTED/LOCAL VARS TABLE
  output$gwrSelectedVariables <- renderDataTable({
    gwrSelectedVariable_DisplayList <<- corr_variable_list$value %>% filter(includeexclude == 1) %>% dplyr::select(-includeexclude)
    
    plotData2 <- cbind(gwrSelectedVariable_DisplayList,
                       data.frame(Actions = shinyInput(  actionButton, nrow(gwrSelectedVariable_DisplayList),
                                                         'button_', label = "Exclude",
                                                         onclick = 'Shiny.onInputChange(\"exclude_button\",  this.id)'))
    )
    colnames(plotData2) <- c("Variable List", "Actions")
    
    plotData2  %>%
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
  
  ######################################
  ##REQUIRE A RENDER GLOBAL VARS TABLE##
  ######################################
  output$gwrSelGlobVariables <- renderDataTable({
    gwrGlobalVariable_DisplayList <<- corr_variable_list$value %>% filter(includeexclude == 2) %>% dplyr::select(-includeexclude)
    
    plotData2 <- cbind(gwrGlobalVariable_DisplayList,
                       data.frame(Actions = shinyInput(  actionButton, nrow(gwrGlobalVariable_DisplayList),
                                                         'button_', label = "Exclude",
                                                         onclick = 'Shiny.onInputChange(\"exclude_global_button\",  this.id)'))
    )
    colnames(plotData2) <- c("Variable List", "Actions")
    
    plotData2  %>%
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
  
  
  
  #ACTIONS FOR LOCAL BUTTON
  observeEvent(input$include_button, {
    selectedRow <- as.numeric(strsplit(input$include_button, "_")[[1]][2])
    
    corr_variable_list$value <- corr_variable_list$value %>%
      mutate(includeexclude = ifelse(var_list == gwrAllVariable_DisplayList[selectedRow,1], 1, includeexclude))
    
    session$sendCustomMessage(type = 'resetInputValue', message =  "include_button")
    
  })
  
  #ACTIONS FOR GLOBAL BUTTON
  observeEvent(input$global_button, {
    selectedRow <- as.numeric(strsplit(input$global_button, "_")[[1]][2])
    
    corr_variable_list$value <- corr_variable_list$value %>%
      mutate(includeexclude = ifelse(var_list == gwrAllVariable_DisplayList[selectedRow,1], 2, includeexclude))
    
    session$sendCustomMessage(type = 'resetInputValue', message =  "global_button")
    
  })
  
  #ACTIONS FOR EXCLUDE BUTTON
  observeEvent(input$exclude_button, {
    selectedRow <- as.numeric(strsplit(input$exclude_button, "_")[[1]][2])
    
    corr_variable_list$value <- corr_variable_list$value %>%
      mutate(includeexclude = ifelse(var_list == gwrSelectedVariable_DisplayList[selectedRow,1] &
                                       !str_detect(var_list, "RESALE"), 0, includeexclude))
    
    session$sendCustomMessage(type = 'resetInputValue', message =  "exclude_button")
    
  })
  
  #ACTIONS FOR EXCLUDE GLOBAL BUTTON
  observeEvent(input$exclude_global_button, {
    selectedRow <- as.numeric(strsplit(input$exclude_global_button, "_")[[1]][2])
    
    corr_variable_list$value <- corr_variable_list$value %>%
      mutate(includeexclude = ifelse(var_list == gwrGlobalVariable_DisplayList[selectedRow,1] &
                                       !str_detect(var_list, "RESALE"), 0, includeexclude))
    
    session$sendCustomMessage(type = 'resetInputValue', message =  "exclude_global_button")
    
  })
  
  #CORRELATION PLOT
  correlationPlotData <- reactiveValues(data = data_frame())
  observeEvent(input$corrBtn, {
    if(nrow(gwrSelectedVariable_DisplayList) > 1) {
      variableSelect <- as.character(gwrSelectedVariable_DisplayList[,'var_list'])
      variableSelect <- c(variableSelect, as.character(gwrGlobalVariable_DisplayList[,'var_list']))
      output$corrPlotErrorMsg <- renderText({""})
      output$correlationPlot <- renderPlot({corrplot.mixed(cor(staged_data_transformed$value[,variableSelect]), 
                                                           tl.cex = 1.1, number.cex = 1.3, cl.cex = 1.1,
                                                           lower = "ellipse", upper = "number")})
    } else {
      output$correlationPlot <- renderPlot({})
      output$corrPlotErrorMsg <- renderText({"Please select minimum 1 independent variable!"})
    }
  })
  
  ##########################################
  ##################GWR#####################
  ##########################################
  
  observeEvent(input$gwrPrev, {
    updateTabsetPanel(session, "gwrTabSet",
                      selected = "gwrstep3")
  })
  
  gwrResultTable <- data_frame()
  gwrResultTable_reactive <- reactiveValues(value = data_frame())
  gwrMixedResultTable <- data_frame()
  gwrMixedResultTable_reactive <- reactiveValues(value = data_frame())
  filedownload_name <- ""
  disable("downloadGWRResult")
  
  observeEvent(input$autobandwidth, {
    if (input$autobandwidth)
      disable('bandwidth_field')
    else
      enable('bandwidth_field')
  })
  
  
  observeEvent(input$btnRunModel, {
    
    staging_data_spdf <- as_Spatial(staged_data_transformed$value %>% st_as_sf(coords = c("X", "Y"), crs = 3414))
    
    errorMessage <- ""
    tryCatch(
      {
        variableSelect <- as.character(gwrSelectedVariable_DisplayList[,'var_list'])
        targetVar <- variableSelect[str_detect(variableSelect, "RESALE")]
        variableSelect <- variableSelect[!str_detect(variableSelect, "RESALE")]
        globalvariableSelect <- as.character(gwrGlobalVariable_DisplayList[,'var_list'])
        
        # if the only variable left in the selection box is the target, the following statement will give an error
        target <- paste0(targetVar, " ~ ")
        formula <- paste0(variableSelect[1:(length(variableSelect)-1)], " + ", collapse = '')
        if(length(globalvariableSelect) > 0){
          formula <- paste0(formula, globalvariableSelect[1:(length(globalvariableSelect))], sep = " + ", collapse = '')
        }
        formula <- paste0(target, formula, variableSelect[length(variableSelect)], collapse = '')
        cat(formula)
        cat(globalvariableSelect)
      }, warning = function(war) {
        errorMessage <<- "WARNING"
      }, error = function(err) {
        errorMessage <<- "ERROR"
      }
    )
    
    if(errorMessage != "" ){
      #print(errorMessage)
      showNotification("Please insert independent variable(s) before running the model!", type = "error")
      return()
    }
    
    
    if (input$autobandwidth) {
      bandwidth <- bw.gwr(as.formula(formula),
                          data = staging_data_spdf,
                          approach = 'CV',
                          kernel = input$kernel_select,
                          adaptive = input$adaptivekernel,
                          longlat = FALSE
      )
    } else {
      bandwidth <- as.numeric(input$bandwidth_field)
    }
    
    errorMessage <- ""
    tryCatch({
      gwrModelResult <- gwr.basic(as.formula(formula),
                                  data = staging_data_spdf, bw=bandwidth, kernel = input$kernel_select,
                                  adaptive = input$adaptivekernel, longlat=FALSE)
      if(length(globalvariableSelect) > 0){
        gwrMixedResult <- gwr.mixed(as.formula(formula),
                                    data = staging_data_spdf,
                                    fixed.vars = c(globalvariableSelect),
                                    bw=bandwidth, kernel = input$kernel_select,
                                    adaptive = input$adaptivekernel, longlat=FALSE)
      }
    }, error = function(err) {
      errorMessage  <<- "ERROR"
    })
    
    if (errorMessage != "") {
      showNotification("Please Input Appropriate Bandwidth!", type = "error")
      return()
    }
    
    gwrLocalResult <- as.data.frame(gwrModelResult$SDF)
    if (exists("gwrMixedResult")){
      gwrMixedDFResult <- as.data.frame(gwrMixedResult$SDF)
    }
    
    gwrResultTable <- staged_data_transformed$value[,c(nonlmVars, targetVar, variableSelect)]
    gwrResultTable[,"yhat"] <- gwrLocalResult[,"yhat"]
    
    gwrResultTable[,"Intercept"] <- gwrLocalResult[,"Intercept"]
    gwrResultTable[,"Intercept_TV"] <- gwrLocalResult[,"Intercept_TV"]
    gwrResultTable[, "Local_R2"] <- gwrLocalResult[, "Local_R2"]
    if (exists("gwrMixedResult")){
      gwrMixedResultTable <- staged_data_transformed$value[,c(nonlmVars, targetVar, variableSelect)]
      gwrMixedResultTable[, "Intercept_L"] <- gwrMixedDFResult[,"Intercept_L"]
    }
    ##FOR GWR
    for (dim_ in variableSelect) {
      gwrResultTable[, paste0(dim_, "_Coef")] <- gwrLocalResult[, dim_]
      gwrResultTable[, paste0(dim_, "_TV")] <- gwrLocalResult[, paste0(dim_, "_TV")]
      
      gwrResultTable[, paste0(dim_, "_PV")] <-
        (1- pt(abs(gwrLocalResult[, paste0(dim_, "_TV")]), df = nrow(gwrLocalResult)-1)) * 2
      
    }
    
    for (dim_ in globalvariableSelect) {
      gwrResultTable[, paste0(dim_, "_Coef")] <- gwrLocalResult[, dim_]
      gwrResultTable[, paste0(dim_, "_TV")] <- gwrLocalResult[, paste0(dim_, "_TV")]
      
      gwrResultTable[, paste0(dim_, "_PV")] <-
        (1- pt(abs(gwrLocalResult[, paste0(dim_, "_TV")]), df = nrow(gwrLocalResult)-1)) * 2
      
    }
    if (exists("gwrMixedResult")){
    ##FOR MIXED GWR
    for (dim_ in variableSelect) {
      gwrMixedResultTable[, paste0(dim_, "_Coef_L")] <- gwrMixedDFResult[, paste0(dim_, "_L")]
    }
    
    for (dim_ in globalvariableSelect) {
      gwrMixedResultTable[, paste0(dim_, "_Coef_F")] <- gwrMixedDFResult[, paste0(dim_, "_F")]
    }
    ###############
    }
    year <- paste0(input$fromYr, "_to_", input$toYr)
    output$showYear <- renderText({
      year
    })
    
    ymean <-  mean(as.numeric(unlist(gwrResultTable[,targetVar])))
    y <- as.numeric(unlist(gwrResultTable[,targetVar]))
    ssr <- sqrt(sum((gwrResultTable$yhat - ymean) ** 2))
    sst <- sqrt(sum((ymean - y) ** 2))
    output$showRSquare <- renderText({
      paste0(as.character(round((ssr/sst),4)*100), "%")
    })
    output$showadjRSquare <- renderText({
      paste0(as.character(round(gwrModelResult$GW.diagnostic$gwR2.adj * 100, 2)), "%")
    })
    
    updateSelectInput(session, inputId="paramPlot_select", label="Select Variable to Plot",
                      choices = c("Fitted Resale Price"="yhat", variableSelect, globalvariableSelect, "Intercept"))
    
    # observe({
    #   updateSelectInput(session, inputId="mixedparamPlot_select", label="Select Variable to Plot",
    #                   choices = c(variableSelect[!variableSelect %in% globalvariableSelect]))
    # })
    gwrResultTable_reactive$value <- gwrResultTable
    enable("downloadGWRResult") # enable for download
    if (exists("gwrMixedResult")){
    gwrMixedResultTable_reactive$value <- gwrMixedResultTable
    enable("downloadMixedGWRResult") # enable for download
    }
    
    filedownload_name <<- paste0(variableSelect, collapse = '')
    filedownload_name <<- paste0("GWR_", year, "_", targetVar, "_", filedownload_name, collapse = '')
    
    # global regression output
    output$globalRegressionOutput <- renderPrint({
      summary(gwrModelResult$lm)
    })
    
    output$globalRegressionDiagnosticOutput <- renderPrint({
      var.n<-length(gwrModelResult$lm$coefficients)
      dp.n<-length(gwrModelResult$lm$residuals)
      cat("**********Extra Diagnostic information**********\n")
      lm_RSS<-sum(gwrModelResult$lm$residuals^2)
      lm_Rank<-gwrModelResult$lm$rank     
      cat("Residual sum of squares:", lm_RSS)
      #lm_sigma<-sqrt(lm_RSS/(dp.n-lm_Rank-2))
      lm_sigma<-sqrt(lm_RSS/(dp.n-2))
      cat("\nSigma(hat):", lm_sigma)
      lm_AIC<-dp.n*log(lm_RSS/dp.n)+dp.n*log(2*pi)+dp.n+2*(var.n + 1)
      #AIC = dev + 2.0 * (double)(MGlobal + 1.0);
      cat("\nAIC: ", lm_AIC)
      ##AICc = 	dev + 2.0 * (double)N * ( (double)MGlobal + 1.0) / ((double)N - (double)MGlobal - 2.0);
      lm_AICc= dp.n*log(lm_RSS/dp.n)+dp.n*log(2*pi)+dp.n+2*dp.n*(var.n+1)/(dp.n-var.n-2)
      cat("\nAICc: ", lm_AICc)
    })
    
    output$gwrVerbatimOutput <- renderPrint({
      var.n<-length(gwrModelResult$lm$coefficients)
      dp.n<-length(gwrModelResult$lm$residuals)
      cat("\nKernel function:", gwrModelResult$GW.arguments$kernel, "\n")
      if(gwrModelResult$GW.arguments$adaptive)
        cat("Adaptive bandwidth: ", gwrModelResult$GW.arguments$bw, " (number of nearest neighbours)\n", sep="")
      else
        cat("Fixed bandwidth:", gwrModelResult$GW.arguments$bw, "\n")
      if(gwrModelResult$GW.arguments$rp.given)
        cat("Regression points: A seperate set of regression points is used.\n")
      else
        cat("Regression points: the same locations as observations are used.\n")
      if (gwrModelResult$GW.arguments$DM.given)
        cat("Distance metric: A distance matrix is specified for this model calibration.\n")
      else
      {
        if (gwrModelResult$GW.arguments$longlat)
          cat("Distance metric: Great Circle distance metric is used.\n")
        else if (gwrModelResult$GW.arguments$p==2)
          cat("Distance metric: Euclidean distance metric is used.\n")
        else if (gwrModelResult$GW.arguments$p==1)
          cat("Distance metric: Manhattan distance metric is used.\n")
        else if (is.infinite(gwrModelResult$GW.arguments$p))
          cat("Distance metric: Chebyshev distance metric is used.\n")
        else
          cat("Distance metric: A generalized Minkowski distance metric is used with p=",gwrModelResult$GW.arguments$p,".\n")
        if (gwrModelResult$GW.arguments$theta!=0&&gwrModelResult$GW.arguments$p!=2&&!gwrModelResult$GW.arguments$longlat)
          cat("Coordinate rotation: The coordinate system is rotated by an angle", gwrModelResult$GW.arguments$theta, "in radian.\n")
      }
      
      cat("\n****************Summary of GWR coefficient estimates:******************\n")
      df0 <- as(gwrModelResult$SDF, "data.frame")[,1:var.n, drop=FALSE]
      if (any(is.na(df0))) {
        df0 <- na.omit(df0)
        warning("NAs in coefficients dropped")
      }
      CM <- t(apply(df0, 2, summary))[,c(1:3,5,6)]
      if(var.n==1)
      {
        CM <- matrix(CM, nrow=1)
        colnames(CM) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
        rownames(CM) <- names(gwrModelResult$SDF)[1]
      }
      rnames<-rownames(CM)
      for (i in 1:length(rnames))
        rnames[i]<-paste("   ",rnames[i],sep="")
      rownames(CM) <-rnames
      printCoefmat(CM)
      cat("\n\nR-square value: ",gwrModelResult$GW.diagnostic$gw.R2,"\n")
      cat("Adjusted R-square value: ",gwrModelResult$GW.diagnostic$gwR2.adj,"\n")	
    })
    
    output$gwrDiagnosticOutput <- renderPrint({
      var.n<-length(gwrModelResult$lm$coefficients)
      dp.n<-length(gwrModelResult$lm$residuals)
      if (gwrModelResult$GW.arguments$hatmatrix) 
      {	
        cat("**********Diagnostic information************\n")
        cat("Number of data points:", dp.n, "\n")
        cat("Effective number of parameters:", gwrModelResult$GW.diagnostic$enp, "\n")
        cat("Effective degrees of freedom:", gwrModelResult$GW.diagnostic$edf, "\n")
        cat("AICc:",
            gwrModelResult$GW.diagnostic$AICc, "\n")
        cat("AIC:", gwrModelResult$GW.diagnostic$AIC, "\n")
        cat("Residual sum of squares:", gwrModelResult$GW.diagnostic$RSS.gw, "\n")
      }
    })
    
    #GWR Mixed Output
    output$mixedGWROutput <- renderPrint({
      if(exists("gwrMixedResult")){
        gwrMixedResult
      } else {
        return(c("No Mixed GWR was run. At least one variable has to be defined as Global for Mixed GWR Model to run."))
      }
    })
    
    output$noMixedWarning1 <- renderUI({
      if(exists("gwrMixedResult")){
        div(
          fluidRow(
          column(
            3,
            selectInput(
              inputId = "mixedparamPlot_select",
              label = "Select Variable to Plot",
              choices = c(variableSelect[!variableSelect %in% globalvariableSelect], "Intercept_L")
            ),
            column(9)
          )
        ),
        fluidRow(
          column(6,
                 leafletOutput("mixedparameterMap") %>% withSpinner(type =4, color = "#099090")),
          column(6)
          )
    )
        
               
      } else {
        return(HTML("<h4><p style='color:red'>No Mixed GWR was run. At least one variable has to be defined as Global for Mixed GWR Model to run.</p></h4><br/>"))
      }
    })
    
    output$noMixedWarning2 <- renderUI({
      if(exists("gwrMixedResult")){
        fluidRow(column(
          12,
          box(
            title = "Mixed GWR Result Table",
            width = 12,
            solidHeader = T,
            status = 'primary',
            downloadButton("downloadMixedGWRResult", "Save Result"),
            dataTableOutput("gwrMixedResultDataTable"),
            hr(),
            h4("Data Description"),
            tableOutput('mgwrDdesc')
          )
        ))
      } else {
        return(HTML("<h4><p style='color:red'>No Mixed GWR was run. At least one variable has to be defined as Global for Mixed GWR Model to run.</p></h4><br/>"))
      }
    })
    
  })
  
  
  output$gwrResultDataTable <- renderDataTable({
    
    gwrResultTable_reactive$value %>%
      datatable(
        class = "nowrap hover row-border",
        escape = FALSE,
        options = list(
          dom = 'ftip',
          scrollX = TRUE,
          sScrollX = "100%",
          scrollX = TRUE,
          server = FALSE
        )
      )
  })
  
  output$gwrMixedResultDataTable <- renderDataTable({
    
    gwrMixedResultTable_reactive$value %>%
      datatable(
        class = "nowrap hover row-border",
        escape = FALSE,
        options = list(
          dom = 'ftip',
          scrollX = TRUE,
          sScrollX = "100%",
          scrollX = TRUE,
          server = FALSE
        )
      )
  })
  
  output$downloadGWRResult <- downloadHandler(
    filename = function() {
      paste(filedownload_name, ".csv", sep="")
    },
    content = function(file) {
      write_csv(gwrResultTable_reactive$value, file)
    }
  )
  
  output$downloadMixedGWRResult <- downloadHandler(
    filename = function() {
      paste(filedownload_name, ".csv", sep="")
    },
    content = function(file) {
      write_csv(gwrMixedResultTable_reactive$value, file)
    }
  )
  
  output$VDdesc <- renderTable({
    viewDatadesc
  }, bordered = T, striped = T)
  output$gwrDdesc <- renderTable({
    gwrDataOutputdesc
  }, bordered = T, striped = T)
  output$mgwrDdesc <- renderTable({
    mixedgwrDataOutputdesc
  }, bordered = T, striped = T)
  
  # Map Drawing Exercise
  shapeData_reactives <- reactiveValues()
  
  observe({
    if (nrow(gwrResultTable_reactive$value) == 0) {
      shapeData_reactives$value = 0
      return()
    }
    #Prepare plot data for isoline map
    
    isoline_sf = st_as_sf(gwrResultTable_reactive$value,
                          coords = c("X", "Y")) %>% st_set_crs(3414)
    
    #Chage plot data into spatial data format
    plot_data_sp = as(isoline_sf, "Spatial")
    
    #Replacing point boundary extent with that of polygon
    plot_data_sp@bbox = mpsz_sp@bbox
    
    ##Create an empty grid
    grd              <- as.data.frame(spsample(plot_data_sp, "regular", n=10000))
    names(grd)       <- c("X", "Y")
    coordinates(grd) <- c("X", "Y")
    gridded(grd)     <- TRUE  # Create SpatialPixel object
    fullgrid(grd)    <- TRUE  # Create SpatialGrid object
    
    #Add P's projection information to the empty grid
    proj4string(grd) = proj4string(mpsz_sp)
    
    shapeData_reactives$plot_data_sp <- plot_data_sp
    shapeData_reactives$grd <- grd
    shapeData_reactives$r2grd <- grd
    
    
    shapeData_reactives$value = 1
  })
  
  
  output$parameterMap <- renderLeaflet({
    tryCatch( {
      if (shapeData_reactives$value == 0) {
        return()
      }
      
      variableSelected <- input$paramPlot_select
      if(variableSelected != "yhat"){
        if(variableSelected != "Intercept"){
          variableSelected <- paste0(variableSelected, "_Coef")
        }
      }
      
      #Interpolate the grid cells using a power value of 2 (idp=2.0)
      plot_idw <- gstat::idw(as.formula(paste(variableSelected, "~ 1")), shapeData_reactives$plot_data_sp, newdata=shapeData_reactives$grd, idp=2)
      
      # Convert to raster object then clip to Polygon
      r       <- raster(plot_idw)
      r.m     <- mask(r, mpsz_sp)
      
      tmap_mode("view")
      
      ##setting up colour palette
      if (min(gwrResultTable_reactive$value %>% dplyr::select(UQ(sym(variableSelected)))) < 0)
        colorPalette <- "RdBu"
      else
        colorPalette <- "Blues"
      
      isoline_title <- variableSelected
      if(isoline_title != "yhat")
        isoline_title <- paste0(input$paramPlot_select, "'s Local Coefficient")
      
      param_Plot <-
        tm_shape(r.m, paste0(isoline_title, "'s Isoline Raster")) +
        tm_raster(n = 4, palette = colorPalette, title=isoline_title, style = "pretty") +
        tm_shape(mpsz, "MPSUBZONE")+tm_borders(col = "black",lwd=0.8) + tm_text("SUBZONE_N",size="SHAPE_Area",col="black",alpha = 0.6)+
        tm_shape(shapeData_reactives$plot_data_sp, paste0(isoline_title, "'s Obs Dots")) +
        tm_dots(n = 4, size=0.02, col = variableSelected, palette = colorPalette, legend.show = F,
                id='FULL_ADRESS',popup.vars=c(setNames(variableSelected, input$paramPlot_select))) +
        tm_legend(legend.outside=TRUE)+
        tm_view(set.zoom.limits = c(11,14),text.size.variable = TRUE, view.legend.position = c("right", "bottom"))
      
      tmap_leaflet(param_Plot)
    }, error = function(err) {
      
    })
  })
  
  output$significanceMap <- renderLeaflet({
    tryCatch( {
      if (shapeData_reactives$value == 0) {
        return()
      }
      
      variableSelected <- input$paramPlot_select
      if(variableSelected != "yhat" & variableSelected != "Intercept"){
        variableSelected <- paste0(variableSelected, "_PV")
      }
      if(variableSelected == "yhat"){
        plot_r2_idw <- gstat::idw(as.formula(paste("Local_R2", "~ 1")),
                                  shapeData_reactives$plot_data_sp,
                                  newdata=shapeData_reactives$r2grd,
                                  idp=2)
        r2       <- raster(plot_r2_idw)
        r2.m     <- mask(r2, mpsz_sp)
        tmap_mode("view")
        param_Plot <-
          tm_shape(r2.m, "Local R-Square Isoline Raster") +
          tm_raster(palette = "Oranges", title="Local R-Square", style = "pretty", n = 5) +
          tm_shape(mpsz, "Master Plan Subzone")+tm_borders(col = "black",lwd=0.8)+tm_text("SUBZONE_N",size="SHAPE_Area",col="black",alpha = 0.6)+
          tm_shape(shapeData_reactives$plot_data_sp, "Observation Dots") +
          tm_dots(size=0.02, col = variableSelected, palette = "Oranges", legend.show = F,
                  id='FULL_ADDRESS',popup.vars=c(setNames(variableSelected, input$paramPlot_select))) +
          tm_legend(legend.outside=TRUE)+
          tm_view(set.zoom.limits = c(11,14),text.size.variable = TRUE, view.legend.position = c("right", "bottom"))
        return(tmap_leaflet(param_Plot))
      }
        # return()
      #Interpolate the grid cells using a power value of 2 (idp=2.0)
      plot_idw <- gstat::idw(as.formula(paste(variableSelected, "~ 1")),
                             shapeData_reactives$plot_data_sp,
                             newdata=shapeData_reactives$grd,
                             idp=2)
      plot_r2_idw <- gstat::idw(as.formula(paste("Local_R2", "~ 1")),
                                shapeData_reactives$plot_data_sp,
                                newdata=shapeData_reactives$r2grd,
                                idp=2)
      # Convert to raster object then clip to Polygon
      r       <- raster(plot_idw)
      r.m     <- mask(r, mpsz_sp)
      r2       <- raster(plot_r2_idw)
      r2.m     <- mask(r2, mpsz_sp)
      
      tmap_mode("view")
      
      ##setting up colour palette
      colorPalette <- "-Greens"
      
      
      param_Plot <-
        tm_shape(r.m, paste0(input$paramPlot_select, "'s PV Isoline Raster")) +
        tm_raster(palette = colorPalette, title=paste0(input$paramPlot_select, "'s P-Value"), style = "fixed", breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.2, 1)) +
        tm_shape(r2.m, "Local R-Square Isoline Raster") +
        tm_raster(palette = "Oranges", title="Local R-Square", style = "pretty", n = 5) +
        tm_shape(mpsz, "Master Plan Subzone")+tm_borders(col = "black",lwd=0.8)+tm_text("SUBZONE_N",size="SHAPE_Area",col="black",alpha = 0.6)+
        tm_shape(shapeData_reactives$plot_data_sp, paste0(input$paramPlot_select, "'s PV Obs Dots")) +
        tm_dots(size=0.02, col = variableSelected, palette = colorPalette, legend.show = F,
                id='FULL_ADDRESS',popup.vars=c(setNames(variableSelected, input$paramPlot_select))) +
        tm_legend(legend.outside=TRUE)+
        tm_view(set.zoom.limits = c(11,14),text.size.variable = TRUE, view.legend.position = c("right", "bottom"))
      
      tmap_leaflet(param_Plot)
    }, error = function(err) {
    })
  })
  
  # Map Drawing Exercise
  mixedshapeData_reactives <- reactiveValues()
  
  observe({
    if (nrow(gwrMixedResultTable_reactive$value) == 0) {
      mixedshapeData_reactives$value = 0
      return()
    }
    #Prepare plot data for isoline map
    
    mixed_isoline_sf = st_as_sf(gwrMixedResultTable_reactive$value,
                          coords = c("X", "Y")) %>% st_set_crs(3414)
    
    #Chage plot data into spatial data format
    mixed_plot_data_sp = as(mixed_isoline_sf, "Spatial")
    
    #Replacing point boundary extent with that of polygon
    mixed_plot_data_sp@bbox = mpsz_sp@bbox
    
    ##Create an empty grid
    mixed_grd              <- as.data.frame(spsample(mixed_plot_data_sp, "regular", n=10000))
    names(mixed_grd)       <- c("X", "Y")
    coordinates(mixed_grd) <- c("X", "Y")
    gridded(mixed_grd)     <- TRUE  # Create SpatialPixel object
    fullgrid(mixed_grd)    <- TRUE  # Create SpatialGrid object
    
    #Add P's projection information to the empty grid
    proj4string(mixed_grd) = proj4string(mpsz_sp)
    
    mixedshapeData_reactives$mixed_plot_data_sp <- mixed_plot_data_sp
    mixedshapeData_reactives$mixed_grd <- mixed_grd
    
    
    mixedshapeData_reactives$value = 1
  })
  
  
  output$mixedparameterMap <- renderLeaflet({
    tryCatch( {
      if (mixedshapeData_reactives$value == 0) {
        return()
      }

      variableSelected <- input$mixedparamPlot_select
      if(variableSelected != "Intercept_L"){
        variableSelected <- paste0(variableSelected, "_Coef_L")
      }

      #Interpolate the grid cells using a power value of 2 (idp=2.0)
      plot_idw <- gstat::idw(as.formula(paste(variableSelected, "~ 1")), mixedshapeData_reactives$mixed_plot_data_sp, newdata=mixedshapeData_reactives$mixed_grd, idp=2)

      # Convert to raster object then clip to Polygon
      r       <- raster(plot_idw)
      r.m     <- mask(r, mpsz_sp)

      tmap_mode("view")

      ##setting up colour palette
      if (min(gwrMixedResultTable_reactive$value %>% dplyr::select(UQ(sym(variableSelected)))) < 0)
        colorPalette <- "RdBu"
      else
        colorPalette <- "Blues"

      isoline_title <- variableSelected
      isoline_title <- paste0(input$mixedparamPlot_select, "'s Local Coefficient")

      param_Plot <-
        tm_shape(r.m, paste0(isoline_title, "'s Isoline Raster")) +
        tm_raster(n=4,palette = colorPalette, title=isoline_title, style = "pretty") +
        tm_shape(mpsz, "MPSUBZONE")+tm_borders(col = "black",lwd=0.8) + tm_text("SUBZONE_N",size="SHAPE_Area",col="black",alpha = 0.6) +
        tm_shape(mixedshapeData_reactives$mixed_plot_data_sp, paste0(isoline_title, "'s Obs Dots")) +
        tm_dots(n=4, size=0.02, col = variableSelected, palette = colorPalette, legend.show = F,
                id='FULL_ADRESS',popup.vars=c(setNames(variableSelected, input$mixedparamPlot_select))) +
        tm_legend(legend.outside=TRUE)+
        tm_view(set.zoom.limits = c(11,14),text.size.variable = TRUE, view.legend.position = c("right", "bottom"))

      tmap_leaflet(param_Plot)
    }, error = function(err) {

    })
  })
  
})
