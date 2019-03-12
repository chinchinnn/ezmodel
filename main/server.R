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
# importPkgs()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  output$userdata <- DT::renderDataTable(
    DT::datatable({
      # input$csvfile will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      req(input$csvfile, input$epsg, input$variableName)
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          assign(input$variableName, read_delim(input$csvfile$datapath,
                         col_names = input$header,
                         delim = input$delim,
                         quote = input$quote))
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      # eval(as.symbol(input$variableName))
      
      assign(input$variableName, {st_as_sf(eval(as.symbol(input$variableName)), coords = c("long", "lat"), crs = paste0("+init=epsg:", input$epsg)) %>%
        st_transform(crs = 3414)})
        
      return(eval(as.symbol(input$variableName)))
    },
                  class = "nowrap hover row-border",
                  escape = FALSE,
                  options = list(
                    scrollY = TRUE,
                    server = FALSE
                  ))
  )
  
  output$overview <- renderText(
    "<h1>Project Overview:</h1><br>User-customised Geographically Weighted Regression Model for HDB Resale Prices Data."
  )
  
  output$test <- renderText(
    testfxn("Hello")
  )
  
})
