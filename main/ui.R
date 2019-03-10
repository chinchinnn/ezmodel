#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# library(shiny)
# library(shinydashboard)
source("global.R", local = T)
importPkgs()

# Define UI for application that draws a histogram
shinyUI(
  ui <- dashboardPage(
    dashboardHeader(title = "EzModel"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("GWR Model", tabName = "gwrmodel", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        # Overview tab content ----
        tabItem(tabName = "overview",
                fluidPage(
                  htmlOutput("overview")
                )
                
        ),
        
        # Main GWR tab content ----
        tabItem(tabName = "gwrmodel",
                # Boxes need to be put in a row (or column)
                #   fluidPage(
                #     sidebarLayout(
                #       sidebarPanel(
                #         selectInput(inputId = "test", 
                #                   label = "Test",
                #                   choices = c("A", "B"), 
                #                   selected = "A")
                #     ),
                #       mainPanel(
                #         textOutput(outputId = "test")
                #       )
                #   )
                # )
                tabsetPanel(
                  tabPanel("Upload Data", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(
                               tags$h4("Please ensure uploaded data contains point location data. For CSV Files, location data should have columns labelled X and Y accordingly."),
                               tags$h5("(e.g. Longitude data column labelled X, Latitude data column labelled Y)"),
                               # Input: Select a file ----
                               fileInput("csvfile", "Upload CSV File Here:",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               fileInput("shapefile", "Upload Shapefile Here:", multiple = TRUE,
                                         accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
                               textInput("variableName",
                                         label = "Name of Variable (Single word/separate words with underscore)",
                                         placeholder = "E.g. Nursing"),
                               textInput("epsg",
                                         label = "EPSG Code:",
                                         placeholder = "E.g. 4326 or 3414"),
                               # numericInput("radius",
                               #              label = "Number of Facilities within X metres:",
                               #              value = 3000,
                               #              min = 500,
                               #              max = 10000,
                               #              step = 500
                               #              ),
                               tags$hr(),
                               checkboxGroupInput(
                                 "preload",
                                 label = "Include Preloaded Data:",
                                 choiceNames = c("CBD - Raffles Place Park", "Preschools", "Primary Schools (X = 1000)", "Secondary Schools"),
                                 choiceValues = c("rpp", "presch", "prisch", "secsch"),
                                 selected = "rpp"
                               )
                               ),
                             mainPanel(
                               textOutput(outputId = "test")
                             )
                           )
                  ),
                  tabPanel("Define Variables", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                             mainPanel(
                               ##
                             )
                           )
                  ),
                  tabPanel("View Data", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                             mainPanel(
                               ##
                             )
                           )
                  ),
                  tabPanel("Transform Variables", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                             mainPanel(
                               ##
                             )
                           )
                  ),
                  tabPanel("Select Variables", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                             mainPanel(
                               ##
                             )
                           )
                  ),
                  tabPanel("Run GWR", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                             mainPanel(
                               ##
                             )
                           )
                  )
                  
                )
        )
      )
    )
  )
)
