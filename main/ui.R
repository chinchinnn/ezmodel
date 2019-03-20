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

# Define UI for application that draws a histogram
shinyUI(
  ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "EzModel"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("GWR Model", tabName = "gwrmodel", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
  ##-------------------------------------------Tab of Overview-----------------------------------------------------
        tabItem(tabName = "overview",
                fluidPage(theme = shinytheme("flatly"),
                  htmlOutput("overview")
                )
                
        ),
  ##--------------------------------------------End of Overview----------------------------------------------------
  
  ##--------------------------------------------Tab for Main GWR---------------------------------------------------
        tabItem(tabName = "gwrmodel",
                fluidPage(theme = shinytheme("flatly"),
                tabsetPanel(
                  tabPanel("Upload Data", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(
                               tags$h3("Required Fields:"),
                               textInput("variableName",
                                         label = "Name of Variable (Single word/separate words with underscore)",
                                         placeholder = "E.g. MY_FEATURE"),
                               textInput("epsg",
                                         label = "EPSG Code:",
                                         placeholder = "E.g. 4326 or 3414"),
                               tags$h3("Upload Data Here:"),
                               tags$h4("Please ensure uploaded data contains point location data. For CSV Files, location data should have columns labelled X and Y accordingly."),
                               tags$h5("(e.g. Longitude data column labelled X, Latitude data column labelled Y)"),
                                h4(strong("Click on the submit button below to submit.")),
                               # Input: Select a file ----
                               fileInput("shapefile", "Upload Shapefile Here:", multiple = TRUE,
                                         accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
                               tags$hr(),
                               fileInput("csvfile", "Upload CSV File Here:",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               tags$h4("Options below are for CSV uploads only"),
                               checkboxInput("header", "Header (column names in first row)", TRUE),
                               radioButtons("delim", "Delimiter",
                                            choices = c(Comma = ",",
                                                        Semicolon = ";",
                                                        Tab = "\t"),
                                            selected = ","),
                               radioButtons("quote", "Quote",
                                            choices = c(None = "",
                                                        "Double Quote" = '"',
                                                        "Single Quote" = "'"),
                                            selected = '"'),

                               # sliderInput("radius",
                               #              label = "Number of Facilities within X metres:",
                               #              value = 500,
                               #              min = 100,
                               #              max = 1000,
                               #              step = 50
                               #              ),
                               actionButton("uploadSubmit", "Upload"),
                               tags$hr(),
                               tags$h3("Choose Preloaded Data Here:"),
                               checkboxGroupInput(
                                 "preload",
                                 label = "Include:",
                                 choiceNames = c("CBD - Raffles Place Park",  "MRT/LRT Stations", "Preschools", "Primary Schools", "Secondary Schools",
                                                 "Food Centres (e.g. Hawker Centres)", "Parks", "Sports Facilities (e.g. Sports Complex)"),
                                 choiceValues = c("rpp", "mrt", "presch", "prisch", "secsch",
                                                  "foodctr",  "park", "sport"),
                                 selected = c("rpp", "mrt", "prisch")
                               )
                               ),
                             mainPanel(
                               box(
                                 title = "Your Uploaded Data Will Appear Here:",
                                 width = 12,
                                 solidHeader = T,
                                 status = 'primary',
                                 div(
                                   style = 'overflow-x: scroll',
                                  uiOutput("featTitle"),
                                 dataTableOutput('userdata') %>%
                                   withSpinner(type = 4),
                                 leafletOutput(outputId = "userMap") %>%
                                   withSpinner(type = 4)
                                 )
                               )
                             )
                           )
                  ),
                  tabPanel("Define Variables", fluid = TRUE,
                           fluidPage(
                             h5(strong("Filter HDB Resale Data by Year:")),
                             h5("View filtered results in next tab. Default filtered to 2018 data."),
                             fluidRow(
                               column(4,
                                selectInput("fromYr", "From:", c(2015, 2016, 2017, 2018), selected = 2018)
                                             ),
                               column(4,
                                selectInput("toYr", "To:", c(2015, 2016, 2017, 2018), selected = 2018)
                                      ),
                               column(4,
                                actionButton("yrFilterBtn", "Filter")
                                      )
                             )
                           )
                           sidebarLayout(
                             sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                             mainPanel(
                               ##
                             )
                           )
                  ),
                  tabPanel("View Data", fluid = TRUE,
                          fluidPage(
                            dataTableOutput('hdbWithVarsDT') %>%
                              withSpinner(type = 4)
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
                  
                ))
        )
  ##--------------------------------------------End of Main GWR---------------------------------------------------
      )
    )
  )
)
