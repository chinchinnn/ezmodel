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
                                 inputId = "preload",
                                 label = "Include:",
                                 choiceNames = c("CBD - Raffles Place Park",  "MRT/LRT Stations", "Preschools", "Primary Schools", "Secondary Schools",
                                                 "Food Centres (e.g. Hawker Centres)", "Parks", "Sports Facilities (e.g. Sports Complex)"),
                                 choiceValues = c("Raffles Place Park",  "MRT/LRT Stations", "Preschools", "Primary Schools", "Secondary Schools",
                                                  "Food Centres", "Parks", "Sports Facilities"),
                                 selected = c("CBD - Raffles Place Park",  "MRT/LRT Stations", "Preschools")
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
                               ),
                               # tableOutput("values")
                               checkboxGroupInput("inCheckboxGroup2", "Input checkbox 2",
                                                  c("Item A", "Item B", "Item C"))
                              
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
                           ),
                           sidebarLayout(
                             sidebarPanel(
                               conditionalPanel(
                                 condition = "input.inCheckboxGroup2.indexOf('Preschools') > -1",
                                 h5(strong("Number of Preschools within radius (X meters) from HDB:")),
                                 sliderInput("preschoolRadius", "Radius (meters)", min=1, max=1000, value=10)
                               ),
                               conditionalPanel(
                                 condition = "input.inCheckboxGroup2.indexOf('MRT/LRT Stations') > -1",
                                 h5(strong("Number of MRT/LRT Stations within radius (X meters) from HDB:")),
                                 sliderInput("mrtRadius", "Radius (meters)", min=1, max=1000, value=10)
                               ),
                               conditionalPanel(
                                 condition = "input.inCheckboxGroup2.indexOf('Primary Schools') > -1",
                                 h5(strong("Number of Primary Schools within radius (X meters) from HDB:")),
                                 sliderInput("prischoolRadius", "Radius (meters)", min=1, max=1000, value=10)
                               ),
                               conditionalPanel(
                                 condition = "input.inCheckboxGroup2.indexOf('Secondary Schools') > -1",
                                 h5(strong("Number of Secondary Schools within radius (X meters) from HDB:")),
                                 sliderInput("secschoolRadius", "Radius (meters)", min=1, max=1000, value=10)
                               ),
                               conditionalPanel(
                                 condition = "input.inCheckboxGroup2.indexOf('Food Centres') > -1",
                                 h5(strong("Number of Food Centres within radius (X meters) from HDB:")),
                                 sliderInput("foodRadius", "Radius (meters)", min=1, max=1000, value=10)
                               ),
                               conditionalPanel(
                                 condition = "input.inCheckboxGroup2.indexOf('Parks') > -1",
                                 h5(strong("Number of Parks within radius (X meters) from HDB:")),
                                 sliderInput("parkRadius", "Radius (meters)", min=1, max=1000, value=10)
                               ),
                               conditionalPanel(
                                 condition = "input.inCheckboxGroup2.indexOf('Sports Facilities') > -1",
                                 h5(strong("Number of Sports Facilities within radius (X meters) from HDB:")),
                                 sliderInput("sportRadius", "Radius (meters)", min=1, max=1000, value=10)
                               ),
                               conditionalPanel(
                                 condition = "input.inCheckboxGroup2.indexOf('Raffles Place Park') > -1",
                                 h5(strong("Number of Raffles Place Park within radius (X meters) from HDB:")),
                                 sliderInput("rppRadius", "Radius (meters)", min=1, max=1000, value=10)
                               )
                             ), 
                             mainPanel(
                               # tableOutput("values")
                               actionButton("calcVar","Calculate Variables")
                               
                             )
                           )
                           
                           
                           
                  ),
                  tabPanel("View Data", fluid = TRUE,
                          fluidPage(
                            dataTableOutput('hdbWithVarsDT') %>%
                              withSpinner(type = 4)
                          )
                  ),
                  tabPanel("Transform Variables", fluid = TRUE, value = 'gwrstep4',
                           tags$br(),
                           bsModal(
                             "varHistModal",
                             "Variable Histogram",
                             trigger = "",
                             size = "large",
                             plotOutput("varhistPlot")
                             
                           ),
                           fluidRow(
                             column(1),
                             column(2,
                                    actionButton("refreshData", "Refresh Variables")
                                    ),
                             column(
                               3,
                               selectInput(
                                 inputId = "variableTrf_gwr",
                                 label = "Select Var to Transform",
                                 choices = c()
                               )
                             ),
                             column(
                               3,
                               selectInput(
                                 "trfMode_gwr",
                                 label = "Select Transform Mode",
                                 choices = c(
                                   "None" = "None",
                                   "Log" =
                                     "Log",
                                   "Sqrt" =
                                     "Sqrt",
                                   "Exp" =
                                     "Exp"
                                 )
                               )
                             ),
                             column(2, actionButton("btnTransform", label =
                                                      "Transform")),
                             column(1)
                           ),
                           
                           fluidRow(
                             column(1),
                             column(
                               10,
                               dataTableOutput("transformationTable") %>% withSpinner(type =
                                                                                        4)
                             ),
                             column(1)
                           )
                  ),
                  tabPanel("Select Variables", fluid = TRUE,
                           value = 'gwrstep3',
                           tags$br(),
                           bsModal(
                             "corrModal",
                             "Correlation Plot",
                             "corrBtn",
                             size = "large",
                             plotOutput("correlationPlot"),
                             textOutput("corrPlotErrorMsg")
                             
                           ),
                           sidebarLayout(
                             sidebarPanel(width = 6,
                                          title = "Independent Variables Pool",
                                          status = 'primary',
                                          dataTableOutput("gwrAllVariables") %>% withSpinner(type = 4)
                                          ),
                             mainPanel(width = 6,
                                       fluidRow(
                                       box(
                                         title = "Selected Local Variable(s) for GWR",
                                         width = 12,
                                         status = 'primary',
                                         dataTableOutput("gwrSelLocVariables") %>% withSpinner(type = 4),
                                         actionButton(inputId = "corrLocBtn", label = "Show Correlations")
                                       )),
                                       fluidRow(
                                       box(
                                         title = "Selected Global Variable(s) for GWR",
                                         width = 12,
                                         status = 'primary',
                                         dataTableOutput("gwrSelGlobVariables") %>% withSpinner(type = 4),
                                         actionButton(inputId = "corrGlobBtn", label = "Show Correlations")
                                       ))
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
