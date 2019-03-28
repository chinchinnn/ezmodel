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
# source("global.R", local = T)

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
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      fluidPage(
        theme = shinytheme("united"),
        shinyjs::useShinyjs(),
        #js function to reset a button, variableName is the button name whose value we want to reset
        tags$script(
          "Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
          Shiny.onInputChange(variableName, null);
          });
          "
      )),
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
                               h4(strong("Required Fields:")),
                               textInput("variableName",
                                         label = "Name of Variable (Single word/separate words with underscore)",
                                         placeholder = "E.g. MY_FEATURE"),
                               textInput("epsg",
                                         label = "EPSG Code:",
                                         placeholder = "E.g. 4326 or 3414"),
                               h4(strong("Upload Data Here:")),
                               h5(strong("Please ensure uploaded data contains point location data. For CSV Files, location data should have columns labelled X and Y accordingly.")),
                               h5("(e.g. Longitude data column labelled X, Latitude data column labelled Y)"),
                                h4("Click on the Upload button below to submit."),
                               # Input: Select a file ----
                               fileInput("shapefile", "Upload Shapefile Here:", multiple = TRUE,
                                         accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
                               fileInput("csvfile", "Upload CSV File Here:",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               box(width = 12, collapsed = TRUE, collapsible = TRUE,
                                 title = "CSV upload options",
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
                                              selected = '"')
                               ),
                               actionButton("uploadSubmit", "Upload", icon("paper-plane"), 
                                            style="color: #fff; background-color: #068587"),
                               tags$hr(),
                               h4(strong("Choose Datasets for Analysis Here:")),
                               checkboxGroupInput("inCheckboxGroup2", "Input checkbox 2",
                                                  c("Item A"))
                               # checkboxGroupInput(
                               #   inputId = "preload",
                               #   label = "Include:",
                               #   choiceNames = c("CBD - Raffles Place Park",  "MRT/LRT Stations", "Preschools", "Primary Schools", "Secondary Schools",
                               #                   "Food Centres (e.g. Hawker Centres)", "Parks", "Sports Facilities (e.g. Sports Complex)"),
                               #   choiceValues = c("Raffles Place Park",  "MRT/LRT Stations", "Preschools", "Primary Schools", "Secondary Schools",
                               #                    "Food Centres", "Parks", "Sports Facilities"),
                               #   selected = c("CBD - Raffles Place Park",  "MRT/LRT Stations", "Preschools")
                               # )
                               ),
                             mainPanel(
                               br(),
                               box(
                                 title = "Your Uploaded Data Will Appear Here:",
                                 width = 12,
                                 solidHeader = T,
                                 status = 'primary',
                                 div(
                                   style = 'overflow-x: scroll',
                                  uiOutput("featTitle"),
                                 dataTableOutput('userdata') %>%
                                   withSpinner(type = 4, color = "#099090"),
                                 leafletOutput(outputId = "userMap") %>%
                                   withSpinner(type = 4, color = "#099090")
                                 )
                               )
                               
                              
                             )
                           )
                  ),
                  tabPanel("Define Variables", fluid = TRUE,
                           fluidPage(
                             br(),
                             box(width = 12,
                               h4(strong("Sample HDB Resale Data by Time Period:")),
                               h5("Default 200 sampled from 2018 data."),
                               h5("Note: The higher the sample count, the longer it will take for the GWR Model to run."),
                               fluidRow(
                                 column(2,
                                        selectInput("fromMth", "From: (Month)", c(1:12), selected = 1)
                                        ),
                                 column(2,
                                        selectInput("fromYr", "(Year)", c(2015, 2016, 2017, 2018), selected = 2018)
                                 ),
                                 column(2,
                                        selectInput("toMth", "To: (Month)", c(1:12), selected = 12)
                                        ),
                                 column(2,
                                        selectInput("toYr", "(Year)", c(2015, 2016, 2017, 2018), selected = 2018)
                                 ),
                                 column(2,
                                        selectInput("sampleNum", "Sample Number", c(100, 200, 300, 400, 500, "All"), selected = 200)
                                        ),
                                 column(2,
                                        actionButton("yrFilterBtn", "Filter", icon("filter"), 
                                                     style="color: #fff; background-color: #068587")
                                 )
                               ),
                               fluidRow(
                                 column(12, h5("View filtered data in the next tab after filtering."))
                               ),
                               fluidRow(
                                 column(12, htmlOutput("allWarning"))
                               )
                             )
                           ),
                           box(width = 12,
                               sidebarLayout(
                                 sidebarPanel(width = 10,
                                              h3(strong("Define Independent Variables: Select Radius of Search for Each Facility/Place")),
                                              uiOutput("Dynamic_Define")  
                                 ), 
                                 mainPanel(width = 2,
                                           # tableOutput("values")
                                           br(),
                                           br(),
                                           actionButton("calcVar","Calculate Variables",
                                                        style="color: #fff; background-color: #068587")
                                           
                                 )
                               )   
                           )
                  ),
                  tabPanel("View Data", fluid = TRUE,
                          fluidPage(
                            dataTableOutput('hdbWithVarsDT') %>%
                              withSpinner(type = 4, color = "#099090")
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
                             column(1, actionButton("btnTransform", label = "Transform", 
                                    style="color: #fff; background-color: #068587")),
                             column(1,
                                    actionButton("refreshData", "Reset", icon("redo-alt"))
                             ),
                             column(3)
                           ),
                           
                           fluidRow(
                             column(1),
                             column(
                               10,
                               dataTableOutput("transformationTable") %>% withSpinner(type = 4, color = "#099090")
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
                                          dataTableOutput("gwrAllVariables") %>% withSpinner(type = 4, color = "#099090")
                                          ),
                             mainPanel(width = 6,
                                       fluidRow(
                                       box(
                                         title = "Selected Local Variable(s) for GWR",
                                         width = 12,
                                         status = 'primary',
                                         dataTableOutput("gwrSelectedVariables") %>% withSpinner(type = 4, color = "#099090")
                                       )),
                                       fluidRow(
                                       box(
                                         title = "Selected Global Variable(s) for GWR",
                                         width = 12,
                                         status = 'primary',
                                         dataTableOutput("gwrSelGlobVariables") %>% withSpinner(type = 4, color = "#099090")
                                       ),
                                       actionButton(inputId = "corrBtn", label = "Show Correlations"))
                             )
                           )
                  ),
                  tabPanel("Run GWR", fluid = TRUE,
                           tags$br(),
                           box(width = 12,
                             h5("Both Non-Mixed and Mixed GWR Models will be run."),
                             h5(strong("Note: Both Local and Global variables will be included in the Non-Mixed GWR Formula."))
                           ),
                           fluidRow(
                             column(4,
                                    fluidRow(
                                      column(1),
                                      column(
                                        4,
                                        textInput('bandwidth_field', label = "Insert your bandwidth:")
                                      ),
                                      column(
                                        6,
                                        materialSwitch(
                                          'autobandwidth',
                                          label = 'Auto Bandwidth',
                                          value = TRUE,
                                          right = TRUE,
                                          status = 'primary'
                                        )
                                      ),
                                      column(1)
                                    )),
                             column(4,
                                    fluidRow(
                                      column(
                                        5,
                                        selectInput(
                                          "kernel_select",
                                          label = "Select Your Kernel",
                                          choices =
                                            c(
                                              "Gaussian" = "gaussian",
                                              "Exponential" =
                                                "exponential",
                                              "Bi-square" =
                                                "bisquare",
                                              "Tricube" =
                                                "tricube",
                                              "Boxcar" =
                                                "boxcar"
                                            ),
                                          selected = "Gaussian"
                                        )
                                      ),
                                      column(
                                        6,
                                        materialSwitch(
                                          'adaptivekernel',
                                          label = 'Adaptive Kernel',
                                          value = TRUE,
                                          right = TRUE,
                                          status = 'primary'
                                        )
                                      ),
                                      column(1)
                                      
                                    )),
                             column(
                               4,
                               actionButton("btnRunModel", label = "Run Models", icon("calculator"), width = '150px',
                                            style="color: #fff; background-color: #068587"),
                               h5(strong("Please allow some time for results to be calculated."))
                             )
                           ),
                           
                           tabBox(
                             title = "GWR Result",
                             width = 12,
                             tabPanel(
                               "Map Result",
                               fluidRow(
                                 column(1),
                                 column(
                                   3,
                                   selectInput(
                                     inputId = "paramPlot_select",
                                     label = "Select Variable to Plot",
                                     choices = c()
                                   )
                                 ),
                                 column(
                                   3,
                                   HTML(
                                     '<div class="info-box-content">
                                     <span class="info-box-text">YEAR: </span>
                                     <span class="info-box-number">
                                     <div id="showYear" class="shiny-text-output"></div>
                                     </span>
                                     </div>'
                                   )
                                   ),
                                 #box(title="", status="primary", textOutput("showYear"))),
                                 column(
                                   4,
                                   HTML(
                                     '<div class="info-box-content">
                                     <span class="info-box-text">Model R-Square: </span>
                                     <span class="info-box-number">
                                     <div id="showRSquare" class="shiny-text-output"></div>
                                     </span>
                                     </div>'
                                   )
                                   ),
                                 column(1)
                                   ),
                               fluidRow(
                                 column(6,
                                        leafletOutput("parameterMap") %>% withSpinner(type =4, color = "#099090")),
                                 column(6,
                                        leafletOutput("significanceMap") %>% withSpinner(type =4, color = "#099090"))
                               )
                                 ),
                             tabPanel("GWR Data Output", fluidRow(column(
                               12,
                               box(
                                 title = "GWR Result Table",
                                 width = 12,
                                 solidHeader = T,
                                 status = 'primary',
                                 downloadButton("downloadGWRResult", "Save Result"),
                                 dataTableOutput("gwrResultDataTable")
                               )
                             ))),
                             tabPanel("Mixed GWR",
                                      tags$style(type='text/css', '#mixedGWROutput {background-color: #068587; color: white; font-family: "TW Cen MT";}'), 
                                      fluidRow(column(
                                        12,
                                        verbatimTextOutput("mixedGWROutput")
                                      ))),
                             
                             tabPanel("Global Regression",
                                      tags$style(type='text/css', '#globalRegressionOutput {background-color: #068587; color: white; font-family: "TW Cen MT";}'), 
                                      fluidRow(column(
                                        12,
                                        verbatimTextOutput("globalRegressionOutput")
                                      )))
                                 )
                  )
                  
                ))
        )
  ##--------------------------------------------End of Main GWR---------------------------------------------------
      )
    )
  )
)
