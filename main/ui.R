#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

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
                          htmlOutput("overview"),
                          box(title = "Introduction", status = "primary", solidHeader = T,
                              width = 12,
                            column(12,
                                   p("Currently, existing hedonic pricing models that use linear regression fail to
                                     take into account spatial variations among the features in the local surroundings
                                     which may be key contributors to the housing prices, hence there is a need for a
                                     geographically weighted regression (GWR) model to more accurately analyse the
                                     effects of local spatial variations on housing prices."),
                                   p("This appplication seeks to allow users flexibility in calibrating a GWR model,
                                      with a focus on HDB resale prices."),
                                   p("An extra feature in this app allows users to also calibrate Semiparametric GWR models
                                      through selecting both global and local parameters to be used in the regression model.")
                                   )
                          ),
                          box(title = "Using the App", status = "primary", solidHeader = T,
                              width = 12,
                              column(12,
                                     HTML("Our application allows users to upload their own location dataset
                                           of features they wish to experiment as an independent variable against
                                           HDB resale prices."),
                                     HTML("<p>Click on the <strong>GWR Model</strong> button in the side menu to launch the
                                        main app, and follow through the applications' tabs and steps below:</p>"),
                                     HTML("<ol>"),
                                       HTML("<li><strong>Upload</strong> any dataset containing point data of features in Singapore that
                                                might be relevant to HDB resale prices. (Optional)</li>"),
                                       HTML("<li>Alternatively, scroll down on the same page to <strong>find datasets</strong> that have been preloaded
                                               onto the server, to experiment with the app's features.</li>"),
                                       HTML("<li><strong>Define</strong> variables in the next tab based on the selectedfeatures'
                                               geographical relationship with individual resale price observation.</li>"),
                                       HTML("<li><strong>View data</strong> in the following tab to take a look at the
                                               variables that were generated through the calculations.</li>"),
                                       HTML("<li><strong>Transform</strong> the values of any variables that exhibit non-normal distributions 
                                               by observing the histogram plots of the variable values.</li>"),
                                       HTML("<li>Users can now <strong>select</strong> which independent variables to include
                                                in both the Full and Semiparametric GWR models. <em>(Note: do view the correlation plots
                                                to identify dependent variables that are highly correlated with each other 
                                               to prevent the issue of multicollinearity in the regression model.)</em></li>"),
                                       HTML("<li>Finally, <strong>run the GWR model</strong> to derive the coefficient and dependent
                                                value estimates, along with model diagnostics information.</li>"),
                                     HTML("</ol>")
                                     )
                              )
                )
                
        ),
        ##--------------------------------------------End of Overview----------------------------------------------------
        
        ##--------------------------------------------Tab for Main GWR---------------------------------------------------
        tabItem(tabName = "gwrmodel",
                fluidPage(theme = shinytheme("flatly"),
                          tabsetPanel(
                            tabPanel("Upload Data", fluid = TRUE,
                                     sidebarLayout(
                                       sidebarPanel(width = 5,
                                         tabBox(
                                           width = 12,
                                           tabPanel(
                                             title = "Upload",
                                             h4(strong("Required Fields:")),
                                             fluidRow(
                                               column(8,
                                                      textInput("variableName",
                                                                label = "Name of Variable (No Spaces):",
                                                                placeholder = "E.g. MY_FEATURE")
                                               ),
                                               column(4,
                                                      textInput("epsg",
                                                                label = "EPSG Code:",
                                                                placeholder = "E.g. 4326")
                                               )
                                             ),
                                             h4(strong("Upload Data Here:")),
                                             h5(strong("Please ensure uploaded data contains point location data. For CSV Files, location data should have columns labelled X and Y accordingly.")),
                                             # h5("(e.g. Longitude data column labelled X, Latitude data column labelled Y)"),
                                             HTML("<h5 style = 'color: #068587'>Click on the <strong>Upload</strong> button below to submit.</h5>"),
                                             uiOutput('resettableInput'),
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
                                             fluidRow(
                                               column(4,
                                                      actionButton("uploadSubmit", "Upload", icon("paper-plane"), 
                                                                   style="color: #fff; background-color: #068587")
                                               ),
                                               column(2),
                                               column(6,
                                                      actionButton("resetUploads", "Clear Upload Fields")
                                               )
                                             )
                                           ),
                                           tabPanel(
                                             title = "Choose Datasets",
                                             h4(strong("Choose Datasets for Analysis Here:")),
                                             checkboxGroupInput("inCheckboxGroup2", "Input checkbox 2",
                                                                c("Item A"))
                                           )
                                         )
                                       ),
                                       mainPanel(width = 7,
                                         br(),
                                         box(
                                           title = "Your Uploaded Data Will Appear Here:",
                                           width = 12,
                                           solidHeader = T,
                                           status = 'primary',
                                           div(
                                             style = 'overflow-x: scroll',
                                             selectInput("viewDatasetChoice", "Select Dataset to View",
                                                         c()),
                                             uiOutput("featTitle"),
                                             dataTableOutput('userdata') %>%
                                               withSpinner(type = 4, color = "#099090"),
                                             leafletOutput(outputId = "userMap") %>%
                                               withSpinner(type = 4, color = "#099090")
                                           )
                                         ),
                                         h4("To upload multiple sets of data:"),
                                         tags$ol(
                                           tags$li("After uploading one dataset, press \"Clear Upload Fields\" button to clear input fields."),
                                           tags$li("Add a new Shapefile/CSV and press the \"Upload\" button to add the new dataset.")
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Define Variables", fluid = TRUE,
                                     fluidPage(
                                       br(),
                                       box(width = 12,
                                           h4(strong("Sample HDB Resale Data by Time Period and Flat Type:")),
                                           h5("Default 300 sampled from 2018 3-Room Flat data."),
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
                                                    selectInput("flatType", "Flat Type", c("1 ROOM", "2 ROOM", "3 ROOM", "4 Room", "5 Room", "EXECUTIVE", "MULTI-GENERATION"), selected = "3 ROOM")
                                             ),
                                             column(2,
                                                    selectInput("sampleNum", "Sample Number", c(100, 200, 300, 400, 500, "All"), selected = 300)
                                             )
                                             ),
                                           fluidRow(
                                             column(12,
                                                    actionButton("yrFilterBtn", "Filter", icon("filter"), 
                                                                 style="color: #fff; background-color: #068587"),
                                                    HTML("&nbsp;&nbsp;View filtered data in the next tab after filtering.")
                                                    )
                                           ),
                                           fluidRow(
                                             column(12, htmlOutput("allWarning"))
                                           )
                                       )
                                     ),
                                     box(width = 12,
                                         sidebarLayout(
                                           sidebarPanel(width = 10,
                                                        h4(strong("Define Independent Variables: Select Radius of Search for Each Facility/Place")),
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
                                         withSpinner(type = 4, color = "#099090"),
                                       h5("Scroll right to see calculated variables."),
                                       hr(),
                                       h4("Data Description"),
                                       tableOutput('VDdesc')
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
                                         "GWR Map Result",
                                         fluidRow(
                                           column(
                                             3,
                                             selectInput(
                                               inputId = "paramPlot_select",
                                               label = "Select Variable to Plot",
                                               choices = c()
                                             )
                                           ),
                                           column(
                                             2,
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
                                             3,
                                             HTML(
                                               '<div class="info-box-content">
                                               <span class="info-box-text">Model R-Square: </span>
                                               <span class="info-box-number">
                                               <div id="showRSquare" class="shiny-text-output"></div>
                                               </span>
                                               </div>'
                                             )
                                           ),
                                           column(
                                             4,
                                             HTML(
                                               '<div class="info-box-content">
                                                     <span class="info-box-text">Adjusted R-Square: </span>
                                                     <span class="info-box-number">
                                                     <div id="showadjRSquare" class="shiny-text-output"></div>
                                                     </span>
                                                     </div>'
                                             )
                                           )
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
                                           dataTableOutput("gwrResultDataTable"),
                                           hr(),
                                           h4("Data Description"),
                                           tableOutput('gwrDdesc')
                                         )
                                       ))),
                                       
                                       tabPanel("Regressions Comparison",
                                                tags$style(type='text/css', '#globalRegressionOutput {background-color: #068587; color: white; font-family: "TW Cen MT";}'), 
                                                tags$style(type='text/css', '#globalRegressionDiagnosticOutput {background-color: #068587; color: white; font-family: "TW Cen MT";}'),
                                                tags$style(type='text/css', '#gwrVerbatimOutput {background-color: #003663; color: white; font-family: "TW Cen MT";}'), 
                                                tags$style(type='text/css', '#gwrDiagnosticOutput {background-color: #003663; color: white; font-family: "TW Cen MT";}'), 
                                                fluidRow(
                                                  column(6,
                                                         h4("Linear Regression Model Output"),
                                                         verbatimTextOutput("globalRegressionOutput")
                                                  ),
                                                  column(6,
                                                         h4("GWR (Non-mixed) Model Output"),
                                                         verbatimTextOutput("gwrVerbatimOutput")
                                                  )
                                                ),
                                                hr(),
                                                fluidRow(
                                                  column(6,
                                                         h5("Linear Regression Diagnostics"),
                                                         verbatimTextOutput("globalRegressionDiagnosticOutput")
                                                         ),
                                                  column(6,
                                                         h5("GWR (Non-mixed) Diagnostics"),
                                                         verbatimTextOutput("gwrDiagnosticOutput")
                                                         )
                                                )
                                       ),
                                       tabPanel("Mixed GWR Map Result",
                                                tags$style(type='text/css', '#mixedGWROutput {background-color: #068587; color: white; font-family: "TW Cen MT";}'),
                                                uiOutput("noMixedWarning1"),
                                                
                                                fluidRow(
                                                  column(6,
                                                         leafletOutput("mixedparameterMap") %>% withSpinner(type =4, color = "#099090")),
                                                  column(6)
                                                  )
                                                ),
                                       tabPanel("Mixed GWR Data Output", 
                                                uiOutput("noMixedWarning2")
                                                # fluidRow(column(
                                                #   12,
                                                #   box(
                                                #     title = "Mixed GWR Result Table",
                                                #     width = 12,
                                                #     solidHeader = T,
                                                #     status = 'primary',
                                                #     downloadButton("downloadMixedGWRResult", "Save Result"),
                                                #     dataTableOutput("gwrMixedResultDataTable"),
                                                #     hr(),
                                                #     h4("Data Description"),
                                                #     tableOutput('mgwrDdesc')
                                                #   )
                                                # ))
                                       
                                          ),
                                       tabPanel("Mixed GWR Diagnostics",
                                                fluidRow(column(
                                                  12,
                                                  verbatimTextOutput("mixedGWROutput")
                                                ))
                                                )#end of last tab Panel
                                     )
                            )
                            
                          )
                )
        )
        ##--------------------------------------------End of Main GWR---------------------------------------------------
      )
    )
  )
)
