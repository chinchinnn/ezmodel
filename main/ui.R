#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

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
        # First tab content
        tabItem(tabName = "overview",
                fluidPage(
                  sidebarLayout(
                    sidebarPanel(
                      selectInput(inputId = "year", 
                                  label = "Year:",
                                  choices = c("2013", "2014"), 
                                  selected = "2013")
                    ),
                    mainPanel(
                      
                    )
                  )
                )
        ),
        
        # Second tab content
        tabItem(tabName = "gwrmodel",
          # Boxes need to be put in a row (or column)
          fluidPage(
            sidebarLayout(
              sidebarPanel(
                selectInput(inputId = "test", 
                            label = "Test",
                            choices = c("A", "B"), 
                            selected = "A")
              ),
              mainPanel(
                textOutput(outputId = "test")
              )
            )
          )  
        )
      )
    )
  ))
