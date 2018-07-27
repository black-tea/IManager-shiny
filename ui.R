##################################
# Infrastructure Manager UI Code #
##################################

library(shiny)
library(DT)
library(leaflet)
library(shinyjs)
library(sf)
library(dplyr)
library(shinydashboard)
appCSS <- ".mandatory_star { color: red; }"

shinyUI(navbarPage("Infrastructure",
  tabPanel(title = "Add",
           value="AddI",
           box(width = 12,
                fluidPage(
                  # Enable javascript
                  shinyjs::useShinyjs(),
                  shinyjs::inlineCSS(appCSS),
                  div(
                    id = "form",
                    fluidRow(
                      # First UI Bin
                      column(4,
                             uiOutput("treatment_type"),
                             uiOutput("intSelect"),
                             uiOutput("treatment_status")),
                      # Second UI Bin
                      column(4,
                             uiOutput("treatment_info1")),
                      # Third UI Bin
                      column(4,
                             uiOutput("treatment_info2"),
                             conditionalPanel(condition = "input.treatment_type != null && input.treatment_type.length > 0",
                                              actionButton("submit", "Submit", class = "btn-default")))),
                    h6(uiOutput("message"), align="center"),
                    hr(),
                    # Map Output
                    leafletOutput("infrastructureManagerMap")
                    )))),
  tabPanel(title = "View",
           value = "ManageI", 
           tabBox(
             width = 12,
             tabPanel("SFS",dataTableOutput('sfs')),
             tabPanel("FB",dataTableOutput('fb')),
             tabPanel("RFG",dataTableOutput('rfg'))))
))




