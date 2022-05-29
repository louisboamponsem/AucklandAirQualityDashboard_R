#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(DT)
library(knitr)
library(markdown)
library(openair)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(leaflet)
library(shinyBS)
library(leaflet.extras)
library(shinythemes)
library(rsconnect)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    navbarPage(
      title = "Auckland Air Quality",
      
      # Home ----------------------------------
      # tab panel 1 - Home
      tabPanel("Home",
               fluidPage(
                 theme = shinytheme("cerulean"),
                 tags$style(type = "text/css", "#map {height: calc(100vh - 120px) !important; text-align: center;}"),
                 includeHTML("HTML/home.html"),
                 shinyjs::useShinyjs(),
                 box(width = 12,
                     status = "primary",
                     leafletOutput("map")),
                 includeHTML("HTML/references.html")
               )),
      
      # Data Analysis ----------------------------------
      # tab panel 2 - Data Analysis
      navbarMenu(
        "Data Analysis",
        
        # Summary Plot ----------------------------------
        # tab panel 2A - Summary Plot
        tabPanel("Summary Plot",
                 #Sidebar - Filters
                 sidebarLayout(
                   sidebarPanel(
                     titlePanel("Summary Plot"),
                     width = 3,
                     tabsetPanel(
                       tabPanel(
                         "Plot 1",
                         radioButtons("summaryDataset", "Site", site_list),
                         uiOutput("summaryParameter"),
                         uiOutput("summaryYear"),
                         uiOutput("summaryMonth"),
                         uiOutput("summaryDay"),
                         submitButton("Submit")
                       ),
                       tabPanel(
                         "Plot 2",
                         radioButtons("summaryDataset2", "Site", site_list),
                         uiOutput("summaryParameter2"),
                         uiOutput("summaryYear2"),
                         uiOutput("summaryMonth2"),
                         uiOutput("summaryDay2"),
                         submitButton("Submit")
                       )
                     )
                   ),
                   
                   mainPanel(width = 9,
                             fluidPage(fluidRow(
                               box(
                                 title = "Summary Plot 2",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("summaryPlot"))
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Summary Plot 2",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("summaryPlot2"))
                               )
                             ), ))
                 )),
        
        # Wind and Pollution Roses ----------------------------------
        # tab panel 2B - Wind and Pollution Roses
        tabPanel(
          "Wind and Pollution Roses",
          #Sidebar - Filters
          sidebarLayout(
            sidebarPanel(
              titlePanel("Wind and Pollution Roses"),
              width = 3,
              tabsetPanel(
                tabPanel(
                  "Plot 1",
                  radioButtons("windRoseDataset", "Site", site_list_wd),
                  uiOutput("windRoseParameter"),
                  uiOutput("windRoseYear"),
                  uiOutput("windRoseMonth"),
                  uiOutput("windRoseDay"),
                  submitButton("Submit")
                ),
                tabPanel(
                  "Plot 2",
                  radioButtons("windRoseDataset2", "Site", site_list_wd),
                  uiOutput("windRoseParameter2"),
                  uiOutput("windRoseYear2"),
                  uiOutput("windRoseMonth2"),
                  uiOutput("windRoseDay2"),
                  submitButton("Submit")
                )
              )
            ),
            
            mainPanel(
              width = 9,
              fluidRow(column(
                width = 6,
                box(
                  title = "Wind Rose 1",
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  withSpinner(plotOutput("windRosePlot"))
                )
              ),
              
              column(
                width = 6,
                box(
                  title = "Pollution Rose 1",
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  withSpinner(plotOutput("pollutionRosePlot"))
                )
              )),
              fluidRow(column(
                width = 6,
                box(
                  title = "Wind Rose 2",
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  withSpinner(plotOutput("windRosePlot2"))
                )
              ),
              
              column(
                width = 6,
                box(
                  title = "Pollution Rose 2",
                  width = 12,
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  withSpinner(plotOutput("pollutionRosePlot2"))
                )
              )),
            )
          )
        ),
        
        # Polar Plot ----------------------------------
        # tab panel 2C - Polar Plot
        tabPanel("Polar Plot",
                 #Sidebar - Filters
                 sidebarLayout(
                   sidebarPanel(
                     titlePanel("Polar Plot"),
                     width = 3,
                     tabsetPanel(
                       tabPanel(
                         "Plot 1",
                         width = 2,
                         radioButtons("polarDataset", "Site", site_list_wd),
                         uiOutput("polarParameter"),
                         uiOutput("polarYear"),
                         uiOutput("polarMonth"),
                         uiOutput("polarDay"),
                         submitButton("Submit")
                       ),
                       tabPanel(
                         "Plot 2",
                         radioButtons("polarDataset2", "Site", site_list_wd),
                         uiOutput("polarParameter2"),
                         uiOutput("polarYear2"),
                         uiOutput("polarMonth2"),
                         uiOutput("polarDay2"),
                         submitButton("Submit")
                       )
                     )
                   ),
                   
                   mainPanel(width = 9,
                             fluidPage(fluidRow(
                               box(
                                 title = "Polar Plot 1",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("polarPlot"))
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Polar Plot 2",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("polarPlot2"))
                               )
                             )))
                 )),
        
        # Polar Annulus Plot ----------------------------------
        # tab panel 2D - Polar Annulus Plot
        tabPanel("Polar Annulus Plot",
                 #Sidebar - Filters
                 sidebarLayout(
                   sidebarPanel(
                     titlePanel("Polar Annulus Plot"),
                     width = 3,
                     tabsetPanel(
                       tabPanel(
                         "Plot 1",
                         radioButtons("polarAnnulusDataset", "Site", site_list_wd),
                         uiOutput("polarAnnulusParameter"),
                         uiOutput("polarAnnulusYear"),
                         uiOutput("polarAnnulusMonth"),
                         uiOutput("polarAnnulusDay"),
                         submitButton("Submit")
                       ),
                       tabPanel(
                         "Plot 2",
                         radioButtons("polarAnnulusDataset2", "Site", site_list_wd),
                         uiOutput("polarAnnulusParameter2"),
                         uiOutput("polarAnnulusYear2"),
                         uiOutput("polarAnnulusMonth2"),
                         uiOutput("polarAnnulusDay2"),
                         submitButton("Submit")
                       )
                     )
                   ),
                   
                   mainPanel(
                     width = 9,
                     fluidRow(column(
                       width = 6,
                       box(
                         title = "Trend plot 1",
                         width = 12,
                         status = "primary",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         withSpinner(plotOutput("polarAnnulusTrend"))
                       )
                     ),
                     
                     column(
                       width = 6,
                       box(
                         title = "Seasonal plot 1",
                         width = 12,
                         status = "primary",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         withSpinner(plotOutput("polarAnnulusSeason"))
                       )
                     )),
                     fluidRow(column(
                       width = 6,
                       box(
                         title = "Day of the week plot 1",
                         width = 12,
                         status = "primary",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         withSpinner(plotOutput("polarAnnulusWeekday"))
                       )
                     ),
                     
                     column(
                       width = 6,
                       box(
                         title = "Diurnal Plot 1",
                         width = 12,
                         status = "primary",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         withSpinner(plotOutput("polarAnnulusHour"))
                       )
                     )),
                     fluidRow(column(
                       width = 6,
                       box(
                         title = "Trend plot 2",
                         width = 12,
                         status = "primary",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         withSpinner(plotOutput("polarAnnulusTrend2"))
                       )
                     ),
                     
                     column(
                       width = 6,
                       box(
                         title = "Seasonal plot 2",
                         width = 12,
                         status = "primary",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         withSpinner(plotOutput("polarAnnulusSeason2"))
                       )
                     )),
                     fluidRow(column(
                       width = 6,
                       box(
                         title = "Day of the week plot 2",
                         width = 12,
                         status = "primary",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         withSpinner(plotOutput("polarAnnulusWeekday2"))
                       )
                     ),
                     
                     column(
                       width = 6,
                       box(
                         title = "Diurnal Plot 2",
                         width = 12,
                         status = "primary",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         withSpinner(plotOutput("polarAnnulusHour2"))
                       )
                     )),
                   )
                 )),
        
        # CPF Plot ----------------------------------
        # tab panel 2E - CPF Plot
        tabPanel("CPF Plot",
                 #Sidebar - Filters
                 sidebarLayout(
                   sidebarPanel(
                     titlePanel("CPF Plot"),
                     width = 3,
                     tabsetPanel(
                       tabPanel(
                         "Plot 1",
                         radioButtons("CPFDataset", "Site", site_list_wd),
                         uiOutput("CPFParameter"),
                         uiOutput("CPFYear"),
                         uiOutput("CPFMonth"),
                         uiOutput("CPFDay"),
                         uiOutput("cpfSilder"),
                         uiOutput("cpfSilderRange"),
                         submitButton("Submit")
                       ),
                       tabPanel(
                         "Plot 2",
                         radioButtons("CPFDataset2", "Site", site_list_wd),
                         uiOutput("CPFParameter2"),
                         uiOutput("CPFYear2"),
                         uiOutput("CPFMonth2"),
                         uiOutput("CPFDay2"),
                         uiOutput("cpfSilder2"),
                         uiOutput("cpfSilderRange2"),
                         submitButton("Submit")
                       )
                     )
                   ),
                   
                   mainPanel(
                     width = 9,
                     fluidRow(column(
                       width = 6,
                       box(
                         title = "CPF - Percentile 1",
                         width = 12,
                         status = "primary",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         withSpinner(plotOutput("CPFPlot"))
                       )
                     ),
                     
                     column(
                       width = 6,
                       box(
                         title = "CPF - Percentile Range 1",
                         width = 12,
                         status = "primary",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         withSpinner(plotOutput("CPFRangePlot"))
                       )
                     )),
                     fluidRow(column(
                       width = 6,
                       box(
                         title = "CPF - Percentile 2",
                         width = 12,
                         status = "primary",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         withSpinner(plotOutput("CPFPlot2"))
                       )
                     ),
                     
                     column(
                       width = 6,
                       box(
                         title = "CPF - Percentile Range 2",
                         width = 12,
                         status = "primary",
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         collapsed = FALSE,
                         withSpinner(plotOutput("CPFRangePlot2"))
                       )
                     )),
                   )
                 )),
        
        # Time Series ----------------------------------
        # tab panel 2F - Time Series
        tabPanel("Time Series",
                 #Sidebar - Filters
                 sidebarLayout(
                   sidebarPanel(
                     titlePanel("Time Series Plot"),
                     width = 3,
                     tabsetPanel(
                       tabPanel(
                         "Plot 1",
                         radioButtons("timeDataset", "Site", site_list),
                         uiOutput("timeParameter"),
                         uiOutput("timeYear"),
                         uiOutput("timeMonth"),
                         uiOutput("timeDay"),
                         submitButton("Submit")
                       ),
                       tabPanel(
                         "Plot 2",
                         radioButtons("timeDataset2", "Site", site_list),
                         uiOutput("timeParameter2"),
                         uiOutput("timeYear2"),
                         uiOutput("timeMonth2"),
                         uiOutput("timeDay2"),
                         submitButton("Submit")
                       )
                     )
                   ),
                   
                   mainPanel(width = 9,
                             fluidPage(fluidRow(
                               box(
                                 title = "Time Series 1",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("timePlot"))
                                 
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Time Series 2",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("timePlot2"))
                               )
                             ), ))
                 )),
        
        # Trend Heat Map ----------------------------------
        # tab panel 2G - Trend Heat Map
        tabPanel("Trend Heat Map",
                 #Sidebar - Filters
                 sidebarLayout(
                   sidebarPanel(
                     titlePanel("Trend Heat Map"),
                     width = 3,
                     tabsetPanel(
                       tabPanel(
                         "Plot 1",
                         radioButtons("heatDataset", "Site", site_list),
                         uiOutput("heatParameter"),
                         uiOutput("heatYear"),
                         uiOutput("heatMonth"),
                         uiOutput("heatDay"),
                         submitButton("Submit")
                       ),
                       tabPanel(
                         "Plot 2",
                         radioButtons("heatDataset2", "Site", site_list),
                         uiOutput("heatParameter2"),
                         uiOutput("heatYear2"),
                         uiOutput("heatMonth2"),
                         uiOutput("heatDay2"),
                         submitButton("Submit")
                       )
                     )
                   ),
                   
                   mainPanel(width = 9,
                             fluidPage(fluidRow(
                               box(
                                 title = "Trend Heat Map 1",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("heatPlot"))
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Trend Heat Map 2",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("heatPlot2"))
                               )
                             )))
                 )),
        
        # Calendar Plot ----------------------------------
        # tab panel 2H - Calendar Plot
        tabPanel("Calendar Plot",
                 #Sidebar - Filters
                 sidebarLayout(
                   sidebarPanel(
                     titlePanel("Calendar Plot"),
                     width = 3,
                     tabsetPanel(
                       tabPanel(
                         "Plot 1",
                         radioButtons("calendarDataset", "Site", site_list),
                         uiOutput("calendarParameter"),
                         uiOutput("calendarYear"),
                         uiOutput("calendarMonth"),
                         uiOutput("calendarDay"),
                         submitButton("Submit")
                       ),
                       tabPanel(
                         "Plot 2",
                         radioButtons("calendarDataset2", "Site", site_list),
                         uiOutput("calendarParameter2"),
                         uiOutput("calendarYear2"),
                         uiOutput("calendarMonth2"),
                         uiOutput("calendarDay2"),
                         submitButton("Submit")
                       )
                     )
                   ),
                   
                   mainPanel(width = 9,
                             fluidPage(fluidRow(
                               box(
                                 title = "Calendar Plot 1",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("calendarPlot"))
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Calendar Plot 2",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("calendarPlot2"))
                               )
                             )))
                 )),
        
        # Theil-Sen Trends ----------------------------------
        # tab panel 2I - Theil-Sen Trends
        tabPanel("Theil-Sen Trends",
                 #Sidebar - Filters
                 sidebarLayout(
                   sidebarPanel(
                     titlePanel("Theil-Sen Trends"),
                     width = 3,
                     tabsetPanel(
                       tabPanel(
                         "Plot 1",
                         radioButtons("theilSenDataset", "Site", site_list),
                         uiOutput("theilSenParameter"),
                         uiOutput("theilSenYear"),
                         uiOutput("theilSenMonth"),
                         uiOutput("theilSenDay"),
                         submitButton("Submit")
                       ),
                       tabPanel(
                         "Plot 2",
                         radioButtons("theilSenDataset2", "Site", site_list),
                         uiOutput("theilSenParameter2"),
                         uiOutput("theilSenYear2"),
                         uiOutput("theilSenMonth2"),
                         uiOutput("theilSenDay2"),
                         submitButton("Submit")
                       )
                     )
                   ),
                   
                   mainPanel(width = 9,
                             fluidPage(fluidRow(
                               box(
                                 title = "Theil-Sen Trends 1",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("theilSenPlot"))
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Theil-Sen Trends 2",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("theilSenPlot2"))
                               )
                             )))
                 )),
        
        # Smooth Trends ----------------------------------
        # tab panel 2J - Smooth Trends
        tabPanel("Smooth Trends",
                 #Sidebar - Filters
                 sidebarLayout(
                   sidebarPanel(
                     titlePanel("Smooth Trends"),
                     width = 3,
                     tabsetPanel(
                       tabPanel(
                         "Plot 1",
                         radioButtons("smoothDataset", "Site", site_list),
                         uiOutput("smoothParameter"),
                         uiOutput("smoothYear"),
                         uiOutput("smoothMonth"),
                         uiOutput("smoothDay"),
                         submitButton("Submit")
                       ),
                       tabPanel(
                         "Plot 2",
                         radioButtons("smoothDataset2", "Site", site_list),
                         uiOutput("smoothParameter2"),
                         uiOutput("smoothYear2"),
                         uiOutput("smoothMonth2"),
                         uiOutput("smoothDay2"),
                         submitButton("Submit")
                       )
                     )
                   ),
                   
                   mainPanel(width = 9,
                             fluidPage(fluidRow(
                               box(
                                 title = "Smooth Trends 1",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("smoothPlot"))
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Smooth Trends 2",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("smoothPlot2"))
                               )
                             )))
                 )),
        
        # Scatter Plot ----------------------------------
        # tab panel 2K - Scatter Plot
        tabPanel("Scatter Plot",
                 #Sidebar - Filters
                 sidebarLayout(
                   sidebarPanel(
                     titlePanel("Scatter Plot"),
                     width = 3,
                     tabsetPanel(
                       tabPanel(
                         "Plot 1",
                         radioButtons("scatterDataset", "Site", site_list),
                         uiOutput("scatterParameterX"),
                         uiOutput("scatterParameterY"),
                         uiOutput("scatterYear"),
                         uiOutput("scatterMonth"),
                         uiOutput("scatterDay"),
                         submitButton("Submit")
                       ),
                       tabPanel(
                         "Plot 2",
                         radioButtons("scatterDataset2", "Site", site_list),
                         uiOutput("scatterParameterX2"),
                         uiOutput("scatterParameterY2"),
                         uiOutput("scatterYear2"),
                         uiOutput("scatterMonth2"),
                         uiOutput("scatterDay2"),
                         submitButton("Submit")
                       )
                     )
                   ),
                   
                   mainPanel(width = 9,
                             fluidPage(fluidRow(
                               box(
                                 title = "Scatter Plot 1",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("scatterPlot"))
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Scatter Plot 2",
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 withSpinner(plotOutput("scatterPlot2"))
                               )
                             )))
                 )),
        
        # Correlation Matrices ----------------------------------
        # tab panel 2L - Correlation Matrices
        tabPanel(
          "Correlation Matrices",
          #Sidebar - Filters
          sidebarLayout(
            sidebarPanel(
              titlePanel("Correlation Matrices"),
              width = 3,
              tabsetPanel(
                tabPanel(
                  "Plot 1",
                  radioButtons("correlationDataset", "Site", site_list),
                  uiOutput("correlationParameter"),
                  uiOutput("correlationYear"),
                  uiOutput("correlationMonth"),
                  uiOutput("correlationDay"),
                  submitButton("Submit")
                ),
                tabPanel(
                  "Plot 2",
                  radioButtons("correlationDataset2", "Site", site_list),
                  uiOutput("correlationParameter2"),
                  uiOutput("correlationYear2"),
                  uiOutput("correlationMonth2"),
                  uiOutput("correlationDay2"),
                  submitButton("Submit")
                )
              )
            ),
            
            mainPanel(width = 9,
                      fluidPage(fluidRow(
                        box(
                          title = "Correlation Matrices 1",
                          width = 12,
                          status = "primary",
                          solidHeader = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                          withSpinner(plotOutput("correlationPlot"))
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Correlation Matrices 2",
                          width = 12,
                          status = "primary",
                          solidHeader = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                          withSpinner(plotOutput("correlationPlot2"))
                        )
                      )))
          )
        ),
        
        # Temporal Variation Plots ----------------------------------
        # tab panel 2M - Temporal Variation Plots
        tabPanel(
          "Temporal Variation Plots",
          #Sidebar - Filters
          sidebarLayout(
            sidebarPanel(
              titlePanel("Temporal Variation Plots"),
              width = 3,
              tabsetPanel(
                tabPanel(
                  "Plot 1",
                  radioButtons("tempVarDataset", "Site", site_list),
                  uiOutput("tempVarParameter"),
                  uiOutput("tempVarYear"),
                  uiOutput("tempVarMonth"),
                  uiOutput("tempVarDay"),
                  submitButton("Submit")
                ),
                tabPanel(
                  "Plot 2",
                  radioButtons("tempVarDataset2", "Site", site_list),
                  uiOutput("tempVarParameter2"),
                  uiOutput("tempVarYear2"),
                  uiOutput("tempVarMonth2"),
                  uiOutput("tempVarDay2"),
                  submitButton("Submit")
                )
              )
            ),
            
            mainPanel(width = 9,
                      fluidPage(fluidRow(
                        box(
                          title = "Temporal Variation Plots 1",
                          width = 12,
                          status = "primary",
                          solidHeader = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                          withSpinner(plotOutput("tempVarPlot"))
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Temporal Variation Plots 2",
                          width = 12,
                          status = "primary",
                          solidHeader = FALSE,
                          collapsible = TRUE,
                          collapsed = FALSE,
                          withSpinner(plotOutput("tempVarPlot2"))
                        )
                      )))
          )
        )
      ),
      
      # Help ----------------------------------
      # tab panel 3 - Help
      navbarMenu(
        "Help",
        # Glossary of terms ----------------------------------
        # tab panel 3a - Glossary of terms
        tabPanel(
          "Glossary of terms",
          includeHTML("HTML/glossaryOfTerms.html"),
          shinyjs::useShinyjs()
        ),
        
        # Plots Interpretation Guide ----------------------------------
        # tab panel 3b - Plots Interpretation Guide
        tabPanel(
          "Plots Interpretation Guide",
          includeHTML("HTML/guide.html"),
          shinyjs::useShinyjs()
        ),
        
        # About ----------------------------------
        # tab panel 3c - About
        tabPanel(
          "About",
          includeHTML("HTML/about.html"),
          shinyjs::useShinyjs(),
          tags$head(
            tags$link(rel = "stylesheet",
                      type = "text/css",
                      href = "plugins/carousel.css"),
            tags$script(src = "plugins/holder.js")
          ),
          tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
          )
        ),
      )
      
    )
  )
))
