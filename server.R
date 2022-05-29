#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
library(reshape2)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)

shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(data = coor) %>%
      addProviderTiles("CartoDB.Positron") %>%
        setView(
          lng = 174.764129,
          lat = -36.850595,
          zoom = 10
        ) %>%
      addMarkers(
        ~ long,
        ~ lat,
        popup = ~ as.character(info),
        label = ~ as.character(site)
      ) %>% addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE)
  })

  dataset <- function(data){ 
    switch (
      data,
      "Auckland" = auckland_data,
      "Customs Street" = customs_st_data,
      "Glen Eden" = glen_eden_data,
      "Henderson" = henderson_data,
      "Khyber Pass" = khyber_pass_data,
      "Pakuranga" = pakuranga_data,
      "Papatoetoe" = papatoetoe_data,
      "Patumahoe" = patumahoe_data,
      "Penrose" = penrose_data,
      "Queen Street" = queen_st_data,
      "Takapuna" = takapuna_data,
      return (customs_st_data)
    )}
  
  # ----------------------------------
  # tab panel 2A - Summary Plot
  
  summaryUpdateData <- reactive({
    dataset(input$summaryDataset)
  })
  
  output$summaryParameter <- renderUI({
    pickerInput(
      inputId = "summaryParameter",
      label = "Parameter",
      choices = names(summaryUpdateData()),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      multiple = TRUE,
      inline = FALSE,
      selected = "date"
    )
  })
  
  output$summaryYear = renderUI({
    c <- function() {
      return(as.numeric(unique(
        format(summaryUpdateData()$date, format = "%Y")
      )))
    }
    
    pickerInput(
      inputId = "summaryYear",
      label = "Year",
      choices = c(),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = c(),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$summaryMonth = renderUI({
    pickerInput(
      inputId = "summaryMonth",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$summaryDay = renderUI({
    pickerInput(
      inputId = "summaryDay",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$summaryPlot = renderPlot({
    req(input$summaryYear)
    sumData <- summaryUpdateData()
    sumData <- select(sumData, input$summaryParameter)
    filterSummaryData <-
      selectByDate(
        sumData,
        year = input$summaryYear,
        month = input$summaryMonth,
        day = input$summaryDay
      )
    summaryPlot(filterSummaryData)
  })
  
  # ----------------------------------
  # ----------------------------------
  # ----------------------------------
  
  summaryUpdateData2 <- reactive({
    dataset(input$summaryDataset2)
  })
  
  output$summaryParameter2 <- renderUI({
    pickerInput(
      inputId = "summaryParameter2",
      label = "Parameter",
      choices = names(summaryUpdateData2()),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      multiple = TRUE,
      inline = FALSE,
      selected = "date"
    )
  })
  
  output$summaryYear2 = renderUI({
    pickerInput(
      inputId = "summaryYear2",
      label = "Year",
      choices = as.numeric(unique(
        format(summaryUpdateData2()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(summaryUpdateData2()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$summaryMonth2 = renderUI({
    pickerInput(
      inputId = "summaryMonth2",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$summaryDay2 = renderUI({
    pickerInput(
      inputId = "summaryDay2",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$summaryPlot2 = renderPlot({
    req(input$summaryYear2)
    sumData2 <- summaryUpdateData2()
    sumData2 <- select(sumData2, input$summaryParameter2)
    filterSummaryData2 <-
      selectByDate(
        sumData2,
        year = input$summaryYear2,
        month = input$summaryMonth2,
        day = input$summaryDay2
      )
    summaryPlot(filterSummaryData2)
  })
  
  # ----------------------------------
  # tab panel 2B - Wind and Pollution Roses
  
  windRoseUpdateData <- reactive({
    dataset(input$windRoseDataset)
  })
  
  output$windRoseParameter <- renderUI({
    pickerInput(
      inputId = "windRoseParameter",
      label = "Parameter",
      choices = names(select_if(windRoseUpdateData(), is.numeric))
    )
  })
  
  output$windRoseYear = renderUI({
    pickerInput(
      inputId = "windRoseYear",
      label = "Year",
      choices = as.numeric(unique(
        format(windRoseUpdateData()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(windRoseUpdateData()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$windRoseMonth = renderUI({
    pickerInput(
      inputId = "windRoseMonth",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$windRoseDay = renderUI({
    pickerInput(
      inputId = "windRoseDay",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$windRosePlot = renderPlot({
    req(input$windRoseYear)
    filterWindRoseData <-
      selectByDate(
        windRoseUpdateData(),
        year = input$windRoseYear,
        month = input$windRoseMonth,
        day = input$windRoseDay
      )
    windRose(filterWindRoseData, pollutant = input$windRoseParameter)
  })
  
  output$pollutionRosePlot = renderPlot({
    req(input$windRoseYear)
    filterPollutionRoseData <-
      selectByDate(
        windRoseUpdateData(),
        year = input$windRoseYear,
        month = input$windRoseMonth,
        day = input$windRoseDay
      )
    pollutionRose(filterPollutionRoseData, pollutant = input$windRoseParameter)
  })
  
  
  # ----------------------------------
  # ----------------------------------
  # ----------------------------------
  
  windRoseUpdateData2 <- reactive({
    dataset(input$windRoseDataset2)
  })
  
  output$windRoseParameter2 <- renderUI({
    pickerInput(
      inputId = "windRoseParameter2",
      label = "Parameter",
      choices = names(select_if(windRoseUpdateData2(), is.numeric))
    )
  })
  
  output$windRoseYear2 = renderUI({
    pickerInput(
      inputId = "windRoseYear2",
      label = "Year",
      choices = as.numeric(unique(
        format(windRoseUpdateData2()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(windRoseUpdateData2()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$windRoseMonth2 = renderUI({
    pickerInput(
      inputId = "windRoseMonth2",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$windRoseDay2 = renderUI({
    pickerInput(
      inputId = "windRoseDay2",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$windRosePlot2 = renderPlot({
    req(input$windRoseYear2)
    filterWindRoseData2 <-
      selectByDate(
        windRoseUpdateData2(),
        year = input$windRoseYear2,
        month = input$windRoseMonth2,
        day = input$windRoseDay2
      )
    windRose(filterWindRoseData2, pollutant = input$windRoseParameter2)
  })
  
  output$pollutionRosePlot2 = renderPlot({
    req(input$windRoseYear2)
    filterPollutionRoseData2 <-
      selectByDate(
        windRoseUpdateData2(),
        year = input$windRoseYear2,
        month = input$windRoseMonth2,
        day = input$windRoseDay2
      )
    pollutionRose(filterPollutionRoseData2, pollutant = input$windRoseParameter2)
    
  })
  
  
  # ----------------------------------
  # tab panel 2C - Polar Plot
  
  polarUpdateData <- reactive({
    dataset(input$polarDataset)
  })
  
  output$polarParameter <- renderUI({
    pickerInput(
      inputId = "polarParameter",
      label = "Parameter",
      choices = names(select_if(polarUpdateData(), is.numeric))
    )
  })
  
  output$polarYear = renderUI({
    pickerInput(
      inputId = "polarYear",
      label = "Year",
      choices = as.numeric(unique(
        format(polarUpdateData()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(polarUpdateData()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$polarMonth = renderUI({
    pickerInput(
      inputId = "polarMonth",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$polarDay = renderUI({
    pickerInput(
      inputId = "polarDay",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$polarPlot = renderPlot({
    req(input$polarYear)
    filterPolarData <-
      selectByDate(
        polarUpdateData(),
        year = input$polarYear,
        month = input$polarMonth,
        day = input$polarDay
      )
    polarPlot(filterPolarData,
              pollutant = input$polarParameter,
              uncertainty = TRUE)
  })
  
  # ----------------------------------
  # ----------------------------------
  # ----------------------------------
  
  polarUpdateData2 <- reactive({
    dataset(input$polarDataset2)
  })
  
  output$polarParameter2 <- renderUI({
    pickerInput(
      inputId = "polarParameter2",
      label = "Parameter",
      choices = names(select_if(polarUpdateData2(), is.numeric))
    )
  })
  
  output$polarYear2 = renderUI({
    pickerInput(
      inputId = "polarYear2",
      label = "Year",
      choices = as.numeric(unique(
        format(polarUpdateData2()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(polarUpdateData2()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$polarMonth2 = renderUI({
    pickerInput(
      inputId = "polarMonth2",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$polarDay2 = renderUI({
    pickerInput(
      inputId = "polarDay2",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$polarPlot2 = renderPlot({
    req(input$polarYear2)
    filterPolarData2 <-
      selectByDate(
        polarUpdateData2(),
        year = input$polarYear2,
        month = input$polarMonth2,
        day = input$polarDay2
      )
    polarPlot(
      filterPolarData2,
      pollutant = input$polarParameter2,
      uncertainty = TRUE
    )
  })
  
  # ----------------------------------
  # tab panel 2D - Polar Annulus Plot
  
  polarAnnulusUpdateData <- reactive({
    dataset(input$polarAnnulusDataset)
  })

  output$polarAnnulusParameter <- renderUI({
    pickerInput(
      inputId = "polarAnnulusParameter",
      label = "Parameter",
      choices = names(select_if(
        polarAnnulusUpdateData(), is.numeric
      ))
    )
  })
  
  output$polarAnnulusYear = renderUI({
    pickerInput(
      inputId = "polarAnnulusYear",
      label = "Year",
      choices = as.numeric(unique(
        format(polarAnnulusUpdateData()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(polarAnnulusUpdateData()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$polarAnnulusMonth = renderUI({
    pickerInput(
      inputId = "polarAnnulusMonth",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$polarAnnulusDay = renderUI({
    pickerInput(
      inputId = "polarAnnulusDay",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$polarAnnulusTrend = renderPlot({
    req(input$polarAnnulusYear)
    filterpolarAnnulusData <-
      selectByDate(
        polarAnnulusUpdateData(),
        year = input$polarAnnulusYear,
        month = input$polarAnnulusMonth,
        day = input$polarAnnulusDay
      )
    polarAnnulus(
      filterpolarAnnulusData,
      poll = input$polarAnnulusParameter,
      period = "trend",
      main = "Trend"
    )
  })
  
  output$polarAnnulusSeason = renderPlot({
    req(input$polarAnnulusYear)
    filterpolarAnnulusData <-
      selectByDate(
        polarAnnulusUpdateData(),
        year = input$polarAnnulusYear,
        month = input$polarAnnulusMonth,
        day = input$polarAnnulusDay
      )
    polarAnnulus(
      filterpolarAnnulusData,
      poll = input$polarAnnulusParameter,
      period = "season",
      main = "Season"
    )
  })
  
  output$polarAnnulusWeekday = renderPlot({
    req(input$polarAnnulusYear)
    filterpolarAnnulusData <-
      selectByDate(
        polarAnnulusUpdateData(),
        year = input$polarAnnulusYear,
        month = input$polarAnnulusMonth,
        day = input$polarAnnulusDay
      )
    polarAnnulus(
      filterpolarAnnulusData,
      poll = input$polarAnnulusParameter,
      period = "weekday",
      main = "Weekday"
    )
  })
  
  output$polarAnnulusHour = renderPlot({
    req(input$polarAnnulusYear)
    filterpolarAnnulusData <-
      selectByDate(
        polarAnnulusUpdateData(),
        year = input$polarAnnulusYear,
        month = input$polarAnnulusMonth,
        day = input$polarAnnulusDay
      )
    polarAnnulus(
      filterpolarAnnulusData,
      poll = input$polarAnnulusParameter,
      period = "hour",
      main = "Hour"
    )
  })
  
  # ----------------------------------
  # ----------------------------------
  # ----------------------------------
  
  polarAnnulusUpdateData2 <- reactive({
    dataset(input$polarAnnulusDataset2)
  })
  
  output$polarAnnulusParameter2 <- renderUI({
    pickerInput(
      inputId = "polarAnnulusParameter2",
      label = "Parameter",
      choices = names(select_if(
        polarAnnulusUpdateData2(), is.numeric
      ))
    )
  })
  
  output$polarAnnulusYear2 = renderUI({
    pickerInput(
      inputId = "polarAnnulusYear2",
      label = "Year",
      choices = as.numeric(unique(
        format(polarAnnulusUpdateData2()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(polarAnnulusUpdateData2()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$polarAnnulusMonth2 = renderUI({
    pickerInput(
      inputId = "polarAnnulusMonth2",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$polarAnnulusDay2 = renderUI({
    pickerInput(
      inputId = "polarAnnulusDay2",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$polarAnnulusTrend2 = renderPlot({
    req(input$polarAnnulusYear2)
    filterpolarAnnulusData2 <-
      selectByDate(
        polarAnnulusUpdateData2(),
        year = input$polarAnnulusYear2,
        month = input$polarAnnulusMonth2,
        day = input$polarAnnulusDay2
      )
    polarAnnulus(
      filterpolarAnnulusData2,
      poll = input$polarAnnulusParameter2,
      period = "trend",
      main = "Trend"
    )
  })
  
  output$polarAnnulusSeason2 = renderPlot({
    req(input$polarAnnulusYear2)
    filterpolarAnnulusData2 <-
      selectByDate(
        polarAnnulusUpdateData2(),
        year = input$polarAnnulusYear2,
        month = input$polarAnnulusMonth2,
        day = input$polarAnnulusDay2
      )
    polarAnnulus(
      filterpolarAnnulusData2,
      poll = input$polarAnnulusParameter2,
      period = "season",
      main = "Season"
    )
  })
  
  output$polarAnnulusWeekday2 = renderPlot({
    req(input$polarAnnulusYear2)
    filterpolarAnnulusData2 <-
      selectByDate(
        polarAnnulusUpdateData2(),
        year = input$polarAnnulusYear2,
        month = input$polarAnnulusMonth2,
        day = input$polarAnnulusDay2
      )
    polarAnnulus(
      filterpolarAnnulusData2,
      poll = input$polarAnnulusParameter2,
      period = "weekday",
      main = "Weekday"
    )
  })
  
  output$polarAnnulusHour2 = renderPlot({
    req(input$polarAnnulusYear2)
    filterpolarAnnulusData2 <-
      selectByDate(
        polarAnnulusUpdateData2(),
        year = input$polarAnnulusYear2,
        month = input$polarAnnulusMonth2,
        day = input$polarAnnulusDay2
      )
    polarAnnulus(
      filterpolarAnnulusData2,
      poll = input$polarAnnulusParameter2,
      period = "hour",
      main = "Hour"
    )
  })
  
  # ----------------------------------
  # tab panel 2E - CPF Plot
  
  CPFUpdateData <- reactive({
    dataset(input$CPFDataset)
  })
  
  output$CPFParameter <- renderUI({
    pickerInput(
      inputId = "CPFParameter",
      label = "Parameter",
      choices = names(select_if(CPFUpdateData(), is.numeric))
    )
  })
  
  output$CPFYear = renderUI({
    pickerInput(
      inputId = "CPFYear",
      label = "Year",
      choices = as.numeric(unique(
        format(CPFUpdateData()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(CPFUpdateData()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$CPFMonth = renderUI({
    pickerInput(
      inputId = "CPFMonth",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$CPFDay = renderUI({
    pickerInput(
      inputId = "CPFDay",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$cpfSilder = renderUI({
    sliderInput("percentile", "Percentile", 0, 100, 90)
  })
  
  output$cpfSilderRange = renderUI({
    sliderInput(
      "range",
      "Percentile Range:",
      min = 1,
      max = 100,
      value = c(75, 100)
    )
  })
  
  output$CPFPlot = renderPlot({
    req(input$CPFYear)
    filterCPFData <-
      selectByDate(
        CPFUpdateData(),
        year = input$CPFYear,
        month = input$CPFMonth,
        day = input$CPFDay
      )
    polarPlot(
      filterCPFData,
      poll = input$CPFParameter,
      stati = "cpf",
      percentile = input$percentile
    )
  })
  
  output$CPFRangePlot = renderPlot({
    req(input$CPFYear)
    filterCPFData <-
      selectByDate(
        CPFUpdateData(),
        year = input$CPFYear,
        month = input$CPFMonth,
        day = input$CPFDay
      )
    polarPlot(
      filterCPFData,
      poll = input$CPFParameter,
      stati = "cpf",
      percentile = input$range
    )
  })
  
  
  # ----------------------------------
  # ----------------------------------
  # ----------------------------------
  
  CPFUpdateData2 <- reactive({
    dataset(input$CPFDataset2)
  })
  
  output$CPFParameter2 <- renderUI({
    pickerInput(
      inputId = "CPFParameter2",
      label = "Parameter",
      choices = names(select_if(CPFUpdateData2(), is.numeric))
    )
  })
  
  output$CPFYear2 = renderUI({
    pickerInput(
      inputId = "CPFYear2",
      label = "Year",
      choices = as.numeric(unique(
        format(CPFUpdateData2()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(CPFUpdateData2()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$CPFMonth2 = renderUI({
    pickerInput(
      inputId = "CPFMonth2",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$CPFDay2 = renderUI({
    pickerInput(
      inputId = "CPFDay2",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$cpfSilder2 = renderUI({
    sliderInput("percentile2", "Percentile", 0, 100, 90)
  })
  
  output$cpfSilderRange2 = renderUI({
    sliderInput(
      "range2",
      "Percentile Range:",
      min = 1,
      max = 100,
      value = c(75, 100)
    )
  })
  
  output$CPFPlot2 = renderPlot({
    req(input$CPFYear2)
    filterCPFData2 <-
      selectByDate(
        CPFUpdateData2(),
        year = input$CPFYear2,
        month = input$CPFMonth2,
        day = input$CPFDay2
      )
    polarPlot(
      filterCPFData2,
      poll = input$CPFParameter2,
      stati = "cpf",
      percentile = input$percentile2
    )
  })
  
  output$CPFRangePlot2 = renderPlot({
    req(input$CPFYear2)
    filterCPFData2 <-
      selectByDate(
        CPFUpdateData2(),
        year = input$CPFYear2,
        month = input$CPFMonth2,
        day = input$CPFDay2
      )
    polarPlot(
      filterCPFData2,
      poll = input$CPFParameter2,
      stati = "cpf",
      percentile = input$range2
    )
  })
  
  # ----------------------------------
  # tab panel 2F - Time Series
  
  timeUpdateData <- reactive({
    dataset(input$timeDataset)
  })
  
  output$timeParameter <- renderUI({
    pickerInput(
      inputId = "timeParameter",
      label = "Parameter",
      choices = names(select_if(timeUpdateData(), is.numeric)),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      multiple = TRUE,
      inline = FALSE,
      selected = "BC.370."
    )
  })
  
  output$timeYear = renderUI({
    c <- function() {
      return(as.numeric(unique(
        format(timeUpdateData()$date, format = "%Y")
      )))
    }
    
    pickerInput(
      inputId = "timeYear",
      label = "Year",
      choices = c(),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = c(),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$timeMonth = renderUI({
    pickerInput(
      inputId = "timeMonth",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$timeDay = renderUI({
    pickerInput(
      inputId = "timeDay",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$timePlot = renderPlot({
    req(input$timeYear)
    filtertimeData <-
      selectByDate(
        timeUpdateData(),
        year = input$timeYear,
        month = input$timeMonth,
        day = input$timeDay
      )
    timePlot(
      filtertimeData,
      poll = input$timeParameter,
      avg.time = "month",
      y.relation = "free"
    )
  })
  
  # ----------------------------------
  # ----------------------------------
  # ----------------------------------
  
  timeUpdateData2 <- reactive({
    dataset(input$timeDataset2)
  })
  
  output$timeParameter2 <- renderUI({
    pickerInput(
      inputId = "timeParameter2",
      label = "Parameter",
      choices = names(select_if(timeUpdateData2(), is.numeric)),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      multiple = TRUE,
      inline = FALSE,
      selected = "BC.370."
    )
  })
  
  output$timeYear2 = renderUI({
    pickerInput(
      inputId = "timeYear2",
      label = "Year",
      choices = as.numeric(unique(
        format(timeUpdateData2()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(timeUpdateData2()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$timeMonth2 = renderUI({
    pickerInput(
      inputId = "timeMonth2",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$timeDay2 = renderUI({
    pickerInput(
      inputId = "timeDay2",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$timePlot2 = renderPlot({
    req(input$timeYear2)
    filtertimeData2 <-
      selectByDate(
        timeUpdateData2(),
        year = input$timeYear2,
        month = input$timeMonth2,
        day = input$timeDay2
      )
    timePlot(
      filtertimeData2,
      poll = input$timeParameter2,
      avg.time = "month",
      y.relation = "free"
    )
  })
  
  # ----------------------------------
  # tab panel 2G - Tread Heat Map
  
  heatUpdateData <- reactive({
    dataset(input$heatDataset)
  })

  output$heatParameter <- renderUI({
    pickerInput(
      inputId = "heatParameter",
      label = "Parameter",
      choices = names(select_if(heatUpdateData(), is.numeric))
    )
  })
  
  output$heatYear = renderUI({
    pickerInput(
      inputId = "heatYear",
      label = "Year",
      choices = as.numeric(unique(
        format(heatUpdateData()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(heatUpdateData()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$heatMonth = renderUI({
    pickerInput(
      inputId = "heatMonth",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$heatDay = renderUI({
    pickerInput(
      inputId = "heatDay",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$heatPlot = renderPlot({
    req(input$heatYear)
    filterHeatMapData <-
      selectByDate(
        heatUpdateData(),
        year = input$heatYear,
        month = input$heatMonth,
        day = input$heatDay
      )
    trendLevel(filterHeatMapData, pollutant = input$heatParameter)
  })
  
  # ----------------------------------
  # ----------------------------------
  # ----------------------------------
  
  heatUpdateData2 <- reactive({
    dataset(input$heatDataset2)
  })
  
  output$heatParameter2 <- renderUI({
    pickerInput(
      inputId = "heatParameter2",
      label = "Parameter",
      choices = names(select_if(heatUpdateData2(), is.numeric))
    )
  })
  
  output$heatYear2 = renderUI({
    pickerInput(
      inputId = "heatYear2",
      label = "Year",
      choices = as.numeric(unique(
        format(heatUpdateData2()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(heatUpdateData2()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$heatMonth2 = renderUI({
    pickerInput(
      inputId = "heatMonth2",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$heatDay2 = renderUI({
    pickerInput(
      inputId = "heatDay2",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$heatPlot2 = renderPlot({
    req(input$heatYear2)
    filterHeatMapData2 <-
      selectByDate(
        heatUpdateData2(),
        year = input$heatYear2,
        month = input$heatMonth2,
        day = input$heatDay2
      )
    trendLevel(filterHeatMapData2, pollutant = input$heatParameter2)
  })
  
  # ----------------------------------
  # tab panel 2H - Calendar Plot
  
  calendarUpdateData <- reactive({
    dataset(input$calendarDataset)
  })
  
  output$calendarParameter <- renderUI({
    pickerInput(
      inputId = "calendarParameter",
      label = "Parameter",
      choices = names(select_if(calendarUpdateData(), is.numeric))
    )
  })
  
  output$calendarYear = renderUI({
    pickerInput(
      inputId = "calendarYear",
      label = "Year",
      choices = as.numeric(unique(
        format(calendarUpdateData()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(calendarUpdateData()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$calendarMonth = renderUI({
    pickerInput(
      inputId = "calendarMonth",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$calendarDay = renderUI({
    pickerInput(
      inputId = "calendarDay",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$calendarPlot = renderPlot({
    req(input$calendarYear)
    filtercalendarData <-
      selectByDate(
        calendarUpdateData(),
        year = input$calendarYear,
        month = input$calendarMonth,
        day = input$calendarDay
      )
    calendarPlot(filtercalendarData, pollutant = input$calendarParameter)
  })
  
  # ----------------------------------
  # ----------------------------------
  # ----------------------------------
  
  calendarUpdateData2 <- reactive({
    dataset(input$calendarDataset2)
  })

  output$calendarParameter2 <- renderUI({
    pickerInput(
      inputId = "calendarParameter2",
      label = "Parameter",
      choices = names(select_if(calendarUpdateData2(), is.numeric))
    )
  })
  
  output$calendarYear2 = renderUI({
    pickerInput(
      inputId = "calendarYear2",
      label = "Year",
      choices = as.numeric(unique(
        format(calendarUpdateData2()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(calendarUpdateData2()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$calendarMonth2 = renderUI({
    pickerInput(
      inputId = "calendarMonth2",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$calendarDay2 = renderUI({
    pickerInput(
      inputId = "calendarDay2",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$calendarPlot2 = renderPlot({
    req(input$calendarYear2)
    filtercalendarData2 <-
      selectByDate(
        calendarUpdateData2(),
        year = input$calendarYear2,
        month = input$calendarMonth2,
        day = input$calendarDay2
      )
    calendarPlot(filtercalendarData2, pollutant = input$calendarParameter2)
  })
  
  # ----------------------------------
  # tab panel 2I - Theil-Sen Trends
  
  theilSenUpdateData <- reactive({
    dataset(input$theilSenDataset)
  })

  output$theilSenParameter <- renderUI({
    pickerInput(
      inputId = "theilSenParameter",
      label = "Parameter",
      choices = names(select_if(theilSenUpdateData(), is.numeric))
    )
  })
  
  output$theilSenYear = renderUI({
    pickerInput(
      inputId = "theilSenYear",
      label = "Year",
      choices = as.numeric(unique(
        format(theilSenUpdateData()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(theilSenUpdateData()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$theilSenMonth = renderUI({
    pickerInput(
      inputId = "theilSenMonth",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$theilSenDay = renderUI({
    pickerInput(
      inputId = "theilSenDay",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$theilSenPlot = renderPlot({
    req(input$theilSenYear)
    filtertheilSenData <-
      selectByDate(
        theilSenUpdateData(),
        year = input$theilSenYear,
        month = input$theilSenMonth,
        day = input$theilSenDay
      )
    TheilSen(
      filtertheilSenData,
      pollutant = input$theilSenParameter,
      date.format = "%Y"
    )
  })
  
  # ----------------------------------
  # ----------------------------------
  # ----------------------------------
  
  theilSenUpdateData2 <- reactive({
    dataset(input$theilSenDataset2)
  })

  output$theilSenParameter2 <- renderUI({
    pickerInput(
      inputId = "theilSenParameter2",
      label = "Parameter",
      choices = names(select_if(theilSenUpdateData2(), is.numeric))
    )
  })
  
  output$theilSenYear2 = renderUI({
    pickerInput(
      inputId = "theilSenYear2",
      label = "Year",
      choices = as.numeric(unique(
        format(theilSenUpdateData2()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(theilSenUpdateData2()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$theilSenMonth2 = renderUI({
    pickerInput(
      inputId = "theilSenMonth2",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$theilSenDay2 = renderUI({
    pickerInput(
      inputId = "theilSenDay2",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$theilSenPlot2 = renderPlot({
    req(input$theilSenYear2)
    filtertheilSenData2 <-
      selectByDate(
        theilSenUpdateData2(),
        year = input$theilSenYear2,
        month = input$theilSenMonth2,
        day = input$theilSenDay2
      )
    TheilSen(
      filtertheilSenData2,
      pollutant = input$theilSenParameter2,
      date.format = "%Y"
    )
  })
  
  # ----------------------------------
  # tab panel 2J - Smooth Trends
  
  smoothUpdateData <- reactive({
    dataset(input$smoothDataset)
  })

  output$smoothParameter <- renderUI({
    pickerInput(
      inputId = "smoothParameter",
      label = "Parameter",
      choices = names(select_if(smoothUpdateData(), is.numeric))
    )
  })
  
  output$smoothYear = renderUI({
    pickerInput(
      inputId = "smoothYear",
      label = "Year",
      choices = as.numeric(unique(
        format(smoothUpdateData()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(smoothUpdateData()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$smoothMonth = renderUI({
    pickerInput(
      inputId = "smoothMonth",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$smoothDay = renderUI({
    pickerInput(
      inputId = "smoothDay",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$smoothPlot = renderPlot({
    req(input$smoothYear,)
    filtersmoothData <-
      selectByDate(
        smoothUpdateData(),
        year = input$smoothYear,
        month = input$smoothMonth,
        day = input$smoothDay
      )
    smoothTrend(filtersmoothData, pollutant = input$smoothParameter)
  })
  
  # ----------------------------------
  # ----------------------------------
  # ----------------------------------
  
  smoothUpdateData2 <- reactive({
    dataset(input$smoothDataset2)
  })

  output$smoothParameter2 <- renderUI({
    pickerInput(
      inputId = "smoothParameter2",
      label = "Parameter",
      choices = names(select_if(smoothUpdateData2(), is.numeric))
    )
  })
  
  output$smoothYear2 = renderUI({
    pickerInput(
      inputId = "smoothYear2",
      label = "Year",
      choices = as.numeric(unique(
        format(smoothUpdateData2()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(smoothUpdateData2()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$smoothMonth2 = renderUI({
    pickerInput(
      inputId = "smoothMonth2",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$smoothDay2 = renderUI({
    pickerInput(
      inputId = "smoothDay2",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$smoothPlot2 = renderPlot({
    req(input$smoothYear2)
    filtersmoothData2 <-
      selectByDate(
        smoothUpdateData2(),
        year = input$smoothYear2,
        month = input$smoothMonth2,
        day = input$smoothDay2
      )
    smoothTrend(filtersmoothData2, pollutant = input$smoothParameter2)
  })
  
  # ----------------------------------
  # tab panel 2K - Scatter Plot
  
  scatterUpdateData <- reactive({
    dataset(input$scatterDataset)
  })

  output$scatterParameterX <- renderUI({
    pickerInput(
      inputId = "scatterParameterX",
      label = "Parameter X",
      choices = names(select_if(scatterUpdateData(), is.numeric))
    )
  })
  
  output$scatterParameterY <- renderUI({
    pickerInput(
      inputId = "scatterParameterY",
      label = "Parameter Y",
      choices = names(select_if(scatterUpdateData(), is.numeric))
    )
  })
  
  output$scatterYear = renderUI({
    pickerInput(
      inputId = "scatterYear",
      label = "Year",
      choices = as.numeric(unique(
        format(scatterUpdateData()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(scatterUpdateData()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$scatterMonth = renderUI({
    pickerInput(
      inputId = "scatterMonth",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$scatterDay = renderUI({
    pickerInput(
      inputId = "scatterDay",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$scatterPlot = renderPlot({
    req(input$scatterYear)
    filterscatterData <-
      selectByDate(
        scatterUpdateData(),
        year = input$scatterYear,
        month = input$scatterMonth,
        day = input$scatterDay
      )
    scatterPlot(filterscatterData, x = input$scatterParameterX, y = input$scatterParameterY)
  })
  
  # ----------------------------------
  # ----------------------------------
  # ----------------------------------
  
  scatterUpdateData2 <- reactive({
    dataset(input$scatterDataset2)
  })

  output$scatterParameterX2 <- renderUI({
    pickerInput(
      inputId = "scatterParameterX2",
      label = "Parameter X",
      choices = names(select_if(scatterUpdateData2(), is.numeric))
    )
  })
  
  output$scatterParameterY2 <- renderUI({
    pickerInput(
      inputId = "scatterParameterY2",
      label = "Parameter Y",
      choices = names(select_if(scatterUpdateData2(), is.numeric))
    )
  })
  
  output$scatterYear2 = renderUI({
    pickerInput(
      inputId = "scatterYear2",
      label = "Year",
      choices = as.numeric(unique(
        format(scatterUpdateData2()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(scatterUpdateData2()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$scatterMonth2 = renderUI({
    pickerInput(
      inputId = "scatterMonth2",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$scatterDay2 = renderUI({
    pickerInput(
      inputId = "scatterDay2",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$scatterPlot2 = renderPlot({
    req(input$scatterYear2)
    filterscatterData2 <-
      selectByDate(
        scatterUpdateData2(),
        year = input$scatterYear2,
        month = input$scatterMonth2,
        day = input$scatterDay2
      )
    scatterPlot(filterscatterData2, x = input$scatterParameterX2, y = input$scatterParameterY2)
  })
  
  # ----------------------------------
  # tab panel 2L - Correlation Matrices
  
  correlationUpdateData <- reactive({
    dataset(input$correlationDataset)
  })

  output$correlationParameter <- renderUI({
    pickerInput(
      inputId = "correlationParameter",
      label = "Parameter",
      choices = names(select_if(correlationUpdateData(), is.numeric))
    )
  })
  
  output$correlationYear = renderUI({
    pickerInput(
      inputId = "correlationYear",
      label = "Year",
      choices = as.numeric(unique(
        format(correlationUpdateData()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(correlationUpdateData()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$correlationMonth = renderUI({
    pickerInput(
      inputId = "correlationMonth",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$correlationDay = renderUI({
    pickerInput(
      inputId = "correlationDay",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$correlationPlot = renderPlot({
    req(input$correlationYear)
    filtercorrelationData <-
      selectByDate(
        correlationUpdateData(),
        year = input$correlationYear,
        month = input$correlationMonth,
        day = input$correlationDay
      )
    corPlot(filtercorrelationData, dendrogram = TRUE)
    
  })
  
  # ----------------------------------
  # ----------------------------------
  # ----------------------------------
  
  correlationUpdateData2 <- reactive({
    dataset(input$correlationDataset2)
  })

  output$correlationParameter2 <- renderUI({
    pickerInput(
      inputId = "correlationParameter2",
      label = "Parameter",
      choices = names(select_if(correlationUpdateData2(), is.numeric))
    )
  })
  
  output$correlationYear2 = renderUI({
    pickerInput(
      inputId = "correlationYear2",
      label = "Year",
      choices = as.numeric(unique(
        format(correlationUpdateData2()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(correlationUpdateData2()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$correlationMonth2 = renderUI({
    pickerInput(
      inputId = "correlationMonth2",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$correlationDay2 = renderUI({
    pickerInput(
      inputId = "correlationDay2",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$correlationPlot2 = renderPlot({
    req(input$correlationYear2)
    filtercorrelationData2 <-
      selectByDate(
        correlationUpdateData2(),
        year = input$correlationYear2,
        month = input$correlationMonth2,
        day = input$correlationDay2
      )
    corPlot(filtercorrelationData2, dendrogram = TRUE)
  })
  
  # ----------------------------------
  # tab panel 2M - Temporal Variation Plots
  
  tempVarUpdateData <- reactive({
    dataset(input$tempVarDataset)
  })

  output$tempVarParameter <- renderUI({
    pickerInput(
      inputId = "tempVarParameter",
      label = "Parameter",
      choices = names(select_if(tempVarUpdateData(), is.numeric)),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      multiple = TRUE,
      inline = FALSE,
      selected = "BC.370."
    )
  })
  
  output$tempVarYear = renderUI({
    c <- function() {
      return(as.numeric(unique(
        format(tempVarUpdateData()$date, format = "%Y")
      )))
    }
    
    pickerInput(
      inputId = "tempVarYear",
      label = "Year",
      choices = c(),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = c(),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$tempVarMonth = renderUI({
    pickerInput(
      inputId = "tempVarMonth",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$tempVarDay = renderUI({
    pickerInput(
      inputId = "tempVarDay",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$tempVarPlot = renderPlot({
    req(input$tempVarYear)
    filtertempVarData <-
      selectByDate(
        tempVarUpdateData(),
        year = input$tempVarYear,
        month = input$tempVarMonth,
        day = input$tempVarDay
      )
    timeVariation(filtertempVarData, pollutant = input$tempVarParameter)
  })
  
  # ----------------------------------
  # ----------------------------------
  # ----------------------------------
  
  tempVarUpdateData2 <- reactive({
    dataset(input$tempVarDataset2)
  })

  output$tempVarParameter2 <- renderUI({
    pickerInput(
      inputId = "tempVarParameter2",
      label = "Parameter",
      choices = names(select_if(tempVarUpdateData2(), is.numeric)),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      multiple = TRUE,
      inline = FALSE,
      selected = "BC.370."
    )
  })
  
  output$tempVarYear2 = renderUI({
    pickerInput(
      inputId = "tempVarYear2",
      label = "Year",
      choices = as.numeric(unique(
        format(tempVarUpdateData2()$date, format = "%Y")
      )),
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3",
      ),
      selected = as.numeric(unique(
        format(tempVarUpdateData2()$date, format = "%Y")
      )),
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$tempVarMonth2 = renderUI({
    pickerInput(
      inputId = "tempVarMonth2",
      label = "Month",
      choices = month_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = month_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$tempVarDay2 = renderUI({
    pickerInput(
      inputId = "tempVarDay2",
      label = "Day",
      choices = day_list,
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 3"
      ),
      selected = day_list,
      multiple = TRUE,
      inline = FALSE
    )
  })
  
  output$tempVarPlot2 = renderPlot({
    req(input$tempVarYear2)
    filtertempVarData2 <-
      selectByDate(
        tempVarUpdateData2(),
        year = input$tempVarYear2,
        month = input$tempVarMonth2,
        day = input$tempVarDay2
      )
    timeVariation(filtertempVarData2, pollutant = input$tempVarParameter2)
  })
})
