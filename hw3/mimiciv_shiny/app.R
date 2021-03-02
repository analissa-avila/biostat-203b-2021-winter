# Load packages ----
library(shiny)
library(tidyverse)
library(ggplot2)

# Data source ----
icu_cohort = readRDS("icu_cohort.rds")

# User interface ----
ui <- fluidPage(
  titlePanel("MIMIC-IV ICU Stays Data Explorer"),
  
  tabsetPanel(
    tabPanel("Demographics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("varDemo",
                             label = "Demographic variable:",
                             choices = c("age at admission" = "age_at_adm", 
                                         "gender", "language", 
                                         "marital status" = "marital_status", 
                                         "ethnicity", "insurance", "died", 
                                         "died in 30 days" = "died_in_30days"),
                             selected = "age_at_adm"),
                 conditionalPanel("input.varDemo == 'age_at_adm'",
                                  sliderInput("bins", "Number of bins", min = 1,
                                              max = 100, value = 30)
                 )
               ),
               mainPanel(
                 # Output: Tabset w/ histogram and table ----
                 tabsetPanel(type = "tabs",
                             tabPanel("Plot", 
                                      plotOutput("plotDemo")),
                             tabPanel("Summary Statistics", 
                                      tableOutput("tableDemo"),
                                      textOutput("missingDemo"))
                             )
                 )
               )
             ),
    tabPanel("Lab Measurements",
             sidebarLayout(
               sidebarPanel(
                 selectInput("varLabs", 
                             label = "Lab measurement:",
                             choices = c("bicarbonate", "calcium", "chloride", 
                                         "creatinine", "glucose", "magnesium",
                                         "potassium", "sodium", "hematocrit",
                                         "wbc", "lactate"),
                             selected = "bicarbonate")
               ),
               mainPanel(
                 # Output: Tabset w/ boxplot, histogram, and table ----
                 tabsetPanel(type = "tabs",
                             tabPanel("Boxplot",
                                      plotOutput("boxLab")),
                             tabPanel("Histogram", 
                                      plotOutput("histLab"),
                                      uiOutput("sliderLab")),
                             tabPanel("Summary Statistics", 
                                      tableOutput("tableLab"),
                                      textOutput("missingLab"))
                             )
                 )
               )
             ),
    tabPanel("Vital Measurements",
             sidebarLayout(
               sidebarPanel(
                 selectInput("varVitals",
                             label = "Vital measurement:",
                             choices = c("heart rate" = "heart_rate",
                             "non-invasive blood pressure, systolic" =
                               "non_invasive_blood_pressure_systolic",
                             "non-invasive blood pressure, mean" = 
                               "non_invasive_blood_pressure_mean",
                             "respiratory rate" = "respiratory_rate",
                             "temperature, fahrenheit" = 
                               "temperature_fahrenheit",
                             "arterial blood pressure, systolic" = 
                               "arterial_blood_pressure_systolic",
                             "arterial blood pressure, mean" = 
                               "arterial_blood_pressure_mean"),
                             selected = "heart_rate")
               ),
               mainPanel(
                 # Output: Tabset w/ boxplot, histogram, and table ----
                 tabsetPanel(type = "tabs",
                             tabPanel("Boxplot", 
                                      plotOutput("boxVital")),
                             tabPanel("Histogram", 
                                      plotOutput("histVital"),
                                      uiOutput("sliderVital")),
                             tabPanel("Summary Statistics", 
                                      tableOutput("tableVital"),
                                      textOutput("missingVital"))
                             )
                 )
               )
             )
    )
  )
# Server logic ----
server <- function(input, output) {
  
  # avoid plot axes being formatted in scientific notation
  options(scipen=999)
  
  ## ---- DEMOGRAPHIC VARIABLES ---- ##
  # Reactive expression to generate data frame for requested DEMOGRAPHIC variable ----
  dataDemo <- reactive({
    if (input$varDemo == "age_at_adm") {
      dat <- data.frame(varDemo = icu_cohort[[input$varDemo]])
    }
    else if (input$varDemo == "died" | input$varDemo == "died_in_30days"){
      dat <- data.frame(varDemo = factor(icu_cohort[[input$varDemo]], 
                                         labels = c("No", "Yes")))
    }
    else {
      dat <- data.frame(varDemo = factor(icu_cohort[[input$varDemo]]))
    }
    dat
  })
  
  # labels for each DEMOGRAPHIC variable
  labelDemo <- reactive({
    lab <- switch(input$varDemo,
                  "age_at_adm" = "age at admission",
                  "gender" = "gender",
                  "language" = "language",
                  "marital_status" = "marital status",
                  "ethnicity" = "ethnicity",
                  "insurance" = "insurance",
                  "died" = "died",
                  "died_in_30days" = "died in 30 days")
  })
    
  # Generate histogram/ bar plot for each DEMOGRAPHIC variable ----
  output$plotDemo <- renderPlot({
    # if age at admission, create histogram with user defined #bins
    if(input$varDemo == "age_at_adm"){
      ggplot(data = dataDemo()) +
        geom_histogram(mapping = aes(x = varDemo), bins = input$bins, 
                       fill = "#75AADB", color = "white") +
        xlab(labelDemo())
    }
    # if ethnicity create horizontal bar graph
    else if(input$varDemo == "ethnicity") {
      ggplot(data = dataDemo()) +
        geom_bar(mapping = aes(y = varDemo),
                 fill = "#75AADB", color = "white") +
        ylab(labelDemo())
    }
    # for all other variables create bar graph
    else {
      ggplot(data = dataDemo()) +
        geom_bar(mapping = aes(x = varDemo),
                 fill = "#75AADB", color = "white") +
        xlab(labelDemo())
    }
  })

  # Generate a table of summary statistics for each DEMOGRAPHIC variable ----
  output$tableDemo <- renderTable({
    # if age at admission, include mean, median, etc.
    if(input$varDemo == "age_at_adm"){
      dataDemo() %>%
        summarise(min = round(min(varDemo), 2),
                  Q1 = round(quantile(varDemo, .25), 2),
                  mean = round(mean(varDemo), 2),
                  median = round(median(varDemo), 2),
                  Q3 = round(quantile(varDemo, .75), 2),
                  max = round(max(varDemo), 2))
    }
    # for all other variables, frequency table
    else{
      dataDemo() %>%
        group_by(varDemo) %>%
        summarise(count = n()) %>%
        mutate(percent = round(count/sum(count)*100, 2)) %>%
        rename(value = varDemo)
    }
  })
  
  # Generate text denoting number of missing values for each DEMOGRAPHIC variable ----
  output$missingDemo <- renderText({
    paste("Number missing:", sum(is.na(dataDemo())))
  })
  
  
  ## ---- LAB EVENTS ---- ##
  # Reactive expression to generate data frame for requested LAB EVENT ----
  dataLab <- reactive({
    dat <- data.frame(varLab = icu_cohort[[input$varLabs]])
    dat
  })
  
  # Generate box plot for each LAB EVENT ----
  output$boxLab <- renderPlot({
    ggplot(data = dataLab()) +
      geom_boxplot(mapping = aes(y = varLab),
                     colour = "#75AADB") +
      ylab(input$varLabs)
  })
  
  # Generate slider for LAB EVENT histograms
  output$sliderLab <- renderUI({
    sliderInput("binsLab", "Number of bins", min = 1, max = 100, value = 30)
  })
  # Generate histogram for each LAB EVENT ----
  output$histLab <- renderPlot({
    ggplot(data = dataLab()) +
      geom_histogram(mapping = aes(x = varLab), bins = input$binsLab,
                     fill = "#75AADB", color = "white") +
      xlab(input$varLabs)

  })
  
  # Generate a table of summary statistics for each LAB EVENT ----
  output$tableLab <- renderTable({
    dataLab() %>%
      summarise(min = round(min(varLab, na.rm = TRUE), 2),
                Q1 = round(quantile(varLab, .25, na.rm = TRUE), 2),
                mean = round(mean(varLab, na.rm = TRUE), 2),
                median = round(median(varLab, na.rm = TRUE), 2),
                Q3 = round(quantile(varLab, .75, na.rm = TRUE), 2),
                max = round(max(varLab, na.rm = TRUE), 2))

  })
  
  # Generate text denoting number of missing values for each LAB EVENT ----
  output$missingLab <- renderText({
    paste("Number missing:", sum(is.na(dataLab())))
  })
  
  
  ## ---- CHART EVENTS (vitals) ---- ##
  # Reactive expression to generate data frame for requested CHART EVENT ----
  dataVital <- reactive({
    dat <- data.frame(varVital = icu_cohort[[input$varVitals]])
    dat
  })
  
  # labels for each CHART EVENT
  labelVital <- reactive({
    lab <- switch(input$varVitals,
                  "heart_rate" = "heart rate",
                  "non_invasive_blood_pressure_systolic" = 
                    "non-invasive blood pressure, systolic",
                  "non_invasive_blood_pressure_mean" = 
                    "non-invasive blood pressure, mean",
                  "respiratory_rate" = "respiratory rate",
                  "temperature_fahrenheit" = "temperature, fahrenheit",
                  "arterial_blood_pressure_systolic" = 
                    "arterial blood pressure, systolic",
                  "arterial_blood_pressure_mean" = 
                    "arterial blood pressure, mean")
  })
  
  
  # Generate box plot for each CHART EVENT ----
  output$boxVital <- renderPlot({
    ggplot(data = dataVital()) +
      geom_boxplot(mapping = aes(y = varVital),
                   colour = "#75AADB") +
      ylab(labelVital())
  })
  
  # Generate slider for CHART EVENT histograms
  output$sliderVital <- renderUI({
    sliderInput("binsVital", "Number of bins", min = 1, max = 100, value = 30)
  })
  # Generate histogram for each CHART EVENT ----
  output$histVital <- renderPlot({
    ggplot(data = dataVital()) +
      geom_histogram(mapping = aes(x = varVital), bins = input$binsVital,
                     fill = "#75AADB", color = "white") +
      xlab(labelVital())
    
  })
  
  # Generate a table of summary statistics for each CHART EVENT ----
  output$tableVital <- renderTable({
    dataVital() %>%
      summarise(min = round(min(varVital, na.rm = TRUE), 2),
                Q1 = round(quantile(varVital, .25, na.rm = TRUE), 2),
                mean = round(mean(varVital, na.rm = TRUE), 2),
                median = round(median(varVital, na.rm = TRUE), 2),
                Q3 = round(quantile(varVital, .75, na.rm = TRUE), 2),
                max = round(max(varVital, na.rm = TRUE), 2))
    
  })
  
  # Generate text denoting number of missing values for each CHART EVENT ----
  output$missingVital <- renderText({
    paste("Number missing:", sum(is.na(dataVital())))
  })
}

# Run app ----
shinyApp(ui, server)
