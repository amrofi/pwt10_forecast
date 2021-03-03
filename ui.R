library(shiny)
library(rsconnect)
library(tidyverse)
library(tidymodels)
library(lubridate)
library(forecast)
library(fpp3)
library(pwt10)
options(scipen=999)
Sys.setenv(LANG = "en")


countries_isocode <- c("ARG", "BRA", "CHL", "COL", "MEX", "VEN")
countries <- c("Argentina", "Brazil", "Chile", "Colombia", "Mexico", "Venezuela")


shinyUI(fluidPage(
    titlePanel("Time Series Forecasting Application"),
    h5("Forecast the output-side real GDP at chained PPPs (in million 2017 USD) with Penn World Table 10.0 data."),
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Select a country:", countries, "Brazil"),
            checkboxGroupInput("models", "Select time series models to evaluate:",
                               choiceNames = list("Holt's Trend Method", "ETS", "ARIMA", "TBATS"),
                               choiceValues = list("holt", "ets", "arima", "tbats"),
                               selected = c("ets", "arima")),
            submitButton("Update View", icon("refresh")),
            h6("This application allows you to forecast the 'rgdpo' (output-side real GDP at chained PPPs 
               in million 2017 USD) variable from the Penn World Table 10.0 for 6 Latin American countries
               for the period 2020-2029. You may select a country and a unique combination of time series models
               to evaluate which model is the best: i.e., the one with the lowest error in the 'test set'.
               The first plot shows forecasts for all selected models from the test set onwards; below, you will 
               find the RMSE and MAPE to measure the error in the test set, and the best model according to the MAPE.
               Finally, the last boxes show the forecast with its prediction intervals according to the best model.")
        ),
        mainPanel(
            plotOutput("all_models_plot"),
            verbatimTextOutput("results"),
            plotOutput("best_model_plot"),
            verbatimTextOutput("best_model_forecast")
        )
    )
))