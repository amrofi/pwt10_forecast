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


shinyServer(function(input, output) {
    
    countries_isocode <- c("ARG", "BRA", "CHL", "COL", "MEX", "VEN")
    countries <- c("Argentina", "Brazil", "Chile", "Colombia", "Mexico", "Venezuela")
    
    ts_models <- function(selected_country) {
        df <- pwt10.0 %>% 
            as_tibble() %>%
            filter(isocode %in% countries_isocode) %>% 
            dplyr::select(year, country, rgdpo) %>% 
            spread(key =
                       country, value = rgdpo) %>% 
            rename(Venezuela = `Venezuela (Bolivarian Republic of)`) %>% 
            dplyr::select(year, selected_country)
        
        ts <- ts(df[,2], freq = 1, start = c(1950), end = c(2019))
        
        if(selected_country == "Chile") {
            ts <- na.interp(ts)
        }
        
        train <- window(ts, end = c(2014))
        h <- length(ts) - length(train)
        
        
        
        holt_model <- holt(train, h = 11)
        holt_forecast <- forecast(holt_model, h = h)
        holt_forecast_11 <- forecast(holt_model, h = 11)
        holt_model_full <- holt(ts, h = 10)
        holt_forecast_full <- forecast(holt_model_full, h = 10)
        
        ets_model <- ets(train)
        ets_forecast <- forecast(ets_model, h = h)
        ets_forecast_11 <- forecast(ets_model, h = 11)
        ets_model_full <- ets(ts)
        ets_forecast_full <- forecast(ets_model_full, h = 10)
        
        arima_model <- auto.arima(train)
        arima_forecast <- forecast(arima_model, h = h)
        arima_forecast_11 <- forecast(arima_model, h = 11)
        arima_model_full <- auto.arima(ts)
        arima_forecast_full <- forecast(arima_model_full, h = 10)
        
        tbats_model <- tbats(train)
        tbats_forecast <- forecast(tbats_model, h = h)
        tbats_forecast_11 <- forecast(tbats_model, h = 11)
        tbats_model_full <- tbats(ts)
        tbats_forecast_full <- forecast(tbats_model_full, h = 10)
        
        return(list(ts = ts,
                    holt_forecast = holt_forecast,
                    holt_forecast_11 = holt_forecast_11,
                    holt_forecast_full = holt_forecast_full,
                    ets_forecast = ets_forecast,
                    ets_forecast_11 = ets_forecast_11,
                    ets_forecast_full = ets_forecast_full,
                    arima_forecast = arima_forecast,
                    arima_forecast_11 = arima_forecast_11,
                    arima_forecast_full = arima_forecast_full,
                    tbats_forecast = tbats_forecast,
                    tbats_forecast_11 = tbats_forecast_11,
                    tbats_forecast_full = tbats_forecast_full))
    }

    ts_plot <- function(selected_country, models_list) {
        g <- ts_models(selected_country)
        `Output-side real GDP at chained PPPs (in million 2017 USD)` <- g$ts
        
        p <- autoplot(`Output-side real GDP at chained PPPs (in million 2017 USD)`)
        if ("holt" %in% models_list) p <- p + autolayer(g$holt_forecast_11, series = "HOLT", PI = FALSE)
        if ("ets" %in% models_list) p <- p + autolayer(g$ets_forecast_11, series = "ETS", PI = FALSE)
        if ("arima" %in% models_list) p <- p + autolayer(g$arima_forecast_11, series = "ARIMA", PI = FALSE)
        if ("tbats" %in% models_list) p <- p + autolayer(g$tbats_forecast_11, series = "TBATS", PI = FALSE) 
        
        return(p)
    }
    
    ts_results <- function(selected_country, models_list) {
        g <- ts_models(selected_country)
        
        RMSE <- MAPE <- vector("numeric")
        
        if ("holt" %in% models_list) RMSE <- append(RMSE, c(HOLT = accuracy(g$holt_forecast, g$ts)["Test set","RMSE"]))
        if ("ets" %in% models_list) RMSE <- append(RMSE, c(ETS = accuracy(g$ets_forecast, g$ts)["Test set","RMSE"]))
        if ("arima" %in% models_list) RMSE <- append(RMSE, c(ARIMA = accuracy(g$arima_forecast, g$ts)["Test set","RMSE"]))
        if ("tbats" %in% models_list) RMSE <- append(RMSE, c(TBATS = accuracy(g$tbats_forecast, g$ts)["Test set","RMSE"]))
        
        if ("holt" %in% models_list) MAPE <- append(MAPE, c(HOLT = accuracy(g$holt_forecast, g$ts)["Test set","MAPE"]))
        if ("ets" %in% models_list) MAPE <- append(MAPE, c(ETS = accuracy(g$ets_forecast, g$ts)["Test set","MAPE"]))
        if ("arima" %in% models_list) MAPE <- append(MAPE, c(ARIMA = accuracy(g$arima_forecast, g$ts)["Test set","MAPE"]))
        if ("tbats" %in% models_list) MAPE <- append(MAPE, c(TBATS = accuracy(g$tbats_forecast, g$ts)["Test set","MAPE"]))
        
        df <- as.data.frame(rbind(RMSE, MAPE))
        
        return(df)
    }
    
    ts_best <- function(selected_country, models_list) {
        df <- ts_results(selected_country, models_list)
    
        best <- names(df)[order(df[2,])[1]]
        
        return(best)
    }
    
    ts_best_plot <- function(selected_country, models_list) {
        g <- ts_models(selected_country)

        best_model <- str_to_lower(ts_best(selected_country, models_list))

        return(switch(best_model,
                      holt = plot(g$holt_forecast_full),
                      ets = plot(g$ets_forecast_full),
                      arima = plot(g$arima_forecast_full),
                      tbats = plot(g$tbats_forecast_full)))
    }

    ts_best_forecast <- function(selected_country, models_list) {
        g <- ts_models(selected_country)

        best_model <- str_to_lower(ts_best(selected_country, models_list))

        return(switch(best_model,
                      holt = g$holt_forecast_full,
                      ets = g$ets_forecast_full,
                      arima = g$arima_forecast_full,
                      tbats = g$tbats_forecast_full))
    }
    
    
    
    
    
    output$all_models_plot <- renderPlot({
        ts_plot(input$country, input$models)
    })

    output$results <- renderPrint({
        print(paste("According to the MAPE, the best model is:", ts_best(input$country, input$models)))
        print("The final results are:")
        ts_results(input$country, input$models)
    })
    
    output$best_model_plot <- renderPlot({
        ts_best_plot(input$country, input$models)
    })
    
    output$best_model_forecast <- renderPrint({
        ts_best_forecast(input$country, input$models)
    })
})
