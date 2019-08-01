# created own model using FluView_LineChart_DataCSV.csv

# Step 1: import data 
# save FluView_LineChart_DataCSV.csv to Desktop
# run lines 7-9 to import FluView_LineChart_DataCSV.csv to R

help(read.csv)
data1 <- read.csv(file.choose(), header=TRUE)
data1 

# Step 2: proceed with SARIMATD1Model 
library(ForecastFramework)
library(R6)
library(forecast)
library(sarimaTD)

# SARIMATDModel Description:
# Implementation of the sarimaTD model created by Evan Ray
# More information about sarimaTD in https://github.com/reichlab/sarimaTD 
# Integrated into ForecastFramework by: Katie House, 6/22/2018


SARIMATD1Model <- R6Class(
  inherit = ContestModel,
  private = list(
    .data1 = NULL,        ## every model should have this
    .models = list(),    ## specific to models that are fit separately for each location
    .nsim = 1000,        ## models that are simulating forecasts need this
    .period = integer(0) ## specific to SARIMA models
  ),
  public = list(
    ## data will be MatrixData
    fit = function(data1) {
      if("fit" %in% private$.debug){browser()}
      ## stores data for easy access and checks to make sure it's the right class
      private$.data <- IncidenceMatrix$new(data)
      
      ## for each location/row
      for (row_idx in 1:private$.data$nrow) {
        ### need to create a y vector with incidence at time t
        y <- private$.data$subset(rows = row_idx, mutate = FALSE)
        
        ## convert y vector to time series data type
        y_ts <- ts(as.vector(y$mat), frequency = private$.period)
        
        ## fit sarimaTD with 'fit_sarima()' from sarimaTD package
        ## fit_sarima() performs box-cox transformation and seasonal differencing
        private$.models[[row_idx]] <- fit_sarima(y = y_ts,
                                                 transformation = "box-cox",
                                                 seasonal_difference = TRUE)
      }
    },
    forecast = function(newdata1 = private$.data1, steps) {
      ## include for debugging
      if("forecast" %in% private$.debug){browser()} 
      
      ## number of models (provinces) to forecast
      nmodels <- length(private$.models)
      
      ## define an array to store the simulated forecasts
      sim_forecasts <- array(dim = c(nmodels, steps, private$.nsim))
      dimnames(sim_forecasts) <- list(newdata$rnames, 1:steps, NULL)
      
      ## iterate through each province and forecast with simulate.satimaTD
      for(model_idx in 1:length(private$.models)) {
        tmp_arima <-  simulate(object = private$.models[[model_idx]],
                               nsim = private$.nsim,
                               seed = 1,
                               newdata = as.vector(newdata$mat[model_idx,]),
                               h = steps
        )
        ## transpose simulate() output to be consistent with ForecastFramework
        tmp_arima <- t(tmp_arima)
        sim_forecasts[model_idx, , ] <- tmp_arima
      }
      private$output <- SimulatedIncidenceMatrix$new(sim_forecasts)
      return(IncidenceForecast$new(private$output, forecastTimes = rep(TRUE, steps)))
    },
    initialize = function(period = 26, nsim=1000) { 
      ## this code is run during SARIMAModel$new()
      ## need to store these arguments within the model object
      private$.nsim <- nsim
      private$.period <- period
    }
  )
)

data1

# Step 3: test the new model 

library(ForecastFramework)
library(R6)
library(forecast)
library(dplyr)
library(ggplot2)
library(cdcfluview)

# Source R6 Files
library(RCurl)

data1
# Function of Source Github Models
source_github <- function(data1) {
  script <- getURL(data1, ssl.verifypeer = FALSE)
  eval(parse(text = script),envir=.GlobalEnv)
}  

# Source R6 Files
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/ContestModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/SARIMATD1Model.R')

data1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Skip process below because not pertain to our data1 from FluView_LineChart_DataCSV.csv
# Preprocess data to Inc Matrix
# data1 <- ilinet(region = "National")
# data1 <- data1 %>% filter( week != 53)
# data1 <- data1 %>%
select('year','week','weighted_ili')

# preprocess_inc <- function(dat){
# dat$time.in.year = dat$week
# dat$t = dat$year + dat$week/52
# inc = ObservationList$new(dat)
# inc$formArray('region','t',val='weighted_ili',
#    dimData = list(NULL,list('week','year','time.in.year','t')),
#   metaData = list(t.step = 1/52,max.year.time = 52))
#  return(inc)
# }
# training_data <- data1 %>% 
#  filter( ! ((year == 2018 & week >= 40)| (year == 2019 ) ))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Create new SARIMATD model
nsim <- 10 # Number of SARIMA simulations 
sarimaTD_model <- SARIMATD1Model$new(period = 42, nsim = nsim)

# Fit SARIMATD model (takes long time)
sarimaTD_model$fit(training_inc)

# Forecast SARIMATD Model
steps <- 42 # forecast ahead 26 biweeks
forecast_X <- sarimaTD_model$forecast(steps = steps)

print(forecast_X$data$mat)

# should yield accurate forecasts