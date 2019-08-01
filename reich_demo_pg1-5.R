# Running Reich Lab demo pg 1-5
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#page 1. Getting started 

#install.packages("devtools")
#install.packages("usethis")
#library(devtools)
#install_github("")

install.packages("workflowr")
install.packages("ForecastFramework")
#library(devtools)
#devtools::install_github('HopkinsIDD/ForecastFramework')

install.packages(c('R6','devtools','forecast','ggplot2','gridExtra','data.table','knitr','kableExtra','RCurl'))
install.packages("dplyr", repos = "https://cloud.r-project.org")

library(devtools)
devtools::install_github('HopkinsIDD/ForecastFramework')
devtools::install_github('reichlab/sarimaTD')
devtools::install_github("hrbrmstr/cdcfluview")

#Forecast Framework Dependencies

library(ForecastFramework)
library(R6)
library(forecast)

#Data Dependencies
library(cdcfluview)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(data.table)
library(knitr)
library(kableExtra)

#Function of Source Github Models
source_github <- function(u) {
  library(RCurl)
  script <- getURL(u, ssl.verifypeer = FALSE)
  eval(parse(text = script),envir=.GlobalEnv)
}

#Source R6 Files
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/ContestModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/SARIMAModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/GamModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/SARIMATD1Model.R')

install.packages("remotes")
remotes::install_github("reichlab/sarimaTD")
install.packages("sarimaTD")
#I added 50-53 b/c R 03.05.01 has difficulty reading sarimaTD

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#page 2. Raw Data

library(cdcfluview)
library(dplyr)
dat <- ilinet(region = "National")
dat <- dat %>%
  select('region','year','week','weighted_ili', 'week_start')
print(head(dat,6))

#Time Series
library(ggplot2)
dat$date_sick <- as.Date(strptime(dat$week_start,"%m/%d/%Y")) # convert to date
plot <- ggplot() +
  geom_line(mapping = aes(x = week_start, y = weighted_ili),
            size=0.7,
            data = dat) +
  xlab("") + ylab("Weighted ILI") +
  coord_cartesian(ylim = c(0, 10)) +
  ggtitle("National Weighted Inluenza-Like Illness")
print(plot)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#page3. Incidence Matrix

library(R6)
library(ForecastFramework)
data_matrix <- matrix(1:9,3,3)
print(data_matrix)
data_object <- IncidenceMatrix$new(data_matrix)
print(data_object)

data_object$mat
data_object$nrow
data_object$ncol

data_object$colData <- list(1:3) # Initialize how many columns headers
data_object$colData <- list(c("A","B","C"))
data_object$colData

data_object$addColumns(2)
data_object$colData

data_object$mat

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#page4. Fitting & Forecasting
library(ForecastFramework)
library(R6)
library(forecast)
library(dplyr)
library(ggplot2)
library(cdcfluview)

#Source R6 Files
library(RCurl)
library(sarimaTD)

#Function of Soucre Github Models
source_github <- function(u) {
  # read script lines from website and evaluate
  script <- getURL(u, ssl.verifypeer = FALSE)
  eval(parse(text = script),envir=.GlobalEnv)
}  

# Source R6 Files
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/ContestModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/SARIMATD1Model.R')

#new sarimaTDmodel class
nsim <- 10 # Number of SARIMA simulations 
sarimaTDmodel <- SARIMATD1Model$new(period = 52, nsim = nsim)


dat <- ilinet(region = "National")
dat <- dat %>% 
  filter( week != 53 &
            year > 2009 & 
            year < 2018 & 
            !(year == 2017 & week > 18))
dat <- dat %>%
  select('region','year','week','weighted_ili', 'week_start')

# Create a Preprocess Function
preprocess_inc <- function(dat){
  dat$time.in.year = dat$week
  dat$t = dat$year + dat$week/52
  inc = ObservationList$new(dat)
  inc$formArray('region','t',val='weighted_ili',
                dimData = list(NULL,list('week','year','time.in.year','t')),
                metaData = list(t.step = 1/52,max.year.time = 52))
  return(inc)
}

# Seperate into Training and Testing Data
training_data <- dat %>% 
  filter( ! ((year == 2016 ) | (year == 2017 ) ))
testing_data <- dat %>% 
  filter( ! ((year == 2016 & week >= 19) | (year == 2017 & week <= 18)) )
truth_data <- dat %>% 
  filter( ((year == 2016 & week >= 19) | (year == 2017 & week <= 18)) )

training_inc <- preprocess_inc(training_data)
#Warning message:
#Unknown or uninitialised column: 'frame'.
testing_inc <- preprocess_inc(testing_data)
#Warning message:
#Unknown or uninitialised column: 'frame'.
truth_inc <- preprocess_inc(truth_data)
#Warning message:
#Unknown or uninitialised column: 'frame'.

print(training_inc$mat[,0:10])

sarimaTDmodel$fit(training_inc)


steps <- 52 # forecast ahead 52 weeks
forecast_X <- sarimaTDmodel$forecast(testing_inc,steps = steps)
#Adding missing grouping variables: `region`, `t`
#Adding missing grouping variables: `region`
#Adding missing grouping variables: `region`
#Adding missing grouping variables: `t`
forecast_X$median()$mat

# converting predictions to a dataframe to use dplyr
preds_df <- data.frame(as.table(t(forecast_X$data$mat)))

# converting predictions to a dataframe to use dplyr
# import testing dataset which has 2013 data

# add prediction dates to original forecast
preds_df[["date_sick"]] <-as.Date(truth_data$week_start, format = "%d-%m-%Y")

preds_df <- preds_df %>%
  mutate( 
    pred_total_cases = as.vector(forecast_X$median(na.rm=TRUE)$mat),
    pred_95_lb = as.vector(forecast_X$quantile(0.025,na.rm=TRUE)$mat),
    pred_95_ub = as.vector(forecast_X$quantile(0.975,na.rm=TRUE)$mat),
    pred_80_lb = as.vector(forecast_X$quantile(0.05,na.rm=TRUE)$mat),
    pred_80_ub = as.vector(forecast_X$quantile(0.95,na.rm=TRUE)$mat),
    pred_50_lb = as.vector(forecast_X$quantile(0.25,na.rm=TRUE)$mat),
    pred_50_ub = as.vector(forecast_X$quantile(0.75,na.rm=TRUE)$mat)
  )
print(head(preds_df,5)) # Print the first 5 rows


data_X_3years <- dat %>% filter(year > 2013)
ggplot(data=preds_df, aes(x=date_sick)) +
  geom_ribbon(
    mapping = aes(ymin = pred_95_lb, ymax = pred_95_ub),
    fill = "cornflowerblue",
    alpha = 0.2) +
  geom_ribbon(
    mapping = aes(ymin = pred_80_lb, ymax = pred_80_ub),
    fill = "cornflowerblue",
    alpha = 0.2) +
  geom_ribbon(
    mapping = aes(ymin = pred_50_lb, ymax = pred_50_ub),
    alpha = 0.2) +
  geom_line(
    mapping = aes(y = pred_total_cases),
    color = "royalblue",
    size = 2) +
  geom_line(aes(x = week_start, y = weighted_ili),
            size=0.7,
            data = data_X_3years) +
  xlab("") + ylab("Weighted ILI")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#page 5. Evaluating Multiple Modles
library(ForecastFramework)
library(R6)
library(forecast)
library(dplyr)
library(ggplot2)
library(cdcfluview)
library(gridExtra)
library(data.table)
library(knitr)
library(kableExtra)
library(RCurl)

# Function of Source Github Models
source_github <- function(u) {
  # read script lines from website and evaluate
  script <- getURL(u, ssl.verifypeer = FALSE)
  eval(parse(text = script),envir=.GlobalEnv)
}  
# Source R6 Files
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/ContestModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/AggregateForecastModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/SARIMAModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/GamModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/SARIMATD1Model.R')


# Seperate into Training and Testing Data
dat <- ilinet(region = "National")
dat <- dat %>% 
  filter( week != 53 &
            year > 2009 & 
            year < 2018 & 
            !(year == 2017 & week > 18))
dat <- dat %>%
  select('region','year','week','weighted_ili', 'week_start')

# Seperate into Training, Testing, and Truth Data
training_data <- dat %>% 
  filter( ! ((year == 2016 ) | (year == 2017 ) ))
testing_data <- dat %>% 
  filter( ! ((year == 2016 & week >= 19) | (year == 2017 & week <= 18)) )
truth_data <- dat %>% 
  filter( ((year == 2016 & week >= 19) | (year == 2017 & week <= 18)) )

head(testing_data,5)


# Create a Preprocess Function
preprocess_inc <- function(dat){
  dat$time.in.year = dat$week
  dat$t = dat$year + dat$week/52
  inc = ObservationList$new(dat)
  inc$formArray('region','t',val='weighted_ili',
                dimData = list(NULL,list('week','year','time.in.year','t')),
                metaData = list(t.step = 1/52,max.year.time = 52))
  return(inc)
}

training_inc <- preprocess_inc(training_data)
#Warning message:
#Unknown or uninitialised column: 'frame'. 
testing_inc <- preprocess_inc(testing_data)
#Warning message:
#Unknown or uninitialised column: 'frame'. 
truth_inc <- preprocess_inc(truth_data)
#Warning message:
#Unknown or uninitialised column: 'frame'. 

# Defining the Models
nsim <- 1000 # Number of simulations 
region_num <- 1 # Only modeling the "National" region
time_step <- 52

sarimaTD_model <- SARIMATD1Model$new(period = time_step, nsim = nsim)
sarima_model <- SARIMAModel$new(period = time_step, nsim = nsim)
gam_model <- GamModel$new(numProvinces = region_num, nSims = nsim)

# Fit the Models 
sarimaTD_model$fit(training_inc) # Fitting this model will take awhile
sarima_model$fit(training_inc)
gam_model$fit(training_inc)

# Forecasting the Models
steps <- 52 # forecast ahead 26 biweeks
forecast_sarimaTD <- sarimaTD_model$forecast(testing_inc, step = steps)
forecast_sarima <- sarima_model$forecast(testing_inc, step = steps)
forecast_gam <- gam_model$forecast(testing_inc, step = steps)


# Function to create time series with Visualization
visualize_model <- function(forecast, rib_color, main_color){
  data_X_3years <- dat %>% filter(year > 2013)
  preds_df <- data.frame(as.table(t(forecast$data$mat)))
  preds_df[["week_start"]] <-as.Date(truth_data$week_start, format = "%Y-%d-%m")
  preds_df <- preds_df %>%
    mutate( 
      pred_total_cases = as.vector(forecast$mean(na.rm=TRUE)$mat),
      pred_95_lb = as.vector(forecast$quantile(0.025,na.rm=TRUE)$mat),
      pred_95_ub = as.vector(forecast$quantile(0.975,na.rm=TRUE)$mat),
      pred_80_lb = as.vector(forecast$quantile(0.05,na.rm=TRUE)$mat),
      pred_80_ub = as.vector(forecast$quantile(0.95,na.rm=TRUE)$mat),
      pred_50_lb = as.vector(forecast$quantile(0.25,na.rm=TRUE)$mat),
      pred_50_ub = as.vector(forecast$quantile(0.75,na.rm=TRUE)$mat)
    )
  plot <- ggplot() +
    geom_ribbon(
      mapping = aes(x = week_start, ymin = pred_95_lb, ymax = pred_95_ub),
      fill = rib_color,
      alpha = 0.2,
      data = preds_df) +
    geom_ribbon(
      mapping = aes(x = week_start, ymin = pred_80_lb, ymax = pred_80_ub),
      fill = rib_color,
      alpha = 0.2,
      data = preds_df) +
    geom_ribbon(
      mapping = aes(x = week_start, ymin = pred_50_lb, ymax = pred_50_ub),
      fill = rib_color,
      alpha = 0.2,
      data = preds_df) +
    geom_line(
      mapping = aes(x = week_start, y = pred_total_cases),
      color = main_color,
      size = 1,
      data = preds_df) + 
    geom_line(mapping = aes(x = week_start, y = weighted_ili),
              size=0.7,
              data = data_X_3years) +
    xlab("") + ylab("Weighted ILI") + coord_cartesian(ylim = c(0, 13))
  return(plot)
}

# Create each plot
plot1 <- visualize_model(forecast_sarimaTD, "palegreen3", "forestgreen") + ggtitle("sarimaTD")
plot2 <- visualize_model(forecast_sarima, "coral1", "red") + ggtitle("SARIMA")
plot3 <- visualize_model(forecast_gam, "cornflowerblue" , "blue")+ ggtitle("GAM")

# Display side-by-side plots
grid.arrange(plot1, plot2, plot3, ncol=3)

# Mean Absolute Error
MAE <- function(forecast_mean, test_inc){
  err <- test_inc$mat-forecast_mean
  abs_err <- abs(err)
  return(mean(abs_err))
}
# Mean Squared Error
MSE <- function(forecast_mean, test_inc){
  err <- test_inc$mat-forecast_mean
  err_sq <- err^2
  return(mean(err_sq))
}
# Mean Absolute Percentage Error
MAPE <- function(forecast_mean, test_inc){
  err <- test_inc$mat-forecast_mean
  abs_err <- abs(err)
  abs_err_percent <- abs_err/test_inc$mat
  return(mean(abs_err_percent)*100)
}
# Root Mean Squared Error
RMSE <- function(forecast_mean, test_inc){
  err <- test_inc$mat-forecast_mean
  err_sq <- err^2
  mse <-mean(err_sq)
  return(sqrt(mse))
}


# Create Arrays with each Metric
evaluate <- function(forecast){
  # Replace any NaN values with 0
  forecast_mat <- forecast$mean(na.rm=TRUE)$mat
  forecast_mat[is.na(forecast_mat)] <- 0
  
  # Evaluate Performance
  MAE <- MAE(forecast_mat, truth_inc)
  MSE <- MSE(forecast_mat, truth_inc)
  MAPE <- MAPE(forecast_mat, truth_inc)
  RMSE <- RMSE(forecast_mat, truth_inc)
  return(c(MAE,MSE,MAPE,RMSE))
}

gam_eval <- evaluate(forecast_gam)
sarimaTD_eval <- evaluate(forecast_sarimaTD)
sarima_eval <- evaluate(forecast_sarima)
head(sarima_eval)
table(sarima_eval)
display.grid(sarima_eval)
return(sarima_eval)

