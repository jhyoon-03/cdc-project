# Fitting and Forecasting Code (Using cdcfluview package to ingest data)
# -----------------------------------------------------------------
# 1 import packages
library(ForecastFramework)
library(R6)
library(forecast)
library(dplyr)
library(ggplot2)
library(cdcfluview)


# Source R6 Files
library(RCurl)

# Function of Source Github Models
source_github <- function(u) {
  # read script lines from website and evaluate
  script <- getURL(u, ssl.verifypeer = FALSE)
  eval(parse(text = script),envir=.GlobalEnv)
}  
# Source R6 Files
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/ContestModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/SARIMATD1Model.R')

# -----------------------------------------------------------------
# 2 new sarimaTDmodel
nsim <- 10 # Number of SARIMA simulations 
sarimaTDmodel <- SARIMATD1Model$new(period = 52, nsim = nsim)

# -----------------------------------------------------------------
# 3 import CDC Flu Data
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
testing_inc <- preprocess_inc(testing_data)
truth_inc <- preprocess_inc(truth_data)

print(training_inc$mat[,0:10])

# -----------------------------------------------------------------
# 4 fit the model
sarimaTDmodel$fit(training_inc)

# -----------------------------------------------------------------
# 5 forecasting SARIMA
steps <- 52
forecast_X <- sarimaTDmodel$forecast(testing_inc,steps = steps)

# -----------------------------------------------------------------
# 6 formatting Forecast Output
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

# -----------------------------------------------------------------
# 7 plot Forecast Output
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


