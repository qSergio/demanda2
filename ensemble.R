rm(list = ls())

# Load some packages: 
library(nnfor)
library(fpp2)
library(tidyverse)
library(magrittr)


# Split data: 
#acti <- read.csv("act_real(1).csv")
acti <- read.csv("training_data_d.csv")

#Esto es con datos de supertienda
activ <- log(retail$cantidad)



train <- activ[53:156] %>% ts(frequency = 52)
test <- activ[157:208] %>% ts(frequency = 52)

#train <- act %>% ts(frequency = 52)
#test <- test %>% ts(frequency = 52)

h <- length(test)

#======================================================
#  MLP neural networks for Forecasting Time Series
#======================================================

# Prepare a series for train and forecasting: 
tt <- cbind(c(1:(length(train) + h), rep(0, h)))

# Fit a network with no differencing, no univariate lags, and fixed deterministic trend: 
my_mlp <- mlp(train, 
              hd = 50, 
              difforder = c(0,1), 
              lags = 1, 
              xreg = tt, 
              xreg.lags = list(0), 
              outplot=TRUE,
              reps = 10,
              xreg.keep = TRUE)

# Make predictions: 
pred <- forecast(my_mlp, h = h, xreg = tt)

# Data Frame for comparing: 

df_mlp <- data_frame(Actual = test %>% as.vector(), 
                     Predicted = pred$mean, 
                     Error = Predicted - Actual,
                     Error_Percent = Error / Actual)#round(Error / Actual, 2))

df_mlp %>% knitr::kable()


# Function calculate some accuracy measures: 
get_accuracy_measures <- function(your_result_df) {
  
  act <- your_result_df %>% pull(Actual)
  pred <- your_result_df %>% pull(Predicted)
  err <- act - pred 
  per_err <- abs(err / act)
  
  # Mean Absolute Error (MAE): 
  mae <- err %>% abs() %>% mean()
  
  # Mean Squared Error (MSE): 
  mse <- mean(err^2)
  
  # Mean Absolute Percentage Error (MAPE): 
  mape <- 100*mean(per_err)
  
  # Return results: 
  return(data_frame(MAE = mae, MSE = mse, MAPE = mape, N = length(act)))
}


#==================================================================
#  Auto ARIMA Approach as proposed by Hyndman and Khandakar (2008)
#==================================================================

my_arima <- auto.arima(train,D=1)

# Use the model for forecasting: 
predicted_arima <- forecast(my_arima, h = h)$mean %>% as.vector()


# Data Frame for comparing: 
df_arima <- data_frame(Actual = test %>% as.vector(), 
                       Predicted = predicted_arima, 
                       Error = Predicted - Actual,
                       Error_Percent = Error / Actual)#round(Error / Actual, 2))

# Model Performance: 
df_arima %>% knitr::kable()


#=======================================
#    My Solution: Ensemble Method 
#=======================================

my_ensemble_predict <- function(your_df, n_ahead) {
  
  N <- length(your_df)
  end1 <- N - n_ahead
  
  train <- your_df[1:end1] %>% ts(frequency = 14)
  test <- your_df[(end1 + 1):N] %>% ts(frequency = 14)
  h <- length(test)
  
  tt <- cbind(c(1:(length(train) + h), rep(0, h)))
  
  my_mlp <- mlp(train, 
                hd = 30, 
                difforder = c(0,1), 
                lags = 1, 
                xreg = tt, 
                xreg.lags = list(0), 
                outplot=TRUE,
                reps = 5,
                xreg.keep = TRUE)
  
  predicted_mlp <- forecast(my_mlp, h = h, xreg = tt)
  
  my_arima <- auto.arima(train,D=1)
  predicted_arima <- forecast(my_arima, h = h)$mean %>% as.vector()
  
  ensemble_predicted <- 0.5*(predicted_mlp$mean + predicted_arima)
  
  return(data_frame(Actual = test %>% as.vector(), 
                    Predicted = ensemble_predicted, 
                    Error = Predicted - Actual,
                    Error_Percent = Error / Actual) )#)round(Error / Actual, 2)))
} 


# Use the function: 
my_ensemble_predict(activ, 26) -> df_ensemble
df_ensemble %>% knitr::kable()


# Compare three approaches: 

bind_rows(df_mlp %>% get_accuracy_measures(), 
          df_arima %>% get_accuracy_measures(), 
          df_ensemble %>% get_accuracy_measures()) %>% 
  mutate(Approach = c("MLP", "ARIMA", "Ensemble")) %>% 
  select(Approach, everything()) %>% 
  mutate_if(is.numeric, function(x) {round(x, 2)}) %>% 
  knitr::kable()

df_mlp %>% get_accuracy_measures()
