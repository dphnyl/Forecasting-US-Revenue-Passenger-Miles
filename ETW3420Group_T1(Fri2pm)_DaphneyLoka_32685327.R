library(fpp2)

## Import dataset ##
ori_rpm <- read.csv("D:\\SEM 2 2023\\ETW3420 (Fri)\\Group assignment\\US revenue passenger miles.csv.csv", header = TRUE)

## Convert full_dataset into time series ##
dataset <- ts(ori_rpm[,-1], start=2000, frequency=12)
dataset

autoplot(dataset) +
  ggtitle("Plot of US Revenue Passenger Miles") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("RPM") 

## Pre-Process Data ##
#start from Jan 2003 due to the 9/11 incident that affects the RPM from 2000 to 2003
#refer as full_dataset
full_dataset <- window(dataset, start = c(2010, 1))

autoplot(full_dataset) +
  ggtitle("Plot of US Revenue Passenger Miles from Jan 2003") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("RPM") 

## Pre-covid and Covid partition ##
pre_covid <- window(full_dataset, end = c(2020, 1))
covid <- window(full_dataset, start = c(2020, 2), end = c(2021, 7))
pre_covid
covid


#### PHASE 1: Model Identification ####

## Partition the "Pre-Covid" dataset into training and test set using 70-30 ratio ##
length(pre_covid) #121 observations

#70% of 121 = 85 observations
#30% of 121 = 36 observations (last 3 years)

train_pre <- window(pre_covid, end = c(2017, 1))
test_pre <- window(pre_covid, start = c(2017, 2))
train_pre
test_pre

length(train_pre) #train set contain 85 observations
length(test_pre) #test set contain the last 36 observations (3 years)

#Plot the training and test partition of pre covid data
autoplot(pre_covid) +
  geom_smooth(se = F) +
  xlab("Year") +
  ylab("RPM") +
  ggtitle("Plot of training and test partition of Pre-Covid data") +
  autolayer(train_pre, series = "Training") +
  autolayer(test_pre, series = "Test")

#Plot the training set data
autoplot(train_pre) +
  geom_smooth(se = F) +
  xlab("Year") +
  ylab("RPM") +
  ggtitle("Plot of training set data of Pre-Covid data")


## Data transformation ##
(bc_lambda <- BoxCox.lambda(train_pre)) #0.462683

train_pre %>% BoxCox(lambda = bc_lambda) %>% ggtsdisplay()

#There is seasonality pattern as revenue is a monthly data

#Test if we need seasonality differencing
nsdiffs(train_pre) #[1]

#Plot seasonality difference
train_pre %>% BoxCox(lambda = bc_lambda) %>% diff(lag = 12) %>% ggtsdisplay()

#Test if first differencing is needed
train_pre %>% diff(lag = 12) %>% ndiffs() #[1]

#Plot the 1st diff
train_pre %>% BoxCox(lambda = bc_lambda) %>% diff(lag = 12) %>% diff(lag = 1) %>% ggtsdisplay()

#Test if second order diff is needed
train_pre %>% BoxCox(lambda = bc_lambda) %>% diff(lag = 12) %>% diff(lag = 1) %>% ndiffs()
#data is already stationary, hence further differencing is not needed anymore


#### Phase 2: Estimation and testing ####

## ARIMA MODEL ## 
#Include constant (c = 0)

#Manually chosen two ARIMA models

#p,P = 0
fit1 <- Arima(train_pre, order = c(0, 1, 1), seasonal = c(0, 1, 2),
              include.constant = TRUE, lambda = bc_lambda)
summary(fit1)

#Variation from fit1 considering that there is 2 significant spike in ACF
fit2 <- Arima(train_pre, order = c(2, 1, 1), seasonal = c(0, 1, 2),
              include.constant = TRUE, lambda = bc_lambda)
summary(fit2)

#q,Q = 0
fit3 <- Arima(train_pre, order = c(2, 1, 0), seasonal = c(0, 1, 0),
              include.constant = TRUE, lambda = bc_lambda)
summary(fit3)

#Variation from fit3 considering the 2 spikes at seasonal lag (lag 12, 24) in ACF (we include lag 12 since near the blue line)
fit4 <- Arima(train_pre, order = c(2, 1, 0), seasonal = c(0, 1, 2),
              include.constant = TRUE, lambda = bc_lambda)
summary(fit4)

#Auto arima model
fit5 <- auto.arima(train_pre, lambda = bc_lambda, stepwise = FALSE, approximation = FALSE) 
summary(fit5)
#ARIMA(0,1,1)(2,1,0)

checkresiduals(fit1) #WN on 5% and 10% level
checkresiduals(fit2) #WN on 5% and 10% level
checkresiduals(fit3) #not WN 
checkresiduals(fit4) #WN on 5% and 10% level
checkresiduals(fit5) #on 5% significance level, the residuals are WN, but not on 10% significance level

c(fit1$aicc, fit2$aicc, fit3$aicc, fit4$aicc, fit5$aicc)
#Top 2 model = fit1 and fit4

#Based on residuals and p-value, we are satisfied with the result as p-value for for both models is greater than 0.1.
#which fulfils the criteria of being white noise in 10% significance level
#hence, we are satisfied with the model selection and no re-identification is required anymore.


#### Phase 3: Application ####
#forecast fit2
model.fit <- Arima(pre_covid, order = c(0, 1, 1), seasonal = c(0, 1, 2),
                   include.constant = T, lambda = bc_lambda)

farima <- forecast(model.fit, length(covid))

a1 <- farima |> accuracy(dataset)
a1[,c("RMSE","MAE","MPE","MAPE","MASE")]

autoplot(full_dataset,series="Data") +
  autolayer(farima,series = "ARIMA")+
  scale_color_manual(name = "Forecast Model", values = c( "Data"="black","ARIMA" = "blue")) +
  scale_y_continuous(labels =scales::comma)+
  ylab("Revenue Passanger Miles")+
  ggtitle("Forecast of Revenue Passanger Miles during Covid period")

#Comparison with ETS model 
fets <- forecast(ets(pre_covid),length(covid))

a2 <- fets |> accuracy(dataset)
a2[,c("RMSE","MAE","MPE","MAPE","MASE")]

autoplot(full_dataset, series = "Data") +
  autolayer(farima, series = "ARIMA", alpha = 0.6)+
  autolayer(fets,PI=T,series="ETS", alpha=0.2)+
  scale_color_manual(name = "Forecast Models", values = c( "Data"="black","ARIMA" = "blue", "ETS"="red")) +
  scale_y_continuous(labels =scales::comma)+
  ylab("Revenue Passanger Miles")+
  ggtitle("Comparison of Revenue Passanger Miles Forecast during Covid period")

#Forecasted loss by ARIMA model
(foreRPM_arima <-sum(farima$mean))
(actualRPM_arima <- sum(ori_rpm$RPM[242:259]))
(forecasted_loss_arima <- foreRPM_arima-actualRPM_arima)

#Forecasted loss by ETS model
(foreRPM_ets <-sum(fets$mean))
(actualRPM_ets <- sum(ori_rpm$RPM[242:259]))
(forecasted_loss_ets <- foreRPM_ets-actualRPM_ets)
