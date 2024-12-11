library(fpp2)

#### STAGE 2: Explore and Visualise US Revenue Passenger Miles dataset ####
revenue <- read.csv("D:\\SEM 2 2023\\ETW3420 (Fri)\\Individual assignment\\US revenue passenger miles.csv", header = TRUE)

#Convert full_dataset into time series
dataset <- ts(revenue[,-1], start=2000, frequency=12)
dataset

autoplot(dataset) +
  ggtitle("Plot of US Revenue Passenger Miles") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("RPM") 

ggseasonplot(dataset, year.labels=TRUE) + ylab("RPM")
ggsubseriesplot(dataset) + ylab("RPM")


#### STAGE 3: Pre-Process Data #### 
#start from Jan 2003 due to the 9/11 incident that affects the RPM from 2000 to 2003
#refer as full_dataset
full_dataset <- window(dataset, start = c(2010, 1))

autoplot(full_dataset) +
  ggtitle("Plot of US Revenue Passenger Miles from Jan 2003") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("RPM") 


#### STAGE 4: Pre-covid and Covid partition ####
pre_covid <- window(full_dataset, end = c(2020, 1))
covid <- window(full_dataset, start = c(2020, 2), end = c(2021, 7))
pre_covid
covid

#Divide "Pre Covid" into training set and test set
train_pre <- window(pre_covid, end = c(2018, 1))
test_pre <- window(pre_covid, start = c(2018, 2))
train_pre
test_pre

length(train_pre) #train set contain 97 observations
length(test_pre) #test set contain the last 24 observations (2 years)

#Plot the training and test partition of pre covid data
autoplot(pre_covid) +
  geom_smooth(se = F) +
  xlab("Year") +
  ylab("RPM") +
  ggtitle("Plot of training and test partition of Pre-Covid data") +
  autolayer(train_pre, series = "Training") +
  autolayer(test_pre, series = "Test")


#### STAGE 5: Apply Forecasting Methods/Models ####

##Simple Forecasting Methods
m.f <- meanf(train_pre, h = length(test_pre)) #average
rw.f <- rwf(train_pre, h = length(test_pre)) #naive
sn.f <- snaive(train_pre, h = length(test_pre)) #snaive
rwd.f <- rwf(train_pre, drift = TRUE, h = length(test_pre)) #ndrift

#Plot pre-covid with forecasts from the 4 simple forecasting methods
autoplot(pre_covid) +
  xlab("Year") +
  ylab("RPM") +
  ggtitle("Plot pre-covid with forecasts from the 4 methods") +
  autolayer(m.f$mean, col=2, series="Mean Method") +
  autolayer(rw.f$mean, col=3, series="Naive Method") +
  autolayer(sn.f$mean, col=4, series="Seasonal Naive Method") +
  autolayer(rwd.f$mean, col=5, series="Drift Method")
#From the graph, we can see that snaive method is the best since it depicts similarly to the actual value

#Evaluate forecast accuracy of the 4 methods
accuracy(m.f, test_pre)
accuracy(rw.f, test_pre)
accuracy(sn.f, test_pre) #snaive is the best method as the RMSE, MAPE, and MASE of test set value are the smallest compared to the other 3 methods
accuracy(rwd.f, test_pre)
#Method 1 will be seasonal naive method


##ETS Model
#Model 2 will be ETS[M,A,A]
model2 <- ets(train_pre, model = "MAA", damped = FALSE)
model2

#Model 3 will be ETS[A,A,A]
model3 <- ets(train_pre, model = "AAA", damped = FALSE)
model3

#Automatically chosen ETS Model by R
model4 <- ets(train_pre)
summary(model4)
#[M,Ad,M]

#Plot training set with the fitted values from each of the method/models
#1. Snaive
snaive_plot <- autoplot(train_pre, series = "Data") +
  autolayer(fitted(snaive(train_pre)), series = "Fitted") +
  labs(subtitle = "from Snaive Method",
       x = "Year",
       y = "RPM")

#2. ETS[M,A,A]
MAA_plot <- autoplot(train_pre, series = "Data") +
  autolayer(fitted(model2), series = "Fitted") +
  labs(subtitle = "from ETS[M,A,A] Model",
       x = "Year",
       y = "RPM")

#3. ETS[A,A,A]
AAA_plot <- autoplot(train_pre, series = "Data") +
  autolayer(fitted(model3), series = "Fitted") +
  labs(subtitle = "from ETS[A,A,A] Model",
       x = "Year",
       y = "RPM")

#4. ETS[M,Ad,M]
MAdM_plot <- autoplot(train_pre, series = "Data") +
  autolayer(fitted(model4), series = "Fitted") +
  labs(subtitle = "from ETS[M,Ad,M] Model",
       x = "Year",
       y = "RPM")

library(ggpubr)

combine <- ggarrange(snaive_plot, MAA_plot, AAA_plot, MAdM_plot, nrow = 2, ncol = 2)

annotate_figure(combine, top = text_grob ("Plot Training Set with the Fitted Values", face = "bold", size = 14))


#Within-sample Residual check for all method/models
checkresiduals(snaive(train_pre))
checkresiduals(model2)
checkresiduals(model3)
checkresiduals(model4)

#Best goodness of fit
accuracy(snaive(train_pre))
accuracy(model2)
accuracy(model3)
accuracy(model4)


#Plot forecasts for the test set period
fc1 <- snaive(train_pre, h = 24)

fc2 <- ets(model2) %>% forecast(h = 24)

fc3 <- ets(model3) %>% forecast(h = 24)

fc4 <- ets(model4) %>% forecast(h = 24)

autoplot(pre_covid) +
  autolayer(fc1, PI = F, series = "Seasonal Naive") +
  autolayer(fc2, PI = F, series = "ETS[M,A,A]") +
  autolayer(fc3, PI = F, series = "ETS[A,A,A]") +
  autolayer(fc4, PI = F, series = "ETS[M,Ad,M]") +
  ggtitle("Out-of-sample Forecast for (test set period) along with the 4 forecasting methods/models") +
  xlab("Year") +
  ylab("RPM")


#### STAGE 6: Evaluation and Comparing Forecasting Performance ####

##TRADITIONAL APPROACH - Out-of-sample forecast
accuracy(fc1, test_pre)
accuracy(fc2, test_pre)
accuracy(fc3, test_pre)
accuracy(fc4, test_pre)

##TIME SERIES CROSSVALIDATION
#Snaive method
e1 <- tsCV(pre_covid, forecastfunction = snaive, h = 12)

#MAA model
fets2 <- function(y, h) {
  ets(y, model = "MAA") %>% forecast(h = h)
}

e2 <- tsCV(pre_covid, forecastfunction = fets2 , h = 12)

#AAA model
fets3 <- function(y, h) {
  ets(y, model = "AAA") %>% forecast(h = h)
}

e3 <- tsCV(pre_covid, forecastfunction = fets3 , h = 12)

#MAdM Model
fets4 <- function(y, h) {
  ets(y) %>% forecast(h = h)
}

e4 <- tsCV(pre_covid, forecastfunction = fets4 , h = 12)

#Calculate MSE
colMeans(e1^2, na.rm = TRUE)
colMeans(e2^2, na.rm = TRUE)
colMeans(e3^2, na.rm = TRUE)
colMeans(e4^2, na.rm = TRUE)


#### STAGE 7: Implement Forecast ####

#Re-estimate the Pre-Covid dataset using ETS[M,Ad,M]
model_pre <- ets(pre_covid)
model_pre

length(covid) #[18]

#Forecast for Covid Period
model_pre_fc <- model_pre %>% forecast(h = 18)
summary(model_pre_fc)

#Plot
autoplot(full_dataset) +
  xlab("Year") +
  ylab("RPM") +
  ggtitle("Forecast for Covid period") +
  autolayer(model_pre_fc, PI = T, series = "ETS[M,Ad,M]")


#### STAGE 8: Forecasted Loss in RPM ####

#Forecasted Loss
sum(covid) - sum(model_pre_fc$mean)

#Lower 95
sum(covid) - sum(model_pre_fc$lower[, 2])

#Upper 95
sum(covid) - sum(model_pre_fc$upper[, 2])
