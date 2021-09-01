library(dplyr)
library(fpp)
library(fpp2)
library(urca)

# read the data in R
Border_data <- read.csv("~/Desktop/Fall 2019/Business Forecasting/BF - Project/Border_Crossing_Entry_Data.csv")
Border_data<-Border_Crossing_Entry_Data
View(Border_data)
#summary(Border_data)


# filter the data data for US-Mexico Border
Border_data = filter(Border_data, Border_data$Border == 'US-Mexico Border')

# Group by date and sum the all the value
Border_data = Border_data %>% group_by(Date) %>% 
  summarise(Value = sum(Value))


# change the date format 
Border_data$Date = as.Date(Border_data$Date, format = "%m/%d/%Y ")

# sort the data by date
Border_data = arrange(Border_data,Date)
View(Border_data)

# filter the value from the data
Border_data = Border_data$Value

summary(Border_data)

# change the data in a time-series data
Border_data = ts(Border_data, start = c(1996,1), end = c(2019,3), frequency = 12)

# plot the time series data 
plot(Border_data)

# gives the trend/sesonal plot of the data
decomp = stl(Border_data, s.window = 5, robust = T)
plot(decomp)

#t.window = ceiling((1.5*12) / (1-(1.5/7)))


plot(Border_data, col="gray",
     main=" Border Data ",
     ylab="Trend", xlab="Time")

lines(decomp$time.series[,"trend"],col="red")

monthplot(decomp$time.series[,'seasonal'])


plot(decomp$time.series[,'seasonal'])

plot(seasadj(decomp))
seasadj(decomp)

plot(Border_data, col = 'grey')
lines(seasadj(decomp), col = 'red')

ggseasonplot(Border_data)
ggsubseriesplot(Border_data)

#qplot(Border_data)

#Moving Average

plot(Border_data, col = 'grey')
lines(ma(Border_data,12), col = 'blue')


# test of stationary
adf.test(Border_data, alternative = "stationary") # no stationary we have to do differencing


# Auto correlation
Acf(Border_data)
ggAcf(Border_data)
Pacf(Border_data)

# smoothing - simple exponential smoothing
fit1 = ses(Border_data, alpha = 0.2, initial = "simple", h=15)
summary(fit1)
fit2 = ses(Border_data, h=15)
plot(Border_data, col = "grey")
lines(fitted(fit1), col = "red")
lines(fitted(fit2), col = "blue")

autoplot(Border_data) +
  autolayer(fit1, PI = F) +
  autolayer(fit2, PI = F)

# smoothing - holt's linear trend
fit3 = holt(Border_data, h=15)
plot(Border_data, col = "grey")
lines(fitted(fit3), col = "red")

autoplot(Border_data) +
  autolayer(fit3, PI = F)

# smoothing halt's winter trend
fit4 = hw(Border_data)


# KPSS test

test = ur.kpss(Border_data)
summary(test)

test = ur.kpss(diff(Border_data)) ## data is non stationary
summary(test)

nsdiffs(Border_data)# Only 1 differencing is required to get stationary data

# ARIMA (first make it a non seasonal data)

#AR

Arima(Border_data, order = c(1,0,0))
summary(Arima(Border_data, order = c(1,0,0)))
auto.arima()

fit=auto.arima(Border_data,seasonal = FALSE)
fit
# MA

Arima(Border_data, order = c(0,0,1))
summary(Arima(Border_data, order = c(0,0,1)))


# ARMA

Arima(Border_data, order = c(1,0,1))
summary(Arima(Border_data, order = c(1,0,1)))


# ARIMA
Arima(Border_data, order = c(2,1,2))
summary(Arima(Border_data, order = c(2,1,2)))








############################################################################
border_data_train = window(Border_data, start = c(1996,1), end = c(2017,12))
border_data_test = window(Border_data, start = c(2018,1), end = c(2019,3))
plot(border_data_train)
plot(border_data_test)

# meanf method
meanf = meanf(border_data_train, h=15)
accuracy(meanf, border_data_test)

# naive method
naive = naive(border_data_train, h = 15)
accuracy(naive, border_data_test)

# seasonal naive
seasonal_naive = snaive(border_data_train, h = 15)
accuracy(seasonal_naive, border_data_test)

# drift 
drift = rwf(border_data_train, h = 15, drift = T)
accuracy(drift, border_data_test)

autoplot(border_data_train) +
  autolayer(meanf, series = "Mean", PI = F) +
  autolayer(naive, series = "Naive", PI = F) +
  autolayer(seasonal_naive, series = "Seasonal Naive", PI = F) +
  autolayer(drift, series = "Drift", PI = F) +
  ggtitle("Forecast for Border Data") +
  xlab("Years") + ylab("Value") + 
  guides(colour = guide_legend(title = "Forecast"))


