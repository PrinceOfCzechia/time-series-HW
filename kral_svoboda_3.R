rm(list = ls())

library(tseries)
library(forecast)
library(rugarch)

# loading the raw data
data = read.csv('TSM.csv', stringsAsFactors = TRUE)
tsm = ts(data$Adj.Close)
plot(data$Date, tsm, type='l')

# log-returns series
lr = diff(log(tsm))
plot(data$Date[-1], lr, type='l')

m_arima = auto.arima(lr, allowmean = TRUE)
summary(m_arima)
