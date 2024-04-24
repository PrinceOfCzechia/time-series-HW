library(readxl)
library(car)
library(tseries)
library(lmtest)
library(forecast)

###
# It is very much possible some of the figures from the report are not
# in the code in the exact way presented in the report.
# They should however be obtainable from the code in a straightforward way.
###

rm(list=ls())

###
# load data
###

data = read_xlsx( 'C:/Users/Kral/Documents/MFF/casove_rady/une_rt_m.xlsx',
                  sheet = 'Sheet 1' )
numeric_at = as.numeric( data[33,] ) # ignore the warnings, it is fine
data_at = na.omit( numeric_at )

at = ts( data = data_at, frequency = 12, start = c(1994,1) )

at_full = window( at, start = c(2010,1), end = c(2023,12) )
plot(at_full, lwd=2, ylab = 'Unemployment')
title( 'Unemployment in Austria between 2010 and 2023 in thousands of persons' )

at_in = window( at, start = c(2010,1), end = c(2022,12), frequency = 12 )
at_out = window( at, start = c(2023,1), end = c(2023,12), frequency = 12 )

data_at_in = data.frame( at = at_in, t = seq_along(at_in), dummy = as.factor(cycle(at_in)))
data_at_out = data.frame( at = at_out, t = seq_along(at_out), dummy = as.factor(cycle(at_out)))

###
# SARIMA model
###

# model
ms = auto.arima(at_in, seasonal = TRUE, ic = "bic", test = "adf", stepwise = FALSE, allowmean = FALSE, approximation=FALSE)
summary(ms)

# diagnostics
autoplot(ms)
checkresiduals(ms)

# plot fit
plot( at_in, lwd = 2, ylab = 'Unemployment',
      main = 'SARIMA(1,0,1)x(2,0,0)[12] model fit' )
lines( ms$fitted, col = 'blueviolet', lwd = 2 )
legend( 'topleft', legend = c('data', 'SARIMA'),
        col = c('black', 'blueviolet'),
        lty = c(1,1), lwd = c(2,2) )

# prediction
ms_forecast = forecast( ms, h = 12 )
plot( ts(at_out), lwd = 2, ylim = c(80, 330), ylab = 'Unemployment',
      main = 'Prediction of the SARIMA(1,0,1)x(2,0,0)[12] model',
      xlab = 'Month of 2023')
lines( ts(ms_forecast$mean), lwd = 2, col = 'blueviolet' )
lines( ts(ms_forecast$lower[,2]), lwd = 2, col = 'blueviolet', lty = 2 )
lines( ts(ms_forecast$upper[,2]), lwd = 2, col = 'blueviolet', lty = 2 )
legend( 'topleft', legend = c('data', 'SARIMA'),
        col = c('black', 'blueviolet'),
        lty = c(1,1), lwd = c(2,2) )