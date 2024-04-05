library(readxl)
library(car)
library(tseries)
library(lmtest)
library(forecast)

###
# it is very much possible some of the figures from the report are not here
# they should however be obtainable from the code in a straightforward way
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
# MODELS
###

###
# 1) SEASONAL DUMMY
###

decomp = decompose( at, type = 'additive' )
plot( decomp )

md0 = lm( at ~ 1 + dummy, data = data_at_in )
checkresiduals( md0 )

md1 = lm( at ~ t + dummy, data = data_at_in )
checkresiduals( md1 )

md2 = lm( at ~ t + I(t^2) + dummy, data = data_at_in )
checkresiduals( md2 )
acf(residuals( md2 ), main = 'Correlogram of residuals of the quadratic dummy model')

md3 = lm( at ~ t + I(t^2) + I(t^3) + dummy, data = data_at_in )
checkresiduals( md3 )

par(mfrow=c(1,1))
plot( at_in, lwd = 2, ylab = 'Unemployment' )
title( 'Fitted values of different dummy variable models')
lines( ts(predict(md0), start = start(at_in), frequency = frequency(at_in)),
       col = 'blueviolet', lwd = 2 )
lines( ts(predict(md1), start = start(at_in), frequency = frequency(at_in)),
       col = 'cyan2', lwd = 2 )
lines( ts(predict(md2), start = start(at_in), frequency = frequency(at_in)),
       col = 'deeppink1', lwd = 2 )
lines( ts(predict(md3), start = start(at_in), frequency = frequency(at_in)),
       col = 'goldenrod', lwd = 2 )
legend('topleft', legend = c('data', 'constant', 'linear', 'quadratic', 'cubic'),
       col = c('black', 'blueviolet', 'cyan2', 'deeppink', 'goldenrod'),
       lty = c(1,1,1,1), lwd = c(2,2,2,2))


c( BIC(md0), BIC(md1), BIC(md2), BIC(md3) )
c( summary(md0)$adj.r.squared, summary(md1)$adj.r.squared,
   summary(md2)$adj.r.squared, summary(md3)$adj.r.squared )


###
# 2) TRIGONOMETRIC SEASONALITY
###

mt1 = lm( at ~ t +
           I(sin(2*pi*t/12)) + I(cos(2*pi*t/12)) +
           I(sin(4*pi*t/12)) + I(cos(4*pi*t/12)) +
           I(sin(6*pi*t/12)) + I(cos(6*pi*t/12)) +
           I(sin(8*pi*t/12)) + I(cos(8*pi*t/12)) +
           I(sin(10*pi*t/12)) + I(cos(10*pi*t/12)) +
           I(sin(12*pi*t/12)) + I(cos(12*pi*t/12)), data = data_at_in )
mt2 = lm( at ~ t + I(t^2) +
            I(sin(2*pi*t/12)) + I(cos(2*pi*t/12)) +
            I(sin(4*pi*t/12)) + I(cos(4*pi*t/12)) +
            I(sin(6*pi*t/12)) + I(cos(6*pi*t/12)) +
            I(sin(8*pi*t/12)) + I(cos(8*pi*t/12)) +
            I(sin(10*pi*t/12)) + I(cos(10*pi*t/12)) +
            I(sin(12*pi*t/12)) + I(cos(12*pi*t/12)), data = data_at_in )
mt3 = lm( at ~ t + I(t^2) + I(t^3) +
            I(sin(2*pi*t/12)) + I(cos(2*pi*t/12)) +
            I(sin(4*pi*t/12)) + I(cos(4*pi*t/12)) +
            I(sin(6*pi*t/12)) + I(cos(6*pi*t/12)) +
            I(sin(8*pi*t/12)) + I(cos(8*pi*t/12)) +
            I(sin(10*pi*t/12)) + I(cos(10*pi*t/12)) +
            I(sin(12*pi*t/12)) + I(cos(12*pi*t/12)), data = data_at_in )
checkresiduals( mt1 )
checkresiduals( mt2 )
acf( residuals(mt2),
     main = 'Correlogram of quadratic model with trigonometric seasonality' )
checkresiduals( mt3 )

plot( at_in, lwd = 2, ylab = 'Unemployment',
      main = 'Fitted values for different trigonometric models')
lines( ts(predict(mt1), start = start(at_in), frequency = frequency(at_in)),
       col = 'blueviolet', lwd = 2 )
lines( ts(predict(mt2), start = start(at_in), frequency = frequency(at_in)),
       col = 'cyan', lwd = 2 )
lines( ts(predict(mt3), start = start(at_in), frequency = frequency(at_in)),
       col = 'deeppink', lwd = 2 )
legend('topleft', legend = c('data', 'linear', 'quadratic', 'cubic'),
       col = c('black', 'blueviolet', 'cyan', 'deeppink'),
       lty = c(1,1,1,1), lwd = c(2,2,2,2))

c( BIC(mt1), BIC(mt2), BIC(mt3) )
c( summary(mt1)$adj.r.squared, summary(mt2)$adj.r.squared, summary(mt3)$adj.r.squared )


###
# 3) ETS model
###

mETS = ets( at_in, model = 'ZZZ', opt.crit = 'lik', ic = 'bic' )
checkresiduals( mETS )

plot(mETS)

plot( at_in, lwd = 2 )
lines( mETS$fitted, col = 'red', lwd = 2)
BIC( mETS )


###
# 4) BATS model
###

mBATS = bats( at_in )
checkresiduals( mBATS )
acf( residuals(mBATS), main = 'Correlogram of the BATS model' )

plot(mBATS)
mBATS
qqnorm( residuals(mBATS),
        main = 'Normal QQ plot of residuals of the TBATS model' )
qqline( residuals(mBATS), col = 'red', lwd = 2 )

plot( at_in, lwd = 2 )
lines( mBATS$fitted, col = 'red', lwd = 2 )


###
# 5) TBATS model
###

mTBATS = tbats( at_in )
checkresiduals( mTBATS )
acf( residuals(mTBATS), main = 'Correlogram of the TBATS model' )

plot(mTBATS)
mTBATS
qqnorm( residuals(mTBATS),
        main = 'Normal QQ plot of residuals of the TBATS model' )
qqline( residuals(mTBATS), col = 'red', lwd = 2 )

plot( at_in, lwd = 2 )
lines( mTBATS$fitted, col = 'red', lwd = 2 )


###
# COMPARISON
###

plot( at_in, lwd = 2 )
lines( mETS$fitted, col = 'blueviolet', lwd = 2 )
lines( mBATS$fitted, col = 'deeppink1', lwd = 2 )
lines( mTBATS$fitted, col = 'goldenrod', lwd = 2 )


###
# 6) FORECAST
###

mETS_forecast = forecast( mETS, h = 12 )
mBATS_forecast = forecast( mBATS, h = 12 )
mTBATS_forecast = forecast( mTBATS, h = 12 )

plot( mETS_forecast, ylab = 'Unemployment', xlab = 'Time',
      main = 'Fit and forecast of the ETS model' )
lines( at_full, col = 'black', lwd = 2 )
lines( mETS$fitted, col = 'red', lwd = 2 )
lines( mETS_forecast$mean, lwd = 2, col = 'cyan2' )
legend( 'topleft', legend = c('data', 'fitted', 'prediction'),
        col = c('black', 'red', 'cyan2'), lty = c(1,1,1), lwd = c(2,2,2) )

plot( mBATS_forecast, ylab = 'Unemployment', xlab = 'Time',
      main = 'Fit and forecast of the BATS model' )
lines( at_full, col = 'black', lwd = 2 )
lines( mBATS$fitted, col = 'red', lwd = 2 )
lines( mBATS_forecast$mean, lwd = 2, col = 'cyan2' )
legend( 'topleft', legend = c('data', 'fitted', 'prediction'),
        col = c('black', 'red', 'cyan2'), lty = c(1,1,1), lwd = c(2,2,2) )

plot( mTBATS_forecast, ylab = 'Unemployment', xlab = 'Time',
      main = 'Fit and forecast of the TBATS model' )
lines( at_full, col = 'black', lwd = 2 )
lines( mTBATS$fitted, col = 'red', lwd = 2 )
lines( mTBATS_forecast$mean, lwd = 2, col = 'cyan2' )
legend( 'topleft', legend = c('data', 'fitted', 'prediction'),
        col = c('black', 'red', 'cyan2'), lty = c(1,1,1), lwd = c(2,2,2) )

plot( ts(at_out), lwd = 2, ylim = c(130, 330), ylab = 'Unemployment',
      main = 'Comparison of predictions by the ETS, BATS ans TBATS models',
      xlab = 'Month of 2023')
lines( ts(mETS_forecast$mean), lwd = 2, col = 'blueviolet' )
lines( ts(mETS_forecast$lower[,2]), lwd = 2, col = 'blueviolet', lty = 2 )
lines( ts(mETS_forecast$upper[,2]), lwd = 2, col = 'blueviolet', lty = 2 )
lines( ts(mBATS_forecast$mean), lwd = 2, col = 'deeppink1' )
lines( ts(mBATS_forecast$lower[,2]), lwd = 2, col = 'deeppink1', lty = 2 )
lines( ts(mBATS_forecast$upper[,2]), lwd = 2, col = 'deeppink1', lty = 2 )
lines( ts(mTBATS_forecast$mean), lwd = 2, col = 'goldenrod' )
lines( ts(mTBATS_forecast$lower[,2]), lwd = 2, col = 'goldenrod', lty = 2 )
lines( ts(mTBATS_forecast$upper[,2]), lwd = 2, col = 'goldenrod', lty = 2 )
legend( 'bottomleft', legend = c('data', 'ETS', 'BATS', 'TBATS'),
        col = c('black', 'blueviolet', 'deeppink1', 'goldenrod'),
        lty = c(1,1,1,1), lwd = c(2,2,2,2) )

accuracy(mETS_forecast, at_out)
accuracy(mBATS_forecast, at_out)
accuracy(mTBATS_forecast, at_out)
