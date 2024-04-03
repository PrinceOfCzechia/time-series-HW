library(readxl)
library(car)
library(tseries)
library(lmtest)
library(forecast)

rm(list=ls())

###
# load data
###

data = read_xlsx( 'C:/Users/Kral/Documents/MFF/casove_rady/une_rt_m.xlsx',
                  sheet = 'Sheet 1' )
numeric_bel = as.numeric( data[14,] )
data_bel = na.omit( numeric_bel )

bel = ts( data = data_bel, frequency = 12, start = c(1986,4) )

bel_in = window(bel, start = c(2010,1), end = c(2022,12) )
bel_out = window(bel, start = c(2023,1), end = c(2023,12) )

data_bel_in = data.frame(BEL = bel_in, t = seq_along(bel_in), dummy = as.factor(cycle(bel_in)))
data_bel_out = data.frame(BEL = bel_out, t = seq_along(bel_out), dummy = as.factor(cycle(bel_out)))


###
# MODELS
###

###
# 1) SEASONAL DUMMY
###

decomp = decompose( bel, type = 'additive' )
plot( decomp )

md0 = lm( BEL ~ 1 + dummy, data = data_bel_in )
checkresiduals( md0 )

md1 = lm( BEL ~ t + dummy, data = data_bel_in )
checkresiduals( md1 )

md2 = lm( BEL ~ t + I(t^2) + dummy, data = data_bel_in )
checkresiduals( md2 )

md3 = lm( BEL ~ t + I(t^2) + I(t^3) + dummy, data = data_bel_in )
checkresiduals( md3 )

par(mfrow=c(1,1))
plot( bel_in, lwd = 2 )
lines( ts(predict(md0), start = start(bel_in), frequency = frequency(bel_in)),
       col = 'yellow1', lwd = 2 )
lines( ts(predict(md1), start = start(bel_in), frequency = frequency(bel_in)),
       col = 'magenta', lwd = 2 )
lines( ts(predict(md2), start = start(bel_in), frequency = frequency(bel_in)),
       col = 'cyan', lwd = 2 )
lines( ts(predict(md3), start = start(bel_in), frequency = frequency(bel_in)),
       col = 'green', lwd = 2 )


c( BIC(md0), BIC(md1), BIC(md2), BIC(md3) )
c( summary(md0)$r.squared, summary(md1)$r.squared,
   summary(md2)$r.squared, summary(md3)$r.squared )


###
# 2) TRIGONOMETRIC SEASONALITY
###

mt1 = lm( BEL ~ t +
           I(sin(2*pi*t/12)) + I(cos(2*pi*t/12)) +
           I(sin(4*pi*t/12)) + I(cos(4*pi*t/12)) +
           I(sin(6*pi*t/12)) + I(cos(6*pi*t/12)) +
           I(sin(8*pi*t/12)) + I(cos(8*pi*t/12)) +
           I(sin(10*pi*t/12)) + I(cos(10*pi*t/12)) +
           I(sin(12*pi*t/12)) + I(cos(12*pi*t/12)), data = data_bel_in )
mt2 = lm( BEL ~ t + I(t^2) +
            I(sin(2*pi*t/12)) + I(cos(2*pi*t/12)) +
            I(sin(4*pi*t/12)) + I(cos(4*pi*t/12)) +
            I(sin(6*pi*t/12)) + I(cos(6*pi*t/12)) +
            I(sin(8*pi*t/12)) + I(cos(8*pi*t/12)) +
            I(sin(10*pi*t/12)) + I(cos(10*pi*t/12)) +
            I(sin(12*pi*t/12)) + I(cos(12*pi*t/12)), data = data_bel_in )
mt3 = lm( BEL ~ t + I(t^2) + I(t^3) +
            I(sin(2*pi*t/12)) + I(cos(2*pi*t/12)) +
            I(sin(4*pi*t/12)) + I(cos(4*pi*t/12)) +
            I(sin(6*pi*t/12)) + I(cos(6*pi*t/12)) +
            I(sin(8*pi*t/12)) + I(cos(8*pi*t/12)) +
            I(sin(10*pi*t/12)) + I(cos(10*pi*t/12)) +
            I(sin(12*pi*t/12)) + I(cos(12*pi*t/12)), data = data_bel_in )
checkresiduals( mt1 )
checkresiduals( mt2 )
checkresiduals( mt3 )

plot( bel_in, lwd = 2 )
lines( ts(predict(mt1), start = start(bel_in), frequency = frequency(bel_in)),
       col = 'magenta', lwd = 2 )
lines( ts(predict(mt2), start = start(bel_in), frequency = frequency(bel_in)),
       col = 'cyan', lwd = 2 )
lines( ts(predict(mt3), start = start(bel_in), frequency = frequency(bel_in)),
       col = 'green', lwd = 2 )

c( BIC(mt1), BIC(mt2), BIC(mt3) )
c( summary(mt1)$r.squared, summary(mt2)$r.squared, summary(mt3)$r.squared )


###
# 3) ETS model
###

mETS = ets( bel_in, model = 'ZZZ', opt.crit = 'lik', ic = 'bic' )
checkresiduals( mETS )

plot( bel_in, lwd = 2 )
lines( mETS$fitted, col = 'red', lwd = 2)
BIC( mETS )


###
# 4) BATS model
###

mBATS = bats( bel_in )
checkresiduals( mBATS )

plot( bel_in, lwd = 2 )
lines( mBATS$fitted, col = 'red', lwd = 2 )


###
# 5) TBATS model
###

mTBATS = tbats( bel_in )
checkresiduals( mTBATS )

plot( bel_in, lwd = 2 )
lines( mTBATS$fitted, col = 'red', lwd = 2 )


###
# COMPARISON
###

plot( bel_in, lwd = 2 )
lines( mETS$fitted, col = 'orange', lwd = 2 )
lines( mBATS$fitted, col = 'green', lwd = 2 )
lines( mTBATS$fitted, col = 'blue', lwd = 2 )


###
# 6) FORECAST
###

mETS_forecast = forecast( mETS, h = 12 )
mBATS_forecast = forecast( mBATS, h = 12 )
mTBATS_forecast = forecast( mTBATS, h = 12 )

plot( bel_out, lwd = 2, ylim = c(200, 400) )
lines( mETS_forecast$mean, lwd = 2, col = 'orange' )
lines( mETS_forecast$lower[,2], lwd = 2, col = 'orange', lty = 2 )
lines( mETS_forecast$upper[,2], lwd = 2, col = 'orange', lty = 2 )
lines( mBATS_forecast$mean, lwd = 2, col = 'magenta' )
lines( mBATS_forecast$lower[,2], lwd = 2, col = 'magenta', lty = 2 )
lines( mBATS_forecast$upper[,2], lwd = 2, col = 'magenta', lty = 2 )
lines( mTBATS_forecast$mean, lwd = 2, col = 'cyan' )
lines( mTBATS_forecast$lower[,2], lwd = 2, col = 'cyan', lty = 2 )
lines( mTBATS_forecast$upper[,2], lwd = 2, col = 'cyan', lty = 2 )

accuracy(mETS_forecast, bel_out)
accuracy(mBATS_forecast, bel_out)
accuracy(mTBATS_forecast, bel_out)
