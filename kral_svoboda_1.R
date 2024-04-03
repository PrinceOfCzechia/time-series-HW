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

plot( bel_in, lwd = 2 )
lines( ts(predict(md0), start = start(bel_in), frequency = frequency(bel_in)),
       col = 'yellow1', lwd = 2 )
lines( ts(predict(md1), start = start(bel_in), frequency = frequency(bel_in)),
       col = 'magenta', lwd = 2 )
lines( ts(predict(md2), start = start(bel_in), frequency = frequency(bel_in)),
       col = 'cyan', lwd = 2 )

# compare BIC and adjusted R^2
c( BIC(md0), BIC(md1), BIC(md2) )
c( summary(md0)$r.squared, summary(md1)$r.squared, summary(md2)$r.squared )
# both metrics prefer md2


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
BIC(mt)
plot( bel_in, lwd = 2 )
lines( ts(predict(mt1), start = start(bel_in), frequency = frequency(bel_in)),
       col = 'magenta', lwd = 2 )
lines( ts(predict(mt2), start = start(bel_in), frequency = frequency(bel_in)),
       col = 'cyan', lwd = 2 )
