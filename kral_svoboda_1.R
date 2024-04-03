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

attach(data_bel_in)

m0 = lm( BEL ~ 1 + dummy )
checkresiduals( m0 )

m1 = lm( BEL ~ t + dummy )
checkresiduals( m1 )

m2 = lm( BEL ~ t + I(t^2) + dummy )
checkresiduals( m2 )

plot( bel_in, lwd = 2 )
lines( ts(predict(m0), start = start(bel_in), frequency = frequency(bel_in)),
       col = "yellow", lwd = 2 )
lines( ts(predict(m1), start = start(bel_in), frequency = frequency(bel_in)),
       col = "magenta", lwd = 2 )
lines( ts(predict(m2), start = start(bel_in), frequency = frequency(bel_in)),
       col = "cyan", lwd = 2 )

# compare BIC and adjusted R^2
c( BIC(m0), BIC(m1), BIC(m2) )
c( summary(m0)$r.squared, summary(m1)$r.squared, summary(m2)$r.squared )
# both metrics prefer m2

###
# 2) TRIGONOMETRIC SEASONALITY
###

