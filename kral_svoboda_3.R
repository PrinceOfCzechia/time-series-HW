rm(list = ls())

library(tseries)
library(forecast)
library(rugarch)

###
# init procedures
###

# loading the raw data
data = read.csv('TSM.csv', stringsAsFactors = TRUE)
tsm = ts(data$Adj.Close)
data$Date = as.Date(data$Date)
plot(data$Date, tsm, type='l', ylab='$TSM adjusted closing price',
     main = 'Daily closing price of $TSM adjusted for dividends', grid = TRUE)
grid()
axis.Date(1, at=seq(as.Date("2017-01-01"), as.Date("2024-03-28"), by="years"), format="%Y")

# log-returns series
lr = diff(log(tsm))
plot(data$Date[-1], lr, type='l', ylab = 'log-returns of $TSM',
     xlab = 'Time', main = 'Daily log returns of $TSM adjsuted for dividends') # we can observe typical behavior of financial series
grid()
axis.Date(1, at=seq(as.Date("2017-01-01"), as.Date("2024-03-28"), by="years"), format="%Y")
acf(lr) # looks fine


###
# ARIMA model
###
m_arima = auto.arima(lr, allowmean = FALSE)
summary(m_arima) # we will use ARMA(2,2) as the mean structure
checkresiduals(m_arima) # uncorrelated with p=0.4, heteroscedastic, slightly leptocurtic
Box.test(m_arima$residuals^2, type='Ljung-Box') # heteroscedastic with p~0 => GARCH


###
# benchmark GARCH(1,1) model
###
mg1_spec = ugarchspec(variance.model = list(model = 'sGARCH',
                                       garchOrder = c(1,1),
                                       submodel = NULL,
                                       external.regressors = NULL,
                                       variance.targeting = FALSE),
                      mean.model = list(armaOrder = c(2,2),
                                   include.mean = FALSE,
                                   external.regressors = NULL),
                      distribution.model = 'std', # t
                      start.pars = list(),
                      fixed.pars = list())
mg1 = ugarchfit(spec = mg1_spec, data = lr)
print(mg1)
benchmark_AIC = infocriteria(mg1)[1]

###
# searching for a better sGARCH model
###
best_model = mg1
for (m in 1:5)
{
  for (s in 1:5)
  {
    mg2_spec = ugarchspec(variance.model = list(model = 'sGARCH',
                                                 garchOrder = c(m, s),
                                                 submodel = NULL,
                                                 external.regressors = NULL,
                                                 variance.targeting = FALSE),
                          mean.model = list(armaOrder = c(2, 2),
                                             include.mean = FALSE,
                                             external.regressors = NULL),
                          distribution.model = 'std',
                          start.pars = list(),
                          fixed.pars = list())

    mg2 = ugarchfit(spec = mg2_spec, data = lr)
    aic = infocriteria(mg2)[1]
    
    if (aic < benchmark_AIC)
    {
      benchmark_AIC = aic
      best_model = mg2
    }
  }
}
# sGARCH(2,4) emerged victorious with AIC = -5.1488
benchmark_AIC
print(best_model)


###
# extension to gjrGARCH
###
best_model = mg2
for (m in 1:5)
{
  for (s in 1:5)
  {
    gjr_spec = ugarchspec(variance.model = list(model = 'gjrGARCH',
                                                garchOrder = c(m, s),
                                                submodel = NULL,
                                                external.regressors = NULL,
                                                variance.targeting = FALSE),
                          mean.model = list(armaOrder = c(2, 2),
                                            include.mean = FALSE,
                                            external.regressors = NULL),
                          distribution.model = 'std',
                          start.pars = list(),
                          fixed.pars = list())
    
    gjr = ugarchfit(spec = gjr_spec, data = lr)
    aic = infocriteria(gjr)[1]
    
    if (aic < benchmark_AIC)
    {
      benchmark_AIC = aic
      best_model = gjr
    }
  }
}
# gjrGARCH(2,4) improved to AIC = -5.1495
benchmark_AIC
print(best_model)

gjr = best_model
plot(gjr)
