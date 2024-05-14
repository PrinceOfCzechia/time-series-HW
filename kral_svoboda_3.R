rm(list = ls())

library(tseries)
library(forecast)
library(rugarch)
library(lmtest)

###
# we strongly discourage the reviewers to run the for cycles
# instead just fit an ARMA(2,1) GARCH(2,4) model manually
# e.g. by restricting the bounds of the for loops
###

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
axis.Date(1, at=seq(as.Date('2017-01-01'), as.Date('2024-03-28'), by='years'), format='%Y')

# log-returns series
lr = diff(log(tsm))
plot(data$Date[-1], lr, type='l', ylab = 'log-returns of $TSM',
     xlab = 'Time', main = 'Daily log returns of $TSM adjsuted for dividends') # we can observe typical behavior of financial series
grid()
axis.Date(1, at=seq(as.Date('2017-01-01'), as.Date('2024-03-28'), by='years'), format='%Y')
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
best_AIC = infocriteria(mg1)[1]

###
# searching for a better sGARCH model
###
best_model = mg1
for (m in 1:7)
{
  for (s in 1:7)
  {
    for (p in 1:2)
    {
      for (q in 1:2)
      {
        mg2_spec = ugarchspec(variance.model = list(model = 'sGARCH',
                                                     garchOrder = c(m, s),
                                                     submodel = NULL,
                                                     external.regressors = NULL,
                                                     variance.targeting = FALSE),
                              mean.model = list(armaOrder = c(p, q),
                                                 include.mean = FALSE,
                                                 external.regressors = NULL),
                              distribution.model = 'std',
                              start.pars = list(),
                              fixed.pars = list())
    
        mg2 = ugarchfit(spec = mg2_spec, data = lr)
        aic = infocriteria(mg2)[1]
        
        if (aic < best_AIC)
        {
          best_AIC = aic
          best_model = mg2
        }
      }
    }
  }
}
# ARMA(2,1) sGARCH(2,4) emerged victorious with AIC = -5.1495
best_AIC
print(best_model)
mg2 = best_model

###
# extension to gjrGARCH
###
best_model = mg2
for (m in 1:7)
{
  for (s in 1:7)
  {
    for (p in 1:2)
    {
      for (q in 1:2)
      {
        gjr_spec = ugarchspec(variance.model = list(model = 'gjrGARCH',
                                                    garchOrder = c(m, s),
                                                    submodel = NULL,
                                                    external.regressors = NULL,
                                                    variance.targeting = FALSE),
                              mean.model = list(armaOrder = c(p, q),
                                                include.mean = FALSE,
                                                external.regressors = NULL),
                              distribution.model = 'std',
                              start.pars = list(),
                              fixed.pars = list())
        
        gjr = ugarchfit(spec = gjr_spec, data = lr)
        aic = infocriteria(gjr)[1]
        
        if (aic < best_AIC)
        {
          best_AIC = aic
          best_model = gjr
        }
      }
    }
  }
}
# gjrGARCH(2,4) with ARMA(2,1) improved to AIC = -5.1501
best_AIC
print(best_model)

gjr = best_model
gjr_res = gjr@fit$z
plot(gjr, which = 2) # expected returns
plot(gjr, which = 3) # expected sigma, same plot on the next lines, but better
plot(data$Date[-1], 10*abs(gjr@fit$fitted.values), type='l', col='lightgray',
     lwd=1, ylim=c(0.00,0.10), xlab='Time', ylab='Expected sigma',
     main='Expected sigma with mean against |expected returns|')
abline(h=mean(gjr@fit$sigma), col='red', lty=2)
lines(data$Date[-1], gjr@fit$sigma, type='l', col='dodgerblue2')
axis.Date(1, at=seq(as.Date('2017-01-01'), as.Date('2024-03-28'), by='years'), format='%Y')
legend('topleft', legend=c('xSigma', 'Mean sigma', '|xReturns|'), 
       col=c('dodgerblue2', 'red', 'lightgray'), lty=c(1, 2, 1), lwd=c(2, 2, 2))
plot(gjr, which = 8) # t density looks accurate
plot(gjr, which = 9) # still looks accurate
plot(gjr, which = 10) # residuals appear uncorrelated
plot(gjr, which = 11) # and also homoscedastic

Box.test(gjr_res, type='Ljung-Box') # uncorrelatednes not rejected
Box.test(gjr_res^2, type='Ljung-Box') # homoscedasticity not rejected


###
# comparison of model predictions
###

# sGARCH(1,1)
mg1_pred = ugarchboot(mg1, method='Full', n.ahead=5, n.bootpred=1000, n.bootfit=1000)
print(mg1_pred)

plot(mg1_pred, which = 2) # expected returns
plot(mg1_pred, which = 3) # expected sigma

# sGARCH(2,4)
mg2_pred = ugarchboot(mg2, method='Full', n.ahead=5, n.bootpred=1000, n.bootfit=1000)
print(mg2_pred)

plot(mg2_pred, which = 2) # expected returns
plot(mg2_pred, which = 3) # expected sigma

# gjrGARCH(2,4)
gjr_pred = ugarchboot(gjr, method='Full', n.ahead=5, n.bootpred=1000, n.bootfit=1000)
print(gjr_pred)

plot(gjr_pred, which = 2) # expected returns
plot(gjr_pred, which = 3) # expected sigma

# together
plot(gjr@fit$sigma[700:1200], type='l', col='magenta', lwd=3,
     xlab='Time index', ylab='Expected sigma',
     main='Expected volatility from different models')
lines(mg2@fit$sigma[700:1200], col='blue3', lwd=3)
lines(mg1@fit$sigma[700:1200], col='darkgoldenrod1', lwd=3)
legend('topright', legend=c('gjrGARCH(2,4)', 'sGARCH(2,4)', 'sGARCH(1,1)'), 
       col=c('magenta', 'blue3', 'darkgoldenrod1'), lwd=c(3, 3, 3))


par(mfrow=c(1,3)) # returns head to head
plot(mg1_pred, which = 2)
plot(mg2_pred, which = 2)
plot(gjr_pred, which = 2)
par(mfrow=c(1,1))

par(mfrow=c(1,3)) # sigma head to head
plot(mg1_pred, which = 3)
plot(mg2_pred, which = 3)
plot(gjr_pred, which = 3)
par(mfrow=c(1,1))
