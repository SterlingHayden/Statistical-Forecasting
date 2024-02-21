# importing the quantmod library, which allows for me to pull stock data from yahoo 
library(quantmod)
library(forecast)


# Downloading 3 years ago to current date's TQQQ data, TQQQ is the 3X leveraged version of QQQ
df <- getSymbols('TQQQ',src='yahoo', from = Sys.Date() - 365*3, to = Sys.Date(), auto.assign=FALSE)


# Lets take a look at our df
head(df) # we see that our data is formatted daily and that we have 6 features
nrow(df) # our data spans 753 trading days


# Lets now plot the data
plot.ts(df$TQQQ.Close, main = 'TQQQ Stock Price') # we are plotting the daily close price of TQQQ; similar to what you would see in your brokerage account
## I'm not a fan of this graph, lets use quantmods built in plot
chart_Series(df$TQQQ.Close,name="TQQQ Stock Price")
## the variance does not look to be stable.
## lets try to see if log will stabilize the variance
chart_Series(log(df$TQQQ.Close),name="LOG TQQQ Stock Price") # looks pretty stable to me
## Lets check if a square root transformation is better
chart_Series(sqrt(df$TQQQ.Close),name="SQRT TQQQ Stock Price") # does not as good as log
# we will be using the log transformation as it seems to stabilize the data well
df_log <- sqrt(df$TQQQ.Close)


# Checking autocorrelation 
par(mfrow=c(2,1))
acf_log <- acf(df_log, lag.max = 300, main = 'ACF of df_log')
#decreases up to lag ~170 and then the autocorrelation starts reversing
pacf_log <- pacf(df_log, lag.max = 300, main = 'PACF of df_log')
## we see significant autocorrelation 


#########################################################################
###################### How to make the model by hand ####################
# Differencing 
df_diff <- diff(df_log, lag=1, diff=1)
df_diff <- na.locf(df_diff, na.rm = TRUE,
                     fromLast = TRUE)


# param selection
par(mfrow=c(2,1))
acf(df_diff,main="ACF of first regular diff of training data")
pacf(df_diff,main="PACF of first regular diff of training data")
## given the plots we would select p=0, and q=0
## the autocorrelations look much more manageable now after being differenced 


# Hand made model
arima(training, order = c(0, 1, 0))


#########################################################################
# Alternatively we can fit the ARIMA model using auto.arima
modelfit <- auto.arima(df_log, stationary = FALSE) #auto.arima() automatically finds the best ARIMA(p,d,q)(P,D,Q) 
summary(modelfit)
# we see the same model

# Check residuals of the model with ARIMA parameters chosen
plot(resid(modelfit),ylab="Residuals",main="Residuals(Arima(0,1,0)) vs. Time")
## check if our residuals fit under a bell curve
hist(resid(modelfit),freq=F,ylim=c(0,10),main="Histogram of Residuals")
e=resid(modelfit)
curve(dnorm(x, mean=mean(e), sd=sd(e)), add=TRUE, col="blue")
## Ljung-Box statistic
tsdiag(modelfit)
## the Ljung-Box plot indicates that there are no autocorrelations in the model 
Box.test(resid(modelfit), type = "Ljung")
## our p-value of 0.8227 > 0.05, thus we fail to reject the null


# ARIMA Forecast
plot(forecast(modelfit,h=30), ylab= 'log(Close Price) in $')
## blue line = mean, blue C.I. = 80%, grey C.I. = 95%
# log nums by day
log_forecast <- forecast(modelfit,h=30)
log_forecast
