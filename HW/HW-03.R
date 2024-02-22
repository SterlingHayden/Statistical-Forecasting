### Sterling Hayden

### 6.16

### a)
os <- read.csv('CH6-CODE-DATA-BASER/osvisit.csv')
os <- ts(data=os, start = c(1977, 1), freq = 12)
# training set
y <- window(os, start=c(1977,1), end=c(1994,12)) 
# test set
test <- window(osvisit, start = c(1995, 1), end=c(1995,12))
# transform
x <- sqrt(y)


## ARIMA(1, 1, 0)(0, 1, 0)
mod1 <- arima(x, order = c(1, 1, 0), seas = list(order = c(0, 1, 0), freq = 12))
mod1
Mod(polyroot(c(1,-0.3991)))
# 2.505638


## ARIMA(0, 1, 0)(1, 1, 0)
mod2 <- arima(x, order = c(0, 1, 0), seas = list(order = c(1, 1, 0), freq = 12))
mod2
Mod(polyroot(c(1,-0.3437)))
# 2.909514


## ARIMA(1, 1, 0)(1, 1, 0)
mod3 <- arima(x, order = c(1, 1, 0), seas = list(order = c(1, 1, 0), freq = 12))
mod3
Mod(polyroot(c(1,-0.3711)))
# 2.694691
Mod(polyroot(c(1,-0.3114)))
# 3.211304

## ARIMA(0, 1, 1)(0, 1, 1)
mod4 <- arima(x, order = c(0, 1, 1), seas = list(order = c(0, 1, 1), freq = 12))
mod4
Mod(polyroot(c(1,-0.4067)))
# 2.458815
Mod(polyroot(c(1,-0.6334)))
# 1.578781

## ARIMA(1, 1, 0)(0, 1, 1)
mod5 <- arima(x, order = c(1, 1, 0), seas = list(order = c(0, 1, 1), freq = 12))
mod5
Mod(polyroot(c(1,-0.3628)))
# 2.75634
Mod(polyroot(c(1,-0.4649)))
# 2.151

## ARIMA(0, 1, 1)(1, 1, 0)
mod6 <- arima(x, order = c(0, 1, 1), seas = list(order = c(1, 1, 0), freq = 12))
mod6
Mod(polyroot(c(1,-0.2902)))
# 3.445899
Mod(polyroot(c(1,-0.6651)))
# 1.503533

## ARIMA(1, 1, 1)(1, 1, 1)
mod7 <- arima(x, order = c(1, 1, 1), seas = list(order = c(1, 1, 1), freq = 12))
mod7
Mod(polyroot(c(1, 0.2078)))
# 4.81232
Mod(polyroot(c(1, 0.284)))
# 3.521127
Mod(polyroot(c(1,-0.6175)))
# 1.619433
Mod(polyroot(c(1,-0.8338)))
# 1.199328

## ARIMA(1, 1, 1)(0, 1, 1)
mod8 <- arima(x, order = c(1, 1, 1), seas = list(order = c(0, 1, 1), freq = 12))
mod8
Mod(polyroot(c(1, 0.292)))
# 3.424658
Mod(polyroot(c(1,-0.439)))
# 2.277904
Mod(polyroot(c(1,-0.8338)))
# 1.199328



### b)
acf(resid(mod1), lag=50, cex=0.5,lwd=2)
Box.test(resid(mod1), lag = 12, type = "Ljung")
# not WN

acf(resid(mod2), lag=50, cex=0.5,lwd=2)
Box.test(resid(mod2), lag = 12, type = "Ljung")
# not WN

acf(resid(mod3), lag=50, cex=0.5,lwd=2)
Box.test(resid(mod3), lag = 12, type = "Ljung")
# Not WN

acf(resid(mod4), lag=50, cex=0.5,lwd=2)
Box.test(resid(mod4), lag = 12, type = "Ljung")
# not WN

acf(resid(mod5), lag=50, cex=0.5,lwd=2)
Box.test(resid(mod5), lag = 12, type = "Ljung")
# not WN

acf(resid(mod6), lag=50, cex=0.5,lwd=2)
Box.test(resid(mod6), lag = 12, type = "Ljung")
# not WN

acf(resid(mod7), lag=50, cex=0.5,lwd=2)
Box.test(resid(mod7), lag = 12, type = "Ljung")
# is WN

acf(resid(mod8), lag=50, cex=0.5,lwd=2)
Box.test(resid(mod8), lag = 12, type = "Ljung")
# almost WN depending on the alpha level



### c)
AIC(mod7, mod8)
# mod7 AIC = 1415.504
# mod8 AIC = 1415.157

# mod8 (ARIMA(1, 1, 1)(0, 1, 1)) has the lowest AIC


# calculating RMSE
osvisit.test = window(sqrt(os), start = c(1995,1), end = c(1995, 12))
test1 = ts(osvisit.test, start = c(1995, 1), frequency = 12)

fore7 = predict(mod7, n.ahead = 12, se.fit = TRUE)
rsme7 = sqrt(mean((fore7$pred^2 - test1^2)^2))
rsme7
# 5637.484

fore8 = predict(mod8, n.ahead = 12, se.fit = TRUE)
rsme8 = sqrt(mean((fore8$pred^2 - test1^2)^2))
rsme8
# 5618.592


### d)
mod1
# 79.36
mod2
# 82.72
mod3
# 71.61
mod4
# 61.34
mod5
# 67.53
mod6
# 63.54 
mod7
# 58.38
mod8
# 58.98


### e) 
# ARIMA(1, 1, 1)(1, 1, 1) has the lowest sigma^2.


### f)
# ARIMA(1, 1, 1)(0, 1, 1) has the lowest RMSE.


### g)
# ARIMA(1, 1, 1)(0, 1, 1) has the lowest AIC.





