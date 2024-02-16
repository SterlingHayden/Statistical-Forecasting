####
#### ARIMA examples
####

###
### Unemployment data
###

## install.packages("urca")
library(urca)

data(npext)
str(npext)
unemploy <- ts(npext$unemploy, start = npext$year[1])
unemploy
unemploy <- window(unemploy, start = 1890)

### split into training set and test set
length(unemploy)
end(unemploy)
y <- window(unemploy, end = 1978)
test <- window(unemploy, start = 1979)

### try fit an ARIMA model to the training data, 'y'
acf(y, lag = 50, main = "ACF of log unemployment rate")
pacf(y, lag = 50, main = "PACF of log unemployment rate")

## AR 1
ar1 <- arima(y, order = c(1, 0, 0))
tsdiag(ar1)

## AR 2
ar2 <- arima(y, order = c(2, 0, 0))
tsdiag(ar2)

## AR 3
ar3 <- arima(y, order = c(3, 0, 0))
tsdiag(ar3)

## AR 4
ar4 <- arima(y, order = c(4, 0, 0))
tsdiag(ar4)

## Model selection based on AIC
AIC(ar2, ar3, ar4)

## Check Normality of residuals of selected model
qqnorm(ar4$resid); qqline(ar4$resid)
shapiro.test(ar4$resid)

### Calculate forecasts...
forecast <- predict(ar4, n.ahead = 10, se.fit = TRUE)

### ...and plot them together with prediction bands and original data
x <- cbind(y, test, forecast$pred, forecast$pred - 1.96 * forecast$se,
           forecast$pred + 1.96 * forecast$se)
plot(x, plot.type = "s", lty = rep(c("solid", "longdash"), c(2, 3)), lwd = 2, 
     col = c("black","black","red","blue","blue"), main = "Log unemployment rate",
     ylab = "", xlab = "Year")
abline(v = 1978.5, lty = 3)
legend("topleft", col = c("blue", "red"), legend = c("Prediction Interval", "Point Forecast"),
       lty = "longdash", lwd = 2, cex = .75, seg.len = 3, bty = "n")

### Compute RMSE of the forecasts
sqrt(mean((forecast$pred - test)^2))

###
### Overseas visitors to New Zeland
###

osvisit <- ts(scan("D:/STAT 4013/osvisit.csv"), start = c(1977, 1), freq = 12)
y <- window(osvisit, end = c(1993, 12)) # training set
test <- window(osvisit, start = c(1994, 1)) # test set

plot(y)
plot(log(y))
plot(sqrt(y))
x <- sqrt(y)
x <- log(y)

### Determine the possible order of the model
acf(x, lag = 36)
acf(diff(x), lag = 36)
acf(diff(diff(x, lag = 12)), lag = 36)
pacf(diff(diff(x, lag = 12)), lag = 36)

### Fit an ARIMA model

## ARIMA(0, 1, 1)(0, 1, 1)
mod1 <- arima(x, order = c(0, 1, 1), seas = list(order = c(0, 1, 1), freq = 12))
mod1
tsdiag(mod1)
acf(mod1$resid)
pacf(mod1$resid)

## ARIMA(1, 1, 1)(0, 1, 1)
mod2 <- arima(x, order = c(1, 1, 1), seas = list(order = c(0, 1, 1), freq = 12))
mod2
tsdiag(mod2)
shapiro.test(mod2$resid)

## ARIMA(1, 1, 1)(1, 1, 1) - suggested by the textbook for sqrt transformation
mod3 <- arima(x, order = c(1, 1, 1), seas = list(order = c(1, 1, 1), freq = 12))
mod3
tsdiag(mod3)
shapiro.test(mod3$resid)

## Compare models
AIC(mod2, mod3)

### Calculate forecasts
forecast <- predict(mod2, n.ahead = 24)

### ...and plot them together with prediction bands and original data,
### undoing the log transformation
z <- cbind(y, test, exp(forecast$pred), exp(forecast$pred - 1.96 * forecast$se),
           exp(forecast$pred + 1.96 * forecast$se))
plot(window(z, start = c(1992, 1)), plot.type = "s", lty = rep(c("solid", "longdash"), c(2, 3)), lwd = 2, 
     col = c("black","black","red","blue","blue"), main = "Visitors to NZ",
     ylab = "", xlab = "Year")
abline(v = 1994, lty = 3)
legend("topleft", col = c("blue", "red"), legend = c("Prediction Interval", "Point Forecast"),
       lty = "longdash", lwd = 2, cex = .75, seg.len = 3, bty = "n")

### Compute RMSE of the forecasts
sqrt(mean((exp(forecast$pred) - test)^2))

forecast3 <- predict(mod3, n.ahead = 24)
sqrt(mean((exp(forecast3$pred) - test)^2))

### MAD
mean(abs(exp(forecast$pred) - test))
mean(abs(exp(forecast3$pred) - test))

### MAPE
mean(abs(exp(forecast$pred) - test) / test) * 100
mean(abs(exp(forecast3$pred) - test) / test) * 100



