######
###### Automatic model selection
######

## install.packages("fable")
## fable:::ARIMA # for a 'tidy' version

## install.packages(c("fpp2", "forecast", "expsmooth"))
library(fpp2)
library(forecast)
library(expsmooth) # for the data

### Monthly manufacture of electrical equipment in the Euro zone
plot(elecequip, type = "o", pch = 20, ylab = "Index", 
     main = "Monthly manufacture of electrical equipment")
adj <- stl(elecequip, s.window = "periodic")
eeadj <- elecequip - adj$time.series[,"seasonal"]
plot(eeadj, type = "o", pch = 20, ylab = "Index", 
     main = "Monthly manufacture of electrical equipment - Seasonally adjusted")

## Difference the data to achieve stationarity
plot(diff(eeadj))
acf(diff(eeadj))
pacf(diff(eeadj))

## Fit plausible ARIMA(p,d,0) and ARIMA(0,d,q)
arima013 <- arima(eeadj, order = c(0,1,3))
arima016 <- arima(eeadj, order = c(0,1,6))
arima310 <- arima(eeadj, order = c(3,1,0))
AIC(arima013, arima016, arima310)

## Check residuals


## Manual automatic procedure
## Explore several ARIMA(p,1,q) models and select the 'best'
P <- 6; Q <- 6
aicS <- matrix(Inf, P+1, Q+1)
dimnames(aicS) <- list(p = 0:P, q = 0:Q)
for (p in 0:P) 
  for (q in 0:Q) 
    if (p +  q > 0) {
      model <- arima(eeadj, order = c(p, 1, q))
      aicS[p+1, q+1] <- AIC(model)
    }

for (p in 0:P) 
  for (q in 0:Q) 
    if (p +  q > 0) {
      model <- try(arima(eeadj, order = c(p, 1, q)))
      if(!inherits(model, "try-error")) aicS[p+1, q+1] <- AIC(model)
    }

aicS
## locate the model having minimum AIC
(cbind(as.vector(row(aicS)), as.vector(col(aicS))) - 1)[which.min(aicS), ]


## Automatic procedure
autofit <- auto.arima(eeadj, seasonal = FALSE)


### Example: Monthly bond yield
plot(bonds, type = "o", pch = 20, main = "Monthly US government bond yield")
aafit1 <- auto.arima(bonds, max.P = 0, max.Q = 0, D = 0, approximation = FALSE)

## check residuals
tsdiag(aafit1)
shapiro.test(residuals(aafit1))

## calculate forecasts
fcast1 <- forecast(aafit1)
## plot the forecasts
plot(fcast1)

### Example: Annual US net electricity generation
plot(usnetelec, type = "o", pch = 20, main = "US net electricity generation") 
aafit2 <- auto.arima(usnetelec)

## check residuals
tsdiag(aafit2)
shapiro.test(residuals(aafit2))

## calculate forecasts
fcast2 <- forecast(aafit2, bootstrap = TRUE, h = 30)
## plot the forecasts
plot(fcast2)

### Example 3: Quarterly passenger motorvehicle production in the UK
plot(ukcars, type = "o", pch = 20, main = "UK passenger motorvehicle production",
     ylab = "Thousands of units")
aafit3 <- auto.arima(ukcars)

## check residuals
tsdiag(aafit3)
shapiro.test(residuals(aafit3))

## calculate forecasts
fcast3 <- forecast(aafit3)
## plot the forecasts
plot(fcast3)

### Example 4: Monthly number of short term visitors to Australia
plot(visitors, type = "o", pch = 20, main = "Number of visitors to Australia")
plot(log(visitors), type = "o", pch = 20, main = "Number of visitors to Australia (log scale)")
aafit4 <- auto.arima(log(visitors))

## check residuals
tsdiag(aafit4)
shapiro.test(residuals(aafit4))

## calculate forecasts
fcast4 <- forecast(aafit4)
## plot the forecasts
plot(fcast4)
