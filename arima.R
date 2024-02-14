# install.packages("astsa")
library(astsa) # for some data sets

### GDP data
str(gdp)
frequency(gdp)
plot(gdp, main = "U.S. GDP")


x <- diff(log(gdp))
plot(x, main = "U.S. GDP - percentage change")

x.ar <- arima(x, order = c(1, 0, 0))
x.ar
tsdiag(x.ar, gof.lag = 20)

x.ma <- arima(x, order = c(0, 0, 2))
x.ma
tsdiag(x.ma, gof.lag = 20)
hist(x.ma$resid) # not the best way to assess Normality
qqnorm(x.ma$resid); qqline(x.ma$resid)
shapiro.test(x.ma$resid) # test Normality of residuals

### Varve data - sedimentary deposit
plot(varve)
plot(log(varve))
plot(diff(log(varve)))

x <- log(varve)
acf(diff(x))
pacf(diff(x))

x.ma <- arima(x, order = c(0, 1, 1))
x.ma
tsdiag(x.ma, gof.lag = 20)

## Fit ARIMA(1,1,1)
x.arma1 <- arima(x, order = c(1, 1, 1))
x.arma1
tsdiag(x.arma1, gof.lag = 20)
qqnorm(x.arma1$resid); qqline(x.arma1$resid)

## Fit ARIMA(0,1,2)
x.arma2 <- arima(x, order = c(0, 1, 2))
x.arma2
tsdiag(x.arma2, gof.lag = 20)
qqnorm(x.arma2$resid); qqline(x.arma2$resid)

## Model selection
AIC(x.arma1)
AIC(x.arma2)
BIC(x.arma1)
BIC(x.arma2)
