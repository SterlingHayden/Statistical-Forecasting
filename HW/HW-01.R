######
###### R code to solve the exercises of homework 1
######

####
#### Exercise 2.17
####
alpha <- 0.2
y <- c(362, 314, 365)
a <- 321

y_forecast <- numeric(3)
y_forecast[1] <- a
for (i in 2:3) {
    ## update 'a'
    a <- alpha * y[i] + (1 - alpha) * a
    ## store forecasted value
    y_forecast[i] <- a
}
y_forecast[1:2] # predictions for the next two periods
y_forecast[3] # out-of-sample forecast value

####
#### Exercise 2.24
####

JJtrain <- window(JohnsonJohnson, end = c(1977, 4))
JJtest <- window(JohnsonJohnson, start = c(1978, 1))

### (a) Use Holt-Winters to forecast 3 years ahead
ES <- HoltWinters(JJtrain)
JJforecast <- predict(ES, n.ahead = 12)
JJforecast

### (b) Produce a plot like Figure 2.12 and compute RMSE
## pdf("Exercise_2.24b.pdf", width = 6, height = 6)
plot(JohnsonJohnson, type = "o", pch = 20, 
     main = "Holt-Winters exponential smoother",
     ylab = "J&J earnings", xlab = "Year")
lines(ES$fitted[, 1], lty = 2, col = "red", lwd = 2)
lines(JJforecast, lty = 5, col = "blue", lwd = 2)
abline(v = 1978, lty = "dotted")
legend("topleft", bty = "n",
       legend = c("data (training and test)", "fitted in-sample",
                  "forecast out-of-sample"), 
       lty = c(1, 2, 5), col = c("black", "red", "blue"))

RMSE <- sqrt(mean((JJforecast - JJtest)^2))
RMSE

### (c) Produce a plot like Figure 2.13
## pdf("Exercise_2.24c.pdf", width = 6, height = 6)
plot(fitted(ES), main = "")
title(main = "Holt-Winters decomposition of\n Johnson & Johnson earnings")

### (d) Regression smoother
Qrt <- factor(cycle(JJtrain))
time <- time(JJtrain)
Reg <- lm(JJtrain ~ time + Qrt)
Reg_forecast <- predict(Reg, newdata = data.frame(time = time(JJtest), Qrt = factor(cycle(JJtest))))
RMSE_reg <- sqrt(mean((Reg_forecast - JJtest)^2))
RMSE_reg
RMSE

### (e) Consensus forecast
consensus <- 0.5 * (Reg_forecast + JJforecast)
RMSE_cons <- sqrt(mean((consensus - JJtest)^2))
RMSE_cons
RMSE_reg
RMSE
