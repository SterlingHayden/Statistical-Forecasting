######
###### Forecasting using Holt-Winters method
######

### Nile river level
plot(Nile, type = "o", pch = 20)
Nile.HW <- HoltWinters(Nile, beta = FALSE, gamma = FALSE)
Nile.HW
plot(Nile.HW, type = "o", pch = 20)
fitted(Nile.HW)
str(Nile.HW)
## specifying 'alpha'
Nile.HW1 <- HoltWinters(Nile, alpha = .95, beta = FALSE, gamma = FALSE)
plot(Nile.HW1, type = "o", pch = 20)
Nile.HW2 <- HoltWinters(Nile, alpha = .05, beta = FALSE, gamma = FALSE)
plot(Nile.HW2, type = "o", pch = 20)

### Thermostat sales
# thermo <- scan("R_code_book/CH2-CODE-DATA-BASER/thermostat.txt")
thermo <- scan("D:/STAT 4013/thermostat.txt")
thermo <- ts(thermo, start = c(1972, 1), freq = 52)
plot(thermo, type = "o", pch = 20, main = "Thermostat sales")
thermo.HW1 <- HoltWinters(thermo, gamma = FALSE)
thermo.HW1
str(thermo.HW1)
plot(thermo.HW1, type = "o", pch = 20, main = "Weekly thermostat sales\nObserved and Predicted")

## out of sample predictions
pred <- predict(thermo.HW1, 12)
plot(thermo.HW1, pred, type = "o", pch = 20)

## RMSE
sqrt(thermo.HW1$SSE / length(thermo))

## Compare with Holt-Winters without trend
thermo.HW2 <- HoltWinters(thermo, beta = FALSE, gamma = FALSE)
thermo.HW2
## RMSE
sqrt(thermo.HW2$SSE / length(thermo))

### CO2 at Mauna Loa
plot(co2, type = "o", pch = 20, cex = 0.75, main = "CO2 concentration")
co2_train <- window(co2, end = c(1993, 12))
co2_test <- window(co2, start = c(1994, 1))
## fit Holt-Winters parameters to training data
co2.HW <- HoltWinters(co2_train)
co2.HW
## compute out-of-sample predictions
co2_pred <- predict(co2.HW, 48)
## plot test data, part of the training data, and out-of-sample predictions
plot(window(co2, start = c(1990, 1)), type = "o", pch = 20,
     main = "CO2 - out of sample predictions", ylab = "co2")
points(co2_pred, col = "tomato", type = "o", pch = 20)
abline(v = start(co2_pred)[1] - 1/24, col = "green", lty = "longdash")
legend("topleft", c("Actual", "Predicted"), pch = 20, col = c("black", "tomato"),
       lty = "solid", bty = "n")
## RMSE
sqrt(mean((co2_test - co2_pred)^2))


### Johnson & Johnson - quarterly earnings per share
plot(JohnsonJohnson, type = "o", pch = 20, main = "J&J earnings")
JJ.HW <- HoltWinters(JohnsonJohnson, seas = "mult")
plot(JJ.HW)
