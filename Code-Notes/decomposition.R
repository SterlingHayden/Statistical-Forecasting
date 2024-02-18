######
###### Time series decomposition
######

rooms <- scan("D:/STAT 4013/rooms.txt")
rooms <- ts(rooms, start = c(1977,1), freq = 12)
plot(rooms, type = "o", pch = 20)

### Multiplicative decomposition
rooms.md <- decompose(rooms, type = "mult")
plot(rooms.md)

### Seasonally adjusted series
rooms.seas.adjusted <- rooms / rooms.md$seasonal
 
plot(rooms.seas.adjusted, main = "Seasonally adjusted rooms",
     ylab = "Rooms with seasonal removed", type = "o", pch = 20, cex = 0.75)

### Trend and seasonal superimposed on data 
plot(rooms, main = "Rooms with trend\nand seasonal effect superimposed", 
     ylab = "Rooms")
lines(rooms.md$trend, lwd = 2, col = "blue")
points(rooms.md$seasonal * rooms.md$trend,
       col = "red", pch = 18)
legend("topleft", c("seasonal and trend", "trend"), col=c("red", "blue"),
       pch = c(18, NA), lty = c(NA, 1), bty = "n")

### Random component
plot(rooms.md$random, type = "o", pch = 20)

### Moving averages
y <- c(3.3602,-3.1769,0.3484,7.469,4.4963,-0.4621,
       0.7218,6.9484,5.2374,2.9242,4.7006,11.2793,5.1637,
       1.5441,12.121,9.6588,8.0922,3.9653,11.4177,13.2088)
y <- ts(y, start = 1960, freq = 4)
filter(y, filter = c(1, 1, 1, 1)/4)
filter(filter(y, filter = c(1, 1, 1, 1)/4), c(1, 1)/2)
filter(y, filter = c(1, 2, 2, 2, 1)/8)

y_trend <- filter(y, filter = c(1, 2, 2, 2, 1)/8)
y_detrend <- y - y_trend
seas <- colMeans(matrix(y_detrend, ncol = 4, byrow = TRUE), na.rm = TRUE)

### LOWESS
plot(stl(rooms, "per"))

### Regression smoothers
start(AirPassengers)
frequency(AirPassengers)
plot(AirPassengers)
lAirPass <- log(AirPassengers)
plot(lAirPass, type = "o", pch = 20)
Time <- time(lAirPass)
Seas <- rep(month.abb, length = length(lAirPass))
Seas <- factor(Seas, levels = month.abb)
y.lm <- lm(lAirPass ~ Time + Seas)
summary(y.lm)
plot(residuals(y.lm), type = 'h'); abline(h=0)
plot(head(residuals(y.lm), -1), tail(residuals(y.lm), -1))

### Using regression for forecasting
future <- ts(0, start = c(1961, 1), end = c(1962, 12), freq = 12)
future[] <- predict(y.lm, data.frame(Time = time(future), Seas = rep(month.abb, 2)))
plot(ts.union(lAirPass, future), plot.type = "s", col = c("black", "orange"),
     type = "o", pch = 20, ylab = "log Air Passengers", main = "Data and predictions")
abline(v = 0.5*(tail(time(lAirPass),1) + head(time(future),1)), 
       lty = "longdash", lwd = 2)

