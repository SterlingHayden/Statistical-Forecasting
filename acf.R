######
###### Autocorrelation
######

plot(nhtemp, type = "o", pch = 20, main = "Average temperature in Nottingham")
acf(nhtemp)
Box.test(nhtemp, lag = 10)
Box.test(nhtemp, lag = 10, type = "Ljung")

## rooms <- scan("D:/STAT 4013/rooms.txt")
rooms <- scan("R_code_book/CH2-CODE-DATA-BASER/rooms.txt")
rooms <- ts(rooms, start = c(1977,1), freq = 12)
plot(rooms, type = "o", pch = 20)

## Multiplicative decomposition
rooms.md <- decompose(rooms, type = "mult")
plot(rooms.md)

x <- rooms.md$random
plot(x, type = "o", pch = 20, main = "Random component of the 'Rooms' series")
acf(x)
head(x, 10)
tail(x, 10)
x <- window(x, start = c(1977, 7), end = c(1990, 6))
acf(x)
Box.test(x, lag = 12, type = "Ljung")

## White noise
y <- rnorm(140, sd = 2)
plot(y, type = "o", pch = 20, main = "This *is* white noise!")
acf(y, lag = 30)
Box.test(y, lag = 15, type = "Ljung")

### Partial Autocorrelations
pacf(x)

### ACF of nonstationary time series
AP <- log(AirPassengers)
plot(AP, type = "o", pch = 20, main = "Air Passengers (log)")
acf(AP, lag = 48)

length(BJsales)
tsp(BJsales)
plot.ts(BJsales)
acf(BJsales, lag = 50)

length(EuStockMarkets[,1])
tsp(EuStockMarkets[,1])
plot(EuStockMarkets[,1])
acf(EuStockMarkets[,1], lag = 100)

length(lynx)
tsp(lynx)
plot.ts(lynx)
acf(lynx, lag = 50)

length(deaths)
tsp(deaths)
plot.ts(deaths)
acf(deaths, lag = 48)

length(nhtemp)
tsp(nhtemp)
plot.ts(nhtemp)
acf(nhtemp, lag = 30)

length(Nile)
tsp(Nile)
plot.ts(Nile)
acf(Nile, lag = 30)

### Differencing
x <- diff(EuStockMarkets[,1])
plot(x, man = "EU Stock Market - first differences")
acf(x, lag = 100)

x <- diff(deaths, lag = 12)
plot(x, main = "Deaths - lag 12 differences")
acf(x, lag = 24)
