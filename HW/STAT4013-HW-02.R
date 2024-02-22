
# Sterling Hayden
# 09/22/23
# STAT4013


#################################################
#################################################
# Exercise 4.3

x=AirPassengers 
plot.ts(x, main="Varriance doesn't look indenpendent of time")
## We try to see if log will stabilize the variance.
plot.ts(log(x))
## We check square root transformation to see if it is better.
plot.ts(sqrt(x)) # not so good
plot.ts(x^(3/4))
# best transformation is log.
y=log(x)
### We check the ACF to see if differencing is needed
acf(y, lag=50, main="Needs differencing")
# 3 differencing operations
par(mfrow=c(3,1)) # 3x1 plot for the bellow
y.regdiff=diff(y, lag=1, diff=1); acf(y.regdiff, lag=50)
y.seasdiff=diff(y, lag=12, diff=1); acf(y.seasdiff, lag=50)
y.seas.reg=diff(y.regdiff, lag=12, diff=1); acf(y.seas.reg, lag=50)
# It appears that seasonal difference of the regular difference is the best.


#################################################
#################################################
# Question 4.10

# i)
# read in data as a ts
towels = ts(read.table('towels.txt'))
par(mfrow=c(3,1)) # 3x1 plot for the bellow
# plot unaltered data
plot(towels,main ="raw towels data",type ="l")
# seasonal differencing
towels.seasdiff=diff(towels, lag=4, diff=1);
plot(towels.seasdiff, main="time plot seasonal differencing")
acf(towels.seasdiff, lag=50, main="acf seasonal differencing")


# ii)
# regular differencing
par(mfrow=c(2,1))
towels.regdiff=diff(towels, lag=1, diff=1)
plot(towels.regdiff, main="time plot regular differencing")
acf(towels.regdiff, lag=50, main="acf regular differencing")


# iii) 
# seasonal differencing of the regular differencing
par(mfrow=c(2,1))
towels.seas.reg=diff(towels.regdiff, lag=4, diff=1)
plot(towels.seas.reg, main="time plot seasonal differencing of the regular differencing")
acf(towels.seas.reg, lag=50, main="acf seasonal differencing of the regular differencing")


# answer)
par(mfrow=c(3,1))
acf(towels.seasdiff, lag=50, main="acf seasonal differencing")
acf(towels.regdiff, lag=50, main="acf regular differencing")
acf(towels.seas.reg, lag=50, main="acf seasonal differencing of the regular differencing")
# it seems to be that the differencing in part ii resulted in a more stationary term.

