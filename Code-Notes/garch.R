######
###### GARCH models for conditionally heteroschedastic time series
######

library(MASS)
sp <- ts(SP500)
plot(sp, main = "Daily returns on the S&P 500 index in the 1990s" )

mean(sp)
acf(sp)

acf(sp^2)
pacf(sp^2)

### Fitting GARCH models
library(tseries)
mod1 <- garch(sp, trace = FALSE, grad = "numerical")
mod1
res <- residuals(mod1)
plot(res, main = "Residuals from GARCH(1,1)")
acf(res[-1])
pacf(res[-1])

####
#### Amsterdam stock data
####

x <- read.csv("stockmarket.csv", header = T)
head(x)
Amsterdam.raw <- ts(x[,1])
length(Amsterdam.raw)
acf(Amsterdam.raw)
ret <- 100 * diff(log(Amsterdam.raw))

### Explore several GARCH(p,q) models and select the 'best'
P <- 2; Q <- 2
aicS <- matrix(Inf, P+1, Q+1)
dimnames(aicS) <- list(p = 0:P, q = 0:Q)
for (p in 0:P) 
  for (q in 0:Q) 
    if (p +  q > 0) {
      model <- garch(ret, order = c(p, q), trace = FALSE, grad = "numerical")
      aicS[p+1, q+1] <- AIC(model)
    }

aicS
## locate the model having minimum AIC - GARCH(2, 1)
(cbind(as.vector(row(aicS)), as.vector(col(aicS))) - 1)[which.min(aicS), ]

   
