install.packages("CARBayes")
install.packages("inlabru")

library(CARBayes)
library(inlabru)

n <- 100
rho <- 0.8
prec <- 10
## note that the marginal precision would be
marg.prec <- prec * (1-rho^2)

E <- sample(c(5,4,10,12),size=n,replace=T)
eta <- as.vector(arima.sim(list(ma = 1, ar = rho), n = n,sd=sqrt(1/prec)))



y=rpois(n,E*exp(eta))
data = list(y=y, z=1:n, E=E)

arima.sim(list(order = c(1,0,0), ar = rho), n = n)


arima.sim(list(order = c(0,1,0)),n=n)   ### Random walk
RW_drift <- arima.sim(list(order = c(0,1,0)),n=n, mean = 10, sd = 1) ### Random walk with drift (mean is also the C or intercept here)

plot.ts(eta)
RW_diff <- diff(eta)

plot.ts(RW_diff)
plot.ts(RW_drift)

RW_drift_diff <- diff(RW_drift)
plot.ts(RW_drift_diff, col = 4, main = "First Order Difference")

arima(RW_drift_diff, order = c(0,0,0))


WN <- arima.sim(list(order = c(0,0,0)), n=n)

plot.ts(WN)
WN_diff <- diff(WN)
plot.ts(WN_diff)

