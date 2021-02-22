install.packages("matlib")
library(matlib)
# contrast matrix C
C <- matrix(c(1, 1, -1, 0, 0 ,-1), ncol = 3, nrow = 2)
c1 <- c(1, -1, 0)
c2 <- c(1, 0, -1)
S <- matrix(c(101.3, 63.0, 71.0, 
              63.0, 80.2, 55.6, 
              71.0, 55.6, 97.4 ), nrow=3, ncol=3) # Sample Cov matrix
xbar <- matrix(c(46.1, 57.3, 50.4), ncol=1, nrow = 3) #col Means
n <- 40 
q <- 3

# test Equality at alpha = 0.05

Tsqr <- 40 * t(C %*% xbar) %*% inv((C %*% S %*% t(C))) %*% (C %*% xbar)
# RHS of inequality w/ F-Stats
E <- (((n-1)*(q-1))/(n-q+1)) * qf(0.05, df1 = 2, df2 = 38 , lower.tail = FALSE) 
# Construct Confidence intervals

c1mu11 <- ((xbar[1,1] - xbar[2,1]) + sqrt(E) * sqrt(t(c1) %*% S %*% c1/n))
c1mu12 <- ((xbar[1,1] - xbar[2,1]) - sqrt(E) * sqrt(t(c1) %*% S %*% c1/n))
c2mu21 <- ((xbar[1,1] - xbar[3,1]) + sqrt(E) * sqrt(t(c2) %*% S %*% c2/n))
c2mu22 <- ((xbar[1,1] - xbar[3,1]) - sqrt(E) * sqrt((t(c2) %*% S %*% c2)/n))
  
# Note: using Lower.tail = FALSE to calculate test value significance, NOT
# finding the probability of confidence interval. 