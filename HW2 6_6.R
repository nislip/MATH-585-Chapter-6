# Data for treatments 2, and 3
pop1 <- matrix(c(3,1,2,3,6,3), ncol=2) # treatment 1
pop2 <- matrix(c(2, 5, 3, 2, 3, 1, 1, 3), ncol = 2) # treatment 2

# Part (A) 
# Separate Covariance matrices 
s1 <- cov(pop1)
s2 <- cov(pop2)
# total OBS for Pop1,2
n1 <- nrow(pop1)
n2 <- nrow(pop2)
p <- 2

# Calculate Pooled Sample Co-variances 
Spooled <- (((n1 - 1)/(n1 + n2 - 2)) * s1) + ((n2 - 1)/(n1 + n2 - 2)) * s2 

# Part (B) 
# calculate c^2
c2 <- ((n1 + n2 - 2)*p/(n1 + n2 - p - 1)) * qf(0.01, df1 = p, 
                                               df2 = n1 + n2 - p - 1,
                                               lower.tail = FALSE)
# Vector means for treatments 1 and 2
X1 <- matrix(c(2,4), ncol = 1)
X2 <- matrix(c(3,2), ncol = 1)

# test Hypothesis
T2 <- t(X1 - X2) %*% inv(  ( (1/n1) + (1/n2) )* Spooled ) %*% (X1 - X2)
T2 > c2 # FALSE, do not reject null hypothesis 

# Part (C) 
(X1[1,1] - X2[1,1]) + sqrt(c2) * sqrt( ((1/n1) + (1/n2)) * Spooled[1,1]) # First
(X1[1,1] - X2[1,1]) - sqrt(c2) * sqrt( ((1/n1) + (1/n2)) * Spooled[1,1])
(X1[2,1] - X2[2,1]) - sqrt(c2) * sqrt( ((1/n1) + (1/n2)) * Spooled[2,2]) # Second
(X1[2,1] - X2[2,1]) + sqrt(c2) * sqrt( ((1/n1) + (1/n2)) * Spooled[2,2])

