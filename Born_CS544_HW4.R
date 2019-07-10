#dunif D for probability mass function (PMF) for continuous data

#punif for cumulative distribution function (CDF) for discrete uniform

#qunif quantile function (inverse of punif) returns values based upon CDF input

#dbinom for binomial distribution
# n = number of trials
# p = the probability of success
# x = how many times you want it to be successful

# 5 trails, 0.5 for success, probabilitiy it succeeds 3 times
# n <- 5; p <- 1/2
# dbinom(3, size = n, prob = p)

#rbinom is random numbers according to the binomial distribution

# Part 1)
# a)
# number of batters is 6, chance to strike-out is 1/2
n <- 6; p <- 1/2

# Probability distribution and plot
heights <- dbinom(0:n, size = n, prob = p)
heights

plot(0:n, heights, type = "h",
     main = "Batters struck out", xlab = "Number of batters",
     ylab = "PMF")
points(0:n, heights, pch = 16)

# b)
# Plotting the CDF
cdf <- c(0, cumsum(heights))

cdfplot <- stepfun(0:n, cdf)
plot(cdfplot, verticals = FALSE, pch = 16,
     main = "Batters struck out", xlab = "Number of batters", ylab = "CDF")

# c)
# Setup variables
n <- 6; p <- 0.7

# Probability distribution and plot
heights <- dbinom(0:n, size = n, prob = p)
heights

plot(0:n, heights, type = "h",
     main = "Batters struck out", xlab = "Number of batters",
     ylab = "PMF")
points(0:n, heights, pch = 16)

# CDF plot
cdf <- c(0, cumsum(heights))

cdfplot <- stepfun(0:n, cdf)
plot(cdfplot, verticals = FALSE, pch = 16,
     main = "Batters struck out", xlab = "Number of batters", ylab = "CDF")

# d)
# Setup variables
n <- 6; p <- 0.3

# Probability distribution and plot
heights <- dbinom(0:n, size = n, prob = p)
heights

plot(0:n, heights, type = "h",
     main = "Batters struck out", xlab = "Number of batters",
     ylab = "PMF")
points(0:n, heights, pch = 16)

# CDF plot
cdf <- c(0, cumsum(heights))

cdfplot <- stepfun(0:n, cdf)
plot(cdfplot, verticals = FALSE, pch = 16,
     main = "Batters struck out", xlab = "Number of batters", ylab = "CDF")

# e)
# The first distribution, 50%, is evenly distributed, highest between 2-4 which is the center of the chart. 
# The second, 70%, is left skewed meaning the lowest probability is between 0-2 and highest at 3-5.
# The last distribution, 30%, is right skewed. The highest is 1-3 and lowest is 4-6.

# Part 2)
# a)
# 10 flights, 80% arrive on time
n <- 10; p <- 0.8

# probability 4 flights arrive on time (0.005505024)
dbinom(4, size = n, prob = p)

# b) probability 4 or fewer arive on time (0.006369382)
pbinom(4, size = n, prob = p)

# c)
# Porbability distribution from 0 to 10 times
dbinom(0:n, size = n, prob = p)

# d)
# Plot the PMF
heights <- dbinom(0:n, size = n, prob = p)
plot(0:n, heights, type = "h",
     main = "Flights on time", xlab = "Number of flights",
     ylab = "PMF")
points(0:n, heights, pch = 16)

# Plot the CDF
cdf <- c(0, cumsum(heights))
cdfplot <- stepfun(0:n, cdf)
plot(cdfplot, verticals = FALSE, pch = 16,
     main = "Flights on time", xlab = "Number of flights", ylab = "CDF")

# Part 3)
# a)
# Probability of exactly 3 cars (0.00756655)
dpois(3, lambda = 10)

# b)
# Probability of at least 3 cars (0.9972306)
ppois(2, lambda = 10, lower.tail = FALSE)

# c)
# Probability of serving between 2 to 5 cars
ppois(5, lambda = 10) - ppois(2, lambda = 10)

# d)
# Store probability for the first 20 cars
pmf <- dpois(0:20, lambda = 10)
pmf

#Plot PMF
plot(0:20, pmf,type="h",
     xlab="first 20 cars", ylab="PMF", ylim = c(0, 0.15))
abline(h=0, col="red")

# Part 4)
# a) All answers interpreted as a continuous case so a min and max values are provided
score <- 60:100
prob <- rep(1/41, 41)
# i) 1/41
# ii) 1/41
# iii) 1/41

# b) Mean is 80, standard deviation is 11.97915
mean <- sum(score * prob)
stdDev <- sd(score)

# c) probability of getting a score between 60-70 is 0.2682927
sum(dunif(60:70, min = 60, max = 100))

# d) probability of getting a score greater than 80 is 0.5
punif(80, min = 60, max = 100, lower.tail = FALSE)

# e) probability of getting a score between 90-100 is 0.2682927
sum(dunif(90:100, min = 60, max = 100))

# part 5)
# a) Plot covering three standard deviations on either side of the mean
mu <- 100
sigma <- 10
x <- seq(65,135)
pdf <- dnorm(x, mean = mu, sd = sigma)

plot(x, pdf, type="l", col="red", 
     xlim=c(65,135), ylim=c(0,0.04),
     xaxt="n", yaxt="n",
     main="Dollars spent", xlab="Dollars", ylab="PDF")
axis(side = 1, at = c(70,80,90,100,110,120,130), 
     labels = TRUE) 
axis(side = 2, at = c(0,0.01,0.02,0.03,0.04,0.05), 
     labels = TRUE)

# b) Visitor spends more than $120: 0.02275013
pnorm(mu + 2*sigma, mean = mu, sd = sigma, lower.tail = FALSE)

# c) Visitor spends bnetween $80-$90: 0.1359051
pnorm(mu - sigma, mean = mu, sd = sigma) -
  pnorm(mu - 2*sigma, mean = mu, sd = sigma)

# d)
# Probability of spending within one standard deviation: 0.6826895
pnorm(mu + sigma, mean = mu, sd = sigma) -
  pnorm(mu - sigma, mean = mu, sd = sigma)

# Probability of spending within two standard deviations: 0.9544997
pnorm(mu + 2*sigma, mean = mu, sd = sigma) -
  pnorm(mu - 2*sigma, mean = mu, sd = sigma)

# Probability of spending within three standard deviations: 0.9973002
pnorm(mu + 3*sigma, mean = mu, sd = sigma) -
  pnorm(mu - 3*sigma, mean = mu, sd = sigma)

# e)
# The two values where 90% of the money spent will be about +/- 1.645 from the standard deviation $83.55 - $116.45
pnorm(mu + 1.645*sigma, mean = mu, sd = sigma) -
  pnorm(mu - 1.645*sigma, mean = mu, sd = sigma)

# f) Plot 10000 visitors using the above distribution
visitors <- rnorm(1000, mean = mu, sd = sigma)
visitors <- round(visitors)

plot(table(visitors), type = "h",
     main="Dollars spent", xlab="Dollars spent", ylab="Visitors")

# part 6)
# a) Next call within 2 minutes: 0.4511884
pexp(2/60, rate = 18)

# b) Next call within 5 minutes: 0.7768698
pexp(5/60, rate = 18)

# c) Next call between 2 to 5 minutes
pexp(5/60, rate=18)-
    pexp(2/60, rate=18)

# d) Plot the CDF
x <- seq(0,1, by=1/60)

cdf <- pexp(x, rate=18)
plot(x, cdf, type="l", col="red", 
     xlim=c(0,0.4))
