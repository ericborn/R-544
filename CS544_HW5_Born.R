# CS544 Week 5 homework by Eric Born

##########
# Part 1 #
##########

# Load probability library
library(prob)

# Setup variables
# Create sequence from 1 to 20
seq <- 1:20

# Create samples 2 and 5
samples.two <- combn(seq, 2)
samples.five <- combn(seq, 5)

# Apply over array 2 and 5
xbar.two <- apply(samples.two, 2, FUN = mean)
xbar.five <- apply(samples.five, 2, FUN = mean)

# a b and c)
# Set par to 1 row, 3 columns
par(mfrow=c(1,3))

# Create the histograms for the base sequence, 2 and 5 samples
hist(seq, prob = TRUE, main = "Sequence")
hist(xbar.two, prob = TRUE, main = "Two Samples")
hist(xbar.five, prob = TRUE, main = "Five Samples")

# Set par back to 1 row 1 column
par(mfrow=c(1,1))

# d)
# Mean for all three is 10.5
mean(seq)
mean(xbar.two)
mean(xbar.five)

# Standard deviation is 5.916, 3.979 and 2.291
stdev(seq)
stdev(xbar.two)
stdev(xbar.five)

##########
# Part 2 #
##########
# a)
# Import data with read.csv
queries <- read.csv("http://kalathur.com/cs544/data/queries.csv")

# Create new column of original query counts divided by 1m for easier viewing on histogram
queries$divided <- queries$queries / 1000000

# histogram of the query data on the queries column
hist(queries$divided, prob = TRUE, xlab = "Total Queries in Millions", 
     main = "Query data")

# mean is 248514980 standard dev is 29202674
mean(queries$queries)
stdev(queries$queries)

# b)
# Set seed for repeatability
set.seed(3425)

# set samples to 1000 and sample size to 5
samples <- 1000
sample.size <- 5

# Create place holder of 1000 0 values in xbar.1kfive
xbar.1kfive <- numeric(samples)

# Replace each 0 value with a random value from the queries data set using sample
for (i in 1: samples) {
  xbar.1kfive[i] <- mean(sample(queries$divided, size = sample.size, replace = TRUE))
}

# Display a histogram for sample size 5
hist(xbar.1kfive, prob = TRUE, xlab = "Total Queries in Millions",
       main = "Query Data, 1000 Samples, Size 5", ylim = c(0, 0.030))

# Mean for 1k samples, size 5 is 248570996
mean(xbar.1kfive)
# Standard deviation is 12608487
stdev(xbar.1kfive)

# c)
# set samples to 1000 and sample size to 5
samples <- 1000
sample.size <- 20

# Create place holder of 1000 0 values in xbar.1k20
xbar.1k20 <- numeric(samples)

# Replace each 0 value with a random value from the queries data set using sample
for (i in 1: samples) {
  xbar.1k20[i] <- mean(sample(queries$divided, size = sample.size, replace = TRUE))
}

# Display a histogram for sample size 20
hist(xbar.1k20, prob = TRUE, xlab = "Total Queries in Millions"
     , main = "Query Data, 1000 Samples, Size 20", ylim = c(0, 0.06))

# Mean for 1k samples, size 5 is 248342376
mean(xbar.1k20)
# Standard deviation is 6322097
stdev(xbar.1k20)

# d)
# Means for each are below:
# a) 248514980 #56016 lower than b, 172604 higher than c
# b) 248570996 #56016 higher than a, 228620 higher than c
# c) 248342376 #172604 lower than a, 172604 lower than b

# Standard devations are below:
# a) 29202674 #16594187 higher than b, 22880577 higher than c
# b) 12608487 #16594187 lower than a, 6286390 higher than c
# c) 6322097 #22880577 lower than a, 6286390 lower than b

##########
# Part 3 #
##########
# a)
# Set seed for repeatability
set.seed(6528)

# Generate 1000 random numbers from a negative binomial distribution
nbd <- rnbinom(1000, size = 5, prob = 0.5)

# View distinct values and their counts from nbd variable using table
table(nbd)

# Store the proportions of each number in nbd.prop
nbd.prop <- prop.table(table(nbd))

# Display barplot of nbd data
barplot(nbd.prop,
        xlab = "x", ylim = c(0, 0.16), ylab = "Proportion")

# b)
# Mean is 5, SD is 3.1
mean(nbd)
sd(nbd)

# set samples to 5000
samples <- 5000

# set par to 2x2 to display 4 histograms on the same plot
par(mfrow = c(2,2))

# Input nbd, mean and sd into for loop which generates data for sample sizes 10, 20, 30 and 40
for (size in c(10, 20, 30, 40)) {
  for (i in 1:samples) {
    nbd[i] <- mean(rnorm(size, 
                          mean = 5, sd = 3.1))
  }

  # Create hisograms from each histogram
  hist(nbd, prob = TRUE, 
       breaks = 15, main = paste("Sample Size =", size))
  
  # Print out sample size, mean and standard deviation
  cat("Sample Size = ", size, " Mean = ", mean(nbd),
      " SD = ", sd(nbd), "\n")
}

# Set par back to 1x1
par(mfrow = c(1,1))

# c)
# The mean for a) and all of b) was 5
# The standard deviation for a) was 3.1, for b) it was 0.98, 0.68, 0.58 and 0.5

##########
# Part 4 #
##########
# a)
# Install and load sampling package
install.packages("sampling")
library("sampling")

# load dataset MU284
data(MU284)

# Set seed for repeatability
set.seed(6546)

# Random sample without replace stored in variable s1
s1 <- srswor(20, nrow(MU284))

# Using variable s1 as the row index to map the results which are not equal to 0 
# from MU284 into the sample.1 variable
sample.1 <- MU284[s1 != 0, ]

# Output the frequencies for each region
table(sample.1$REG)

# Question was to show the percentages of these with respect to the entire dataset
# Unsure if this was what the question was asking for but I displayed the 
# percentages each region makes up for the original and sampled dataset
round(table(MU284$REG) / length(MU284$REG), 2)
round(table(sample.1$REG) / length(sample.1$REG), 3)

# Original
#   1    2    3    4    5    6    7    8 
#0.09 0.17 0.11 0.13 0.20 0.14 0.05 0.10
# Sampled
#   1    2    3    5    7    8 
#0.20 0.25 0.15 0.20 0.05 0.15 

#Sampled set does not have any selections from region 4 or 6, which are both larger than 

# b)
# Set seed for repeatability
set.seed(1137)

# Capture the number of rows in thje MU284 dataset
N <- nrow(MU284)

# set sample size to 20
n <- 20

# items in each group
k <- floor(N / n)
k

# random item from first group
r <- sample(k, 1)
r

# select every kth item
s <- seq(r, by = k, length = n)

# Store the rows using s as the index for the MU dataset
sample.2 <- MU284[s, ]

# Output reg for the selected rows
table(sample.2$REG)

# percentages of original dataset compared to sampling
# Sampled dataset only adds to 95% because the last row does not contain data due to the last increment
# being higher than the last row in the dataset
round(table(MU284$REG) / length(MU284$REG), 2)
round(table(sample.2$REG) / length(sample.2$REG), 3)

# Original
#   1    2    3    4    5    6    7    8 
#0.09 0.17 0.11 0.13 0.20 0.14 0.05 0.10

# Sampled
#   1    2    3    4    5    6    7    8 
#0.10 0.10 0.15 0.10 0.20 0.15 0.05 0.10 

# c)
# Set seed for repeatability
set.seed(564)

# Store the inclusion probabilities from MU284 dataset column S82 with a sample size of 20
pik <-inclusionprobabilities(MU284$S82, 20)

# Perform systematic sampling based on the inclusion probabilities captured from the dataset
s <- UPsystematic(pik)

# Store the rows using s as the index for the MU dataset
sample.3 <- MU284[s != 0, ]

# Sample from the dataset
sample.3

# Reg from the dataset
table(sample.3$REG)

# percentages of original dataset compared to sampling
# This seems to be the best sampling method so far as all regions are included
round(table(MU284$REG) / length(MU284$REG), 2)
round(table(sample.3$REG) / length(sample.3$REG), 3)

# Original
#   1    2    3    4    5    6    7    8 
#0.09 0.17 0.11 0.13 0.20 0.14 0.05 0.10

# Sampled
#   1    2    3    4    5    6    7    8 
#0.10 0.15 0.10 0.15 0.20 0.15 0.05 0.10

# d)
# Set seed for repeatability
set.seed(961)

# Order the MU284 dataset by the REG column
order.index <- order(MU284$REG)

# Store ordered data
data <- MU284[order.index, ]

# Store dataset frequency
freq <- table(MU284$REG)

# create group sizes for each region
sizes <- round(20 * freq / sum(freq))

# Create a stratified sample using the group sizes
st <- strata(data, stratanames = c("REG"),
             size = sizes, method = "srswor")

# Draws the sample from the dataset
sample.4 <- getdata(data, st)

# frequencies for sample
table(sample.4$REG)

# percentages of original dataset compared to sampling
# Same distribution as part c
round(table(MU284$REG) / length(MU284$REG), 2)
round(table(sample.4$REG) / length(sample.4$REG), 3)

# Original
#   1    2    3    4    5    6    7    8 
#0.09 0.17 0.11 0.13 0.20 0.14 0.05 0.10

# Sampled
#   1    2    3    4    5    6    7    8 
#0.10 0.15 0.10 0.15 0.20 0.15 0.05 0.10

# e)
mean(sample.1$RMT85) #138
mean(sample.2$RMT85) #291.1
mean(sample.3$RMT85) #236.6
mean(sample.4$RMT85) #171.7
mean(MU284$RMT85)    #245.088
