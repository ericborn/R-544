### Part 1)
# Event A1 = probablity of selected tax supporter is a democrat
# Event A2 = probablity of selected tax supporter is a republican
# Event A3 = probablity of selected tax supporter is an independent

# Create variables to store party affiliation and tax support percents
demo <- 0.40
repub <- 0.50
inde <- 0.10

dem.support <- 0.70
rep.support <- 0.40
ind.support <- 0.20

# Determine probability of selecting a supporter of the tax
tax <- 0.70*0.40 + 0.40*0.50 + 0.20*0.10
# = .28+.2+.02
# = 0.50

# Print probability of selecting a supporter of the tax
cat("The probability of selecting a tax supporter is", tax, "\n");

# probablity of selected tax supporter is a democrat
#P(A1|B) = P(B|A1)*P(A1)*P(A1)/P(B) = 0.70*0.40/0.5 = 0.56
dem.prob <- 0.70*0.40/0.5

# probablity of selected tax supporter is a republican
#P(A2|B) = P(B|A2)*P(A2)*P(A2)/P(B) = 0.40*0.50/0.5 = 0.4
rep.prob <- 0.40*0.50/0.5

# probablity of selected tax supporter is independent
#P(A3|B) = P(B|A3)*P(A3)*P(A3)/P(B) = 0.20*0.10/0.5 = 0.04
ind.prob <- 0.20*0.10/0.5

### i)
# print probablity of selected tax supporter being a democrat
cat("The probablity of a selected tax supporter being a democrat is", dem.prob, "\n");

### ii)
# print probablity of selected tax supporter being a republican
cat("The probablity of a selected tax supporter being a republican is", rep.prob, "\n");

### iii)
# print probablity of selected tax supporter being independent
cat("The probablity of a selected tax supporter being independent is", ind.prob, "\n");

### Verified using the bayes function provided
bayes <- function (prior, likelihood) {
  numerators <- prior * likelihood
  return (numerators / sum(numerators)) 
}

# Provide percents per party
prior <- c(0.40, 0.50, 0.10)

# Provide percents who support the tax within each party
likelihood <- c(0.70, 0.40, 0.20)

# Print probabilities, democrat, republican then independent
bayes(prior, likelihood)

### Part 2)
# a)
# Import and load probabilitiy packge
if (!is.element("prob", installed.packages()[,"Package"]))
  install.packages("prob", repos="http://cran.us.r-project.org", dependencies = TRUE)

library(prob)

# Initialize rolldie with 2 die and the probs column included
S <- rolldie(2, makespace = TRUE)

# Add random variable to the probability space that indicates the absolute value of the difference between X1 and X2
S <- addrv(S, U = abs(X1 - X2))

# b)
# Probability the two rolls differ by exactly 2
Prob(S, U == 2)

# Probability the two rolls differ by at most 2
Prob(S, U <= 2)

# Probability the two rolls differ by at least 3
Prob(S, U > 2)

# c)
# uses marginal to determine the marginal distribution of the difference between two rolls
marginal(S, vars = "U")

# d)
# Create user defined function called check. Check uses data[1] and data[2], X1 and X2, and adds them together. 
# Then it uses modulo 2 to determine if the number is equal to 0. If it is, than the number is even and a TRUE 
# is returned. If not a FALSE is returned.
check <- function(data) { 
  res <- (data[1] + data[2]) %% 2
  if(res == 0) {
    return(TRUE)  } else {
    return(FALSE)
  }
}


# Adds the random variable called v using the user defined function just created
S <- addrv(S, FUN = check, name = "V")

# Outputs the probability space with the new true/false indicator
S

# probability the sum of the two rolls is even
Prob(S, V == "TRUE")

# Marginal distribution for the even/odd check
marginal(S, vars = "V")

# 3
# Creates the evensum function that takes an input as data, iterates through it using a for loop
# Checks if the current value in the vector is evenly divisible by 2. If it is, it adds it to the variable num.
# Once the loop is finished it return the variable num
evensum <- function(data) {
  num <- 0
  for (i in data) {
    if(i %% 2 == 0) num = num + i
  }
  return(num)
}

evensum(c(15, 20, 25, 30, 35))
evensum(1:10)
evensum(seq(1,15, by = 2))

###
# Second part
###

# creates a function called reduceFunction which performs a modulo check on the variable number. 
# If number is evenly divisble by 2, it adds it to the variable result then returns result.
reduceFunction <- function(result, number) {
  if (number %% 2 == 0) {
    return(number + result)
  }
  return(result)
}

# Using the reduceFunction and the builtin Reduce we create another function called evensum2.
# evensum2 takes a single input called data. This input is fed into the Reduce function which uses
# Reduce function as its function, checking if the number is even, and applys it to the input data.
# recursively adding the values together that are returned from reduceFunction. A 0 is included as
# the third parameter to indicate to Reduce what number to start adding from.
evensum2 <- function(data) {
  Reduce(reduceFunction, data, 0)
}

evensum2(c(15, 20, 25, 30, 35))
evensum2(1:10)
evensum2(seq(1,15, by = 2))
# 4
# a)
# Import csv into dataframe
dow <- read.csv('http://kalathur.com/dow.csv', stringsAsFactors = FALSE)

# Calculates difference in values between rows, stores in a new column named DIFFS
dow$DIFFS <- c(0, diff(dow$VALUE))

# b)
# Counts the total number of days where the dow closed higher than the previous day
# 30
nrow(subset(dow, subset = DIFFS > 0))

# Counts the total number of days where the dow closed lower than the previous day
# 21
nrow(subset(dow, subset = DIFFS < 0))

# c)
# Display only days where the dow made a gain of at least 400 points
subset(dow, subset = DIFFS >= 400)

# d)
# Add Y/N in the streak column to indicate if the dow gained 100 or not
for (row in 1:nrow(dow)) {
  if(dow[row, 'DIFFS'] >= 100){
      dow[row, 'STREAK'] <- "Y"
  }
  else {
    #print("N")
    #dow[row, 'STREAK'] <- "N"
  }
}

# Create run length encoding based upon the streak column
dow.rle <- rle(dow$STREAK)

# Store the max run length, which is 3 into the max_run_length variable
max_run_length <- max(dow.rle$lengths)

# index of max run in rle output
max_index <- which.max(dow.rle$lengths == max(dow.rle$lengths))

# Number of values before the max
prev_values_length <- ifelse(max_index == 1, 0, sum(dow.rle$lengths[1:(max_index-1)]))

# index into dow to get the max run
dow[(prev_values_length + 1) : (prev_values_length + max_run_length),]