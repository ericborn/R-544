# Return true if odd indexes are twice their succeeding value
# true scenario
a <- c(6, 3, 10, 5, 22, 11, 26, 13)

# false scenario
b <- c(7, 3, 6, 5, 22, 11, 26, 13)

# returns true
all(a[c(T, F)] == 2 * a[c(F, T)])

# Returns FALSE
all(b[c(T, F)] == 2 * b[c(F, T)])

# Sum of all even values
data <- c(0,2,3,4,7,8,6,9)

evensum1 <- function(data) {
  out <- 0
  for(i in data){
    if(i %% 2 == 0){
    out <- out + i
    }
  }
  return(out)
}

evensum1(data)

evensum2 <- function(data) {
  sum(data[data %% 2 == 0])
}

evensum2(data)

# find the longest run
long <- c(10,20,20,40,40,40,40,40,50,50,10,40)
run <- rle(long)
max_index <- which.max(run$lengths == max(run$lengths))
run$values[max_index]

# select 5 students from a group
library(prob)

students <- c("boy","boy","boy","boy","boy","boy","girl","girl","girl","girl")

# urnsamples may need replacement to be TRUE (allows items to reside in multiple groups)
# Or ordering to TRUE if the order in which the items are added to a group matters
selection <- urnsamples(students, 5)

# create probability space
prob.space <- probspace(selection)

head(prob.space)

# Create function to count boys
countBoys <- function (x){
  return(sum(x == "boy"))
}

# Create function to count girls
countGirls <- function (x){
  return(sum(x == "girl"))
}

# Store all random variables based upon B column
S1 <- addrv(prob.space, FUN = countBoys, name = "B")
head(S1)

marginal(S1, vars = "B")

S2 <- addrv(prob.space, FUN = countGirls, name = "G")
head(S2)
marginal(S2, vars = "G")

# output marginal S1 or S2 and list out some probabilities
# 2 girls 0.476
# 5 boys 0.024


#various counts from Titantic datasets
# dataset to use
Titanic

#structure of table
str(Titanic)

# a)
# Count by gender
margin.table(Titanic, c(2))

# b)
# provides counts on sex and survived which are elements 2 and 4
margin.table(Titanic, c(2, 4))

# c)
# By traveling class how many survived
margin.table(Titanic, c(1, 4))

# d)
# By traveling class and age, how many survived and did not survive?
margin.table(Titanic, c(1, 3, 4))

# Distributions
# Normal distribution
# pnorm - cumulative probability
# qnorm - quantile function

set.seed(4567)

# a)
# Quartiles 25% of population below this, 50% below this, 75% below this
qnorm(c(0.25, 0.5, 0.75), mean = 100, sd = 5)

# b)
# if question asks for value that all variables exceed, use inverse of that value from 100
# ie. exceed 65%, you would use 35%
qnorm(0.35, mean = 100, sd = 5)

# Or use the actual value with lower.tail = FALSE
qnorm(0.65, mean = 100, sd = 5, lower.tail = FALSE)

# c)
qnorm(0.025, 100, 5)
qnorm(0.025, 100, 5, lower.tail = FALSE)


# stratified sampling

sort(table(beer$beer_abv), decreasing = TRUE)[1:10]

library(cluster)
library(sampling)
data(votes.repub)
data.voting <- votes.repub

# a)
# Uses substring to grab only the first letter from each rowname and create a new column called Letter
data.voting$Letter <- substring(rownames(data), 0 ,1)

# b)
# contingency table
table(data.voting$Letter)

# c)
# write down all sampling techniques
# count how many groups you need to sample from 
# use rep(x, y) to setup groups x = number from the group, y = number of groups
# srswor sample without replacement = each item has 1 chance to be selected
# srswr sample with replacement = each item returns to the pool to be selected from again
st <- strata(data.voting, stratanames = c("Letter"), size = rep(1, 19), method = "srswor")

# Returns full data with probabilties for each item to be selected
sample1 <- getdata(data.voting, st)

# mean for sample 1976
mean(sample1$X1976)

# mean for entire dataset 1976
mean(data.voting$X1976)

# d)

# utilizing Stringer functions
library(stringr)
x <- "abc"
# a)
strata(data.voting, stratanames = c("Letter"), size = rep(1, 19), method = "srswor")

# b)
str_sub(x, 1:(str_length(x) + 1), -1)

# c)
# select all the first letters from the fruit set
first <- str_sub(fruit, 1, 1)

# find highest occurence
max(table(first))

# find highest occurence location and letter
which.max(table(first))

# select all the last letters from the fruit set
last <- str_sub(fruit, -1, -1)

# Find highest occurence location and letter
letter <- which.max(table(last))


# d)
# find all start with p and ending with e, any letters in between
fruit[str_detect(fruit, '^p[a-z]+e$')]
