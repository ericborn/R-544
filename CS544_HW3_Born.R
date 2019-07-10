# Install and Import UsingR library for datasets and visualizations
if (!is.element("UsingR", installed.packages()[,"Package"]))
  install.packages("UsingR", dep = TRUE)

library(UsingR)

# Part 1)

# Output the difference between successive prime numbers.
diff(primes)

# Store those differences in table format which hows the number and the frequency throughout the dataset
tab.primes <- table(diff(primes))

# Output the table
tab.primes

# Display the table as a barplot
barplot(tab.primes)

# Part 2)
# Install and load the plyr library
install.packages('plyr')
library(plyr)

# Store the coins data into a dataframe using the count function from the plyr library
# Count takes the coins data and performs a count on the value column
df <- data.frame(count(coins, 'value'))

# a)
# Output the dataframe which contains the count of each coin by denomination
df

# b)
# Creates a new column, tValue, in the df dataframe which is the coins value multiplied by
# the total frequency.
df$tValue <- df$value * df$freq

# c)
# sum the tValue column to display the total value of all coins
sum(df$tValue)

# d)
# number of coins per year stored in cy using count on coins$year column
cy <- count(coins$year)

# rename column labels
names(cy) <- c("Year", "Frequency")

# Barplot created using column lables for the x and y axis and names.arg based on the year column from cy so the x
# axis has a reference for the year
barplot(cy$Frequency, xlab = "Year", ylab = "Frequency", names.arg = c(cy$Year))

# Part 3)
# a)
# Output using the stem plot.
# the largest portion of this data set falls between 10-14
stem(south)

# b)
# Output the five number summary
summary(south)

# Calculating lower upper and interquartile ranges
# using double brackets to only retrieve the value not the percent
lowerQ <- quantile(south)[[2]]
upperQ <- quantile(south)[[4]]
iqr <- IQR(south)

# Calculate bounds for mild outliers
mild.upper <- (iqr * 1.5) + upperQ
mild.lower <- lowerQ - (iqr * 1.5)

# Calculate bounds for extreme outliers
extreme.upper <- (iqr * 3) + upperQ
extreme.lower <- lowerQ - (iqr * 3)

# Compare mild and extreme uppers and lowers against south dataset to find the outliers
south.mild.upper <- south[south > mild.upper]
south.mild.lower <- south[south < mild.lower]
south.extreme.upper <- south[south > extreme.upper]
south.extreme.lower <- south[south < extreme.lower]

# Print out if there are any outliers
if (length(south.mild.lower) > 0){
  cat("The mild lowers are", south.mild.lower, "\n");
} else{
  print("There are no mild lowers")
}
if (length(south.mild.upper) > 0){
  cat("The mild uppers are", south.mild.upper, "\n");
} else{
  print("There are no mild uppers")
}
if (length(south.extreme.lower) > 0){
  cat("The extreme lowers are", south.extreme.lower, "\n");
} else{
  print("There are no extreme lowers")
}
if (length(south.extreme.upper) > 0){
  cat("The extreme uppers is", south.extreme.upper, "\n");
} else{
  print("There are no extreme uppers")
}

# c)
# Horizontal boxplot with labels displaying five number summary
boxplot(south, horizontal = TRUE, xaxt = "n")
axis(side = 1, at = fivenum(south), labels = TRUE)

# Part 4)
# a)
# Output the total number each digit 0 to 9 occurs
table(pi2000)

# b) Calculate the percent frequencies by dividing the table output by the total number of values in pi2000
table(pi2000) / sum(pi2000)

# c)
# Store previous calculation in bar
bar <- table(pi2000) / sum(pi2000) 

# Plot using bar with a y limit at 0.025 for better graph scale
barplot(bar,
        col = "blue", ylim=c(0,0.025),
        xlab = "Digit", ylab = "Frequency")

# d)
# The first bar encompasses both 0 and 1 so it seems twice as large as the other bars
hist(pi2000)

# Histogram that looks similar to previous barplot using breaks 0 to 9 with an increment of 0.9 so that
# 0 and 1 are not grouped together into the same bar. Y limit set from 0 to 250 and x set from 0 to 10
hist(pi2000, breaks=seq(0,9,0.9)
     ,ylim=c(0,250), xlim=c(0,10))

# Part 5)
# a) Create sport matrix with cbind
sport <- cbind(c(25,20), c(10,40), c(15,30))

# b)
# Set row and column names for matrix sport
rownames(sport) <- c("Men", "Women")

# c)
colnames(sport) <- c("NFL", "NBA", "NHL")

# Output sport matrix with column and row names
sport

# d)
# store genders and categories
gender <- c("Men", "Women")
category <- c("NFL", "NBA", "NHL")

# Add dimension variables to the sport table
dimnames(sport) <- list(Gender=gender, Sport=category)

# Output matrix with dimension variables
sport

# e)
# Marginal distributions
# Store sums for each column and row in a new table
sport.sum <- addmargins(sport)

# Marginal distributions by gender
# divide the sum of each row by the value in the last row and column of the sport.sum table
apply(sport, 1, sum) / sport.sum[length(sport.sum)]

# Marginal distributions by sport
# divide the sum of each column by the value in the last row and column of the sport.sum table
apply(sport, 2, sum) / sport.sum[length(sport.sum)]

# f)
# Added sum margin to the data
addmargins(sport)

sport.sum <- addmargins(sport)

# Output data with sum margins
sport.sum

# g)
# The largest two groups were NFL for men at 50% and 44% of women for NBA
# Both men and women chose NHL as the second highest picked with 30% and 33% respectively.
# Women chose NFL last at 22% and men chose NBA last at 20%
prop.table(sport, 1)

# h)
# Output matrix as a mosaic plot. 
mosaicplot(sport, color="blue")

# Not sure what the requirement for "appropriate colors" means. Here is the same output with two colors if thats
# What was desired.
# mosaicplot(sport, color=c("red", "blue"))

# Barplot grouped by gender showing favorite sports for each
barplot(t(sport), xlab = "Gender", 
        beside = TRUE, legend.text = TRUE,
        main = "Favorite sport by gender",
        ylim=c(0,50), col=c("red", "blue", "green"))

# 6)
# Output midsize data set using pairs
pairs(midsize)
pairs(midsize[1:2])

# b)
# 1. The 2004 Taurus is the most expensive car out of the three brands.
# 2. The Taurus loses value the fastest out of the three cars. 
# 3. The Camry is the second most expensive and retains value over the accord on most years.
# 4. The largest jump in price is from 2003 to 2004 in the value of the Taurus. Just over 10k to 20k

# 7)
# a)
# Create vectors to contain wins for each of the 5 teams
BAL <- subset(MLBattend, select = wins, MLBattend$franchise == "BAL")[,1]
BOS <- subset(MLBattend, select = wins, MLBattend$franchise == "BOS")[,1]
DET <- subset(MLBattend, select = wins, MLBattend$franchise == "DET")[,1]
LA <- subset(MLBattend, select = wins, MLBattend$franchise == "LA")[,1]
PHI <- subset(MLBattend, select = wins, MLBattend$franchise == "PHI")[,1]

# b)
# Create a dataframe to store all 5 teams wins
MLB.wins <- data.frame(BAL,BOS, DET, LA, PHI)

# Name the columns with the team names
names <- c("BAL","BOS","DET","LA","PHI")
colnames(MLB.wins) <- names

# c)
# Boxplot for the MLB win data
boxplot(MLB.wins)

# d)
# Baltimore has the most total wins around 110
# Boston has the smallest IQR and min/max 85-90 and 75-100, with a few outliers below 60
# Baltimore has the largest overall min/max range about 55-110
# Baltimore also has the largest IQR, 75-97ish
# Detroits median seems to be the closest to the third quartile between the 5 teams

# 8)
# Initalize data sets
house <- read.csv('http://kalathur.com/house.csv', stringsAsFactors = FALSE)
senate <- read.csv('http://kalathur.com/senate.csv', stringsAsFactors = FALSE)

# Initialize count variables
house.repub <- 0
house.dem <- 0
senate.repub <- 0
senate.dem <- 0

for(i in 1:nrow(house)) {
  if (subset(house, select = Party)[i,1] == "Republican"){
    house.repub <- house.repub + 1
  }
  else{
    house.dem <- house.dem + 1
  }
}

for(i in 1:nrow(senate)) {
  if (subset(senate, select = Party)[i,1] == "Republican"){
    senate.repub <- senate.repub + 1
  }
  else{
    senate.dem <- senate.dem + 1
  }
}

# output total dems and repubs betwen the senate and house
cat("There are", house.repub, "republicans and", house.dem, "democrats in the house", "\n");
cat("There are", senate.repub, "republicans and", senate.dem, "democrats in the senate", "\n");
cat("There are", senate.repub + house.repub, "republicans and", house.dem + senate.dem, "democrats total", "\n");

# b)
# Top 10 states by number of house members
sort(table(house$State), decreasing = TRUE)[1:10]

# c)
# Store house member counts into a data frame
house.counts <- as.data.frame.table(table(house$State))

# Store state names
states <- house.counts$Var1

# Create a boxplot off that data
boxplot(house.counts$Freq)

# outliers are greater than 20, which is California, Florida, New York and Texas
states[house.counts$Freq > 20]

# d)
# Average number of years served
house.avg.dem <- mean(house[house[,3] == "Democratic",2])
house.avg.rep <- mean(house[house[,3] == "Republican",2])
senate.avg.dem <- mean(senate[senate[,3] == "Democratic",2])
senate.avg.rep <- mean(senate[senate[,3] == "Republican",2])

# Store years in years.served
years.served <- data.frame(house.avg.dem,house.avg.rep, senate.avg.dem, senate.avg.rep)

# output years.served
years.served