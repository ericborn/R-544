#############
# Start setup
#############

# Install rmarkdown, plyr and plotly
if (!is.element("rmarkdown", installed.packages()[,"Package"]))
  install.packages("rmarkdown", repos="http://cran.us.r-project.org", dependencies = TRUE)

if (!is.element("plyr", installed.packages()[,"Package"]))
  install.packages("plyr", repos="http://cran.us.r-project.org", dependencies = TRUE)

if (!is.element("plotly", installed.packages()[,"Package"]))
  install.packages("plotly", repos="http://cran.us.r-project.org", dependencies = TRUE)

# Load plyr, plotly and sampling libraries
library(rmarkdown)
library(plyr)
library(plotly)
library("sampling")

###############
# Start data storage
# Set path to the csv file location
setwd("C:/Users/TomBrody/Desktop/School/544/Final project")

####
# cleanup steps
####

# Import the csv as a dataframe
beer.dirty <- read.csv("beer_reviews_dirty.csv")

# Remove columns 3, 5-7, 9-10 and 13 which are not being used during this analysis
beer.dirty <- beer.dirty[, c(1,2,4,8,11,12)]

# Remove all rows with the a beer review score of 0 (7 rows)
beer.dirty <- beer.dirty[!beer.dirty$review_overall %in% 0, ]

# Create a table with beer names and total frequency
dirty.beer.table <- data.frame(table(beer.dirty$beer_name))

# Isolate out just the beer names with a single review
dirty.beer.list <- dirty.beer.table[dirty.beer.table$Freq < 2, ]

#table(beer.dirty[is.na(beer.dirty), ])

#beer <- beer[colSums(!is.na(beer)) > 0]
#str(beer)

# Remove all beers from the main dataset that had only 1 review
beer <- beer.dirty[!beer.dirty$beer_name %in% dirty.beer.list$Var1, ]

# drop empty factor levels
beer <- droplevels(beer)

# Reset rownames from 1 to n
rownames(beer) <- 1:nrow(beer)

# check structure
str(beer)

####
# CSV save/load steps
####

# save out cleaned data to a new csv file
#write.csv(beer.dirty, file = "beer_reviews_cleaned.csv", row.names = FALSE)

# Read direct from cleaned CSV file
#beer <- read.csv("beer_reviews_cleaned.csv", na.strings = c('NA', 'NULL'))

#############
# End setup
#############

#############
# Start basic stats
#############

# Basic dataset stats
# 6 columns and 1567698 rows
# str(beer)
# nrow(beer)

# Total distinct beers 56857
# length(beerz$Var1)

# Average reviews per beer 28
# mean(beer$Freq)

# After removing all beers with only 1 review
# Total distinct beers 37948
# length(beer.table$Var1)

# Average reviews 41
# mean(beer.table$Freq)

# Average ABV 7.1
#round(mean(beer$beer_abv, na.rm = TRUE), digits = 1)

# Highest ABV beer reviewed 43
#max(beer$beer_abv, na.rm = TRUE)

# table with counts of total reviews by beer
beer.table <- data.frame(table(beer$beer_name))

# new table for only beers that have 2 or more reviews 11932
#beer.two <- beer.table[beer.table$Freq < 5, ]

# Create df for basic stats
basic.stats <- data.frame("Measure" = c("Total rows", "Total distinct Beers", 
                                        "Average reviews per beer", "Total distinct beers after cleaning", 
                                        "Average reviews after cleaning", "Average ABV",
                                        "Highest ABV"),
                          "Total" = c(nrow(beer), length(dirty.beer.table$Var1), 
                                      round(mean(dirty.beer.table$Freq, digits = 0)),
                                      length(beer.table$Var1), 
                                      round(mean(beer.table$Freq), digits = 0),
                                      round(mean(beer$beer_abv, na.rm = TRUE), digits = 1),
                                      max(beer$beer_abv, na.rm = TRUE)))

# reset factors to order by Measure column
basic.stats$Measure <- factor(basic.stats$Measure, 
                              levels = c(as.character(basic.stats$Measure)))

# Convert factors to character
basic.stats$Measure <- as.character(basic.stats$Measure)


# create table for top 10 abv's, total reviews and average review score
stat.table <- plot_ly(
  type = 'table',
  height = 250,
  width = 700,
  header = list(
    values = c('Measure', 'Total'),
    line = list(width = 1, color = 'black'),
    fill = list(color = c('#1f77b4', '#1f77b4')),
    font = list(famile = 'Arial', size = 14, color = 'white')
  ),
  cells = list(
    values = rbind(basic.stats$Measure, basic.stats$Total),
    align = c('center'),
    line = list(width = 1, color = 'black')
  ))
# Output table

stat.table

#############
# End basic stats
#############

#############
# Start Top 10 reviewed beers by abv
#############
# Top 10 reviewed beers by abv 133896
abv.top10 <- sort(table(beer$beer_abv), decreasing = TRUE)[1:10]

# Convert to dataframe
abv.top10.df <- data.frame(table(beer$beer_abv))

# Rename column
names(abv.top10.df) <- c("beer_abv", "Freq")

# Calculate average review scores for top 10 beers
abv.top10.averages <- aggregate(review_overall ~ beer_abv, data = beer, mean)

# Round averages
abv.top10.averages$review_overall <- round(abv.top10.averages$review_overall, digits = 2)

# Merge beer abv, overall review and total reviews
abv.top10.df <- merge(x = abv.top10.averages, y = abv.top10.df, by = "beer_abv", all = FALSE)

attach(abv.top10.df)
abv.top10.df <- abv.top10.df[order(-Freq),]
detach(abv.top10.df)

# Limit to top 10
abv.top10.df <- abv.top10.df[1:10, ]


# Reset rownames from 1 to n
rownames(abv.top10.df) <- 1:nrow(abv.top10.df)

# create table for top 10 abv's, total reviews and average review score
abv.table <- plot_ly(
  type = 'table',
  height = 300,
  width = 500,
  header = list(
    values = c("Beer ABV", "Total Reviews", "Average Rating"),
    align = c('center'),
    line = list(width = 1, color = 'black'),
    fill = list(color = c('#1f77b4', '#1f77b4')),
    font = list(famile = 'Arial', size = 14, color = 'white')
  ),
  cells = list(
    values = rbind(abv.top10.df$beer_abv, abv.top10.df$Freq, 
                   abv.top10.df$review_overall),
    align = c('center'),
    line = list(width = 1, color = 'black')
  ))
# Output table
abv.table

# Percent these make up from the total dataset 38.3%
paste(round(sum(abv.top10) / nrow(beer) * 100, digits = 1), "%", sep = '')

#############
# End Top 10 reviewed beers by abv
#############

#############
# Start top 10 most reviewed beer by name 
#############

# Create counts by beer name
beer.name.count <- count(beer, vars = c("beer_name", "brewery_name"))

# order beer.name.count highest to lowest
attach(beer.name.count)
beer.name.count <- beer.name.count[order(-freq),]
detach(beer.name.count)

# beer.top10
beer.top10 <- beer.name.count[1:10, ]

# Reset rownames from 1 to n
rownames(beer.top10) <- 1:nrow(beer.top10)

# drop empty factor levels
beer.top10 <- droplevels(beer.top10)

# reset factors to order by frequency decending
beer.top10$beer_name <- factor(beer.top10$beer_name, 
                               levels = c(as.character(beer.top10$beer_name)))

#############
# End top 10 most reviewed beer by name bar graph
#############

#############
# Start Average ratings for the top 10 most reviewed beers
#############

# Calculate average review scores for top 10 beers
beer.averages <- aggregate(review_overall ~ beer_name, data = beer, mean)

# Round averages
beer.averages$review_overall <- round(beer.averages$review_overall, digits = 2)

# Merge averages with top 10 most reviewed beers
beer.top10.avg <- merge(x = beer.averages, y = beer.top10, by = "beer_name", all = FALSE)

# drop empty factor levels
beer.top10.avg <- droplevels(beer.top10.avg)

# Reorder by total reviews
attach(beer.top10.avg)
beer.top10.avg <- beer.top10.avg[order(-freq),]
detach(beer.top10.avg)


# Reset rownames from 1 to n
rownames(beer.top10.avg) <- 1:nrow(beer.top10.avg)

# reset factors to order by frequency decending
beer.top10.avg$beer_name <- factor(beer.top10.avg$beer_name, 
                                   levels = c(as.character(beer.top10.avg$beer_name)))

#####
# Plot 
#####
# Create Y axis label
# Display top 10 most reviewed beers by name
y <- list(title = "Number of Reviews")
x <- list(title = 'Beer Name')
label <- c('4.2 average rating','4.2 average rating','4.2 average rating','4.3 average rating',
           '4.1 average rating','4.2 average rating','4.2 average rating','4.3 average rating',
           '4.6 average rating','4.3 average rating') 

plot.top10.name <- plot_ly(beer.top10.avg, x = ~beer_name, y = ~freq, type = 'bar', text = label) %>% 
  layout(yaxis = y, title = "Top 10 most reviewed beers by name", xaxis = x)

# Draw plot
plot.top10.name
# most reviewed beer by name


#############
# End Average ratings for the top 10 most reviewed beers
#############

#############
# Start Top 10 most reviews by style
#############

# Most reviewed style
beer.style.count <- count(beer, vars = "beer_style")

# order beer.style.count highest to lowest
attach(beer.style.count)
beer.style.count <- beer.style.count[order(-freq),]
detach(beer.style.count)

# Select only the top 10
style.top10 <- beer.style.count[1:10, ]

# Reset rownames from 1 to n
rownames(style.top10) <- 1:nrow(style.top10)

# drop empty factor levels
style.top10 <- droplevels(style.top10)

# reset factors to order by frequency decending
style.top10$beer_style <- factor(style.top10$beer_style, 
                                 levels = c(as.character(style.top10$beer_style)))

#Display top 10 most reviewed beers by style
y <- list(title = "Number of Reviews")
x <- list(title = 'Beer Name')
text <- c('4.0 Average review')
plot.top10.style <- plot_ly(style.top10, x = ~beer_style, y = ~freq, type = 'bar') %>% 
  layout(xaxis = x, yaxis = y, title = "Top 10 most reviewed beers by style")

# Draw plot
plot.top10.style

#############
# End Top 10 most Reviews by style bar graph
#############

#############
# Start Top 10 most Reviews by style with averages table
#############

# Calculate average review scores for top 10 beers
style.averages <- aggregate(review_overall ~ beer_style, data = beer, mean)

# Round averages
style.averages$review_overall <- round(style.averages$review_overall, digits = 2)


# Merge beer style, overall review and total reviews
beer.top10.style.avg <- merge(x = style.averages, y = style.top10, by = "beer_style", 
                              all = FALSE)

attach(beer.top10.style.avg)
beer.top10.style.avg <- beer.top10.style.avg[order(-review_overall),]
detach(beer.top10.style.avg)

# drop empty factor levels
beer.top10.style.avg <- droplevels(beer.top10.style.avg)

attach(beer.top10.style.avg)
beer.top10.style.avg <- beer.top10.style.avg[order(-freq),]
detach(beer.top10.style.avg)

# reset factors to order by frequency decending
beer.top10.style.avg$beer_style <- factor(beer.top10.style.avg$beer_style, 
                                          levels = c(as.character(beer.top10.style.avg$beer_style)))

# Reset rownames from 1 to n
rownames(beer.top10.style.avg) <- 1:nrow(beer.top10.style.avg)

# Revert factor levels to characters
beer.top10.style.avg$beer_style <- as.character(beer.top10.style.avg$beer_style)

# create table for top 10 styles, total reviews and average review score
style.table <- plot_ly(
  type = 'table',
  height = 300,
  header = list(
    values = c("Beer Style", "Total Reviews", "Average Rating")
  ),
  cells = list(
    values = rbind(beer.top10.style.avg$beer_style, beer.top10.style.avg$freq, 
                   beer.top10.style.avg$review_overall)
  ))
# Output table
style.table

#############
# End Top 10 most Reviews by style with averages table
#############

#############
# Start Top 10 by style with ABV boxplot
#############

# BOXPLOT top 10 beer styles and their ABV's
# Create dataset that contains the top 10 most reviewed ABV's and abv
top.10.abv <- beer[beer$beer_style %in% beer.top10.style.avg$beer_style, c(4,6)]

# Reset rownames from 1 to n
rownames(top.10.abv) <- 1:nrow(top.10.abv)

# drop empty factor levels
top.10.abv <- droplevels(top.10.abv)

### Lists are out of order, ABV diff being applied to the wrong items
### numbers not needed
# # Store min/max ABV's by style
# top.10.abv.min <- aggregate(beer_abv ~ beer_style, top.10.abv, function(x) min(x))
# top.10.abv.max <- aggregate(beer_abv ~ beer_style, top.10.abv, function(x) max(x))
# 
# # calculate difference between min and max abv by style
# max.min.diff <- top.10.abv.max[2] - top.10.abv.min[2]
# 
# # Bind top 10 styles with min/max difference in ABV
# style.top10 <- cbind(style.top10, max.min.diff)
# 
# # Rename column to abv_diff
# colnames(style.top10)[colnames(style.top10)=="beer_abv"] <- "ABV_diff"


# Create boxplot based on top 10 beer styles with ABV information
y <- list(title = "Beer Style")
x <- list(title = 'ABV')
abv.box <- plot_ly(top.10.abv, x = ~beer_abv, y = ~beer_style, type = 'box',
                   size = 2)%>% 
  layout(xaxis = x, yaxis = y, title = "ABV distribution of top 10 beer styles")

# Draw plot
abv.box

#############
# End Top 10 by style with ABV boxplot
#############

#############
# Start review breakdown table and chart creation
#############

# Create counts based upon review overall rating
review <- count(beer, vars = "review_overall")

# Reorder review table based upon review_overall column
attach(review)
review <- review[order(-review_overall), ]
detach(review)

# create table for top 10 abv's, total reviews and average review score
review.table <- plot_ly(
  type = 'table',
  header = list(
    values = c("Rating", "Total")
  ),
  cells = list(
    values = rbind(
      review$review_overall, review$freq)
    
  ))
# Output table
review.table

# Create x and Y axis label
# Create bar chart
y <- list(title = "Number of Reviews")
x <- list(title = 'Review Rating')
plot.review <- plot_ly(review, x = ~review_overall, y = ~freq, type = 'bar') %>% 
  layout(xaxis = x, yaxis = y, title = "Total reviews by rating")

# Draw chart
plot.review

#############
# End review breakdown table and chart creation
#############

#############
# Start central limit theorem and histograms on overall_review ratings
#############

# Samples
set.seed(3425)

# Sample size and total samples
x10 <- 10
x20 <- 50
x30 <- 100
x40 <- 200

samples <- 1000

# Create place holder of 1000 0 values in xbar.1k variables
xbar.1k10 <- numeric(samples)
xbar.1k20 <- numeric(samples)
xbar.1k30 <- numeric(samples)
xbar.1k40 <- numeric(samples)

# Replace each 0 value with a random value from the queries data set using sample
for (i in 1: samples) {
  xbar.1k10[i] <- mean(sample(beer$review_overall, size = x10, replace = TRUE))
}

for (i in 1: samples) {
  xbar.1k20[i] <- mean(sample(beer$review_overall, size = x20, replace = TRUE))
}

for (i in 1: samples) {
  xbar.1k30[i] <- mean(sample(beer$review_overall, size = x30, replace = TRUE))
}

for (i in 1: samples) {
  xbar.1k40[i] <- mean(sample(beer$review_overall, size = x40, replace = TRUE))
}

# Mean and SD of whole review_overall column 3.80093, 0.7404
mean(beer$review_overall)
sd(beer$review_overall)

# Mean and SD of sampled review_overall column
# Sample size 10
# Mean 3.8008, SD 0.23604
mean(xbar.1k10)
sd(xbar.1k10)

# Sample size 20
# Mean 3.80145, SD 0.16426
mean(xbar.1k20)
sd(xbar.1k20)

# Sample size 30
# Mean 3.80305, SD 0.13469
mean(xbar.1k30)
sd(xbar.1k30)

# Sample size 40
# Mean 3.808275, SD 0.118075
mean(xbar.1k40)
sd(xbar.1k40)

# Create histogram plots from samples
k10 <- plot_ly(x = xbar.1k10, type = "histogram", name = "10")
k20 <- plot_ly(x = xbar.1k20, type = "histogram", name = "50")
k30 <- plot_ly(x = xbar.1k30, type = "histogram", name = "100")
k40 <- plot_ly(x = xbar.1k40, type = "histogram", name = "200")

# Draw plots
subplot(k10, k20, k30, k40, nrows = 2) %>% 
  layout(title = "Overall Review Distribution")

#############
# End samples and hisgrams on overall_review ratings
#############

#############
# Start Simple random sample without replacement
#############

# Set seed for repeatability
set.seed(6546)

# 30000 Random sample without replace stored in variable s1
s1 <- srswor(30000, nrow(beer))

# Using variable s1 as the row index to map the results which are not equal to 0 
# from beer into the sample.1 variable
sample.1 <- beer[s1 != 0, ]

# Output the differences in frequencies for each region
round(table(beer$review_overall) / length(beer$review_overall), 3) -
  round(table(sample.1$review_overall) / length(sample.1$review_overall), 3)

# Store histogram from sample without replacement data
plot.samp.wor <- plot_ly(sample.1, x = ~review_overall, type = "histogram",
                         name = "SRSWOR")

#############
# End Simple random sample without replacement
#############

#############
# Start systematic sampling
#############

# Set seed for repeatability
set.seed(999)

# Store the inclusion probabilities from beer dataset 
# column review_overall with a sample size of 36000
pik <- inclusionprobabilities(beer$review_overall, 250000)

# Perform systematic sampling based on the inclusion probabilities captured from the dataset
s <- UPsystematic(pik)

# Store the rows using s as the index for the MU dataset
samp.syst <- beer[s != 0, ]

# Output the differences in frequencies for each region
round(table(beer$review_overall) / length(beer$review_overall), 3) -
  round(table(samp.syst$review_overall) / length(samp.syst$review_overall), 3)

# Store histogram from sample without replacement data
plot.samp.syst <- plot_ly(samp.syst, x = ~review_overall, type = "histogram",
                          name = "Systematic Sampling")

#############
# End systematic sampling
#############

#############
# Start stratified sampling
#############

# Set seed for repeatability
set.seed(1234)

# Order the beer dataset by the review_overall column
order.index <- order(beer$review_overall)

# Store ordered data
data <- beer[order.index, ]

# Store dataset frequency
freq <- table(beer$review_overall)

# create group sizes for each region
sizes <- round(1000 * freq / sum(freq))

# Create a stratified sample using the group sizes
st <- strata(data, stratanames = c("review_overall"),
             size = sizes, method = "srswor")

# Draws the sample from the dataset
samp.strat <- getdata(data, st)

# Output the differences in frequencies for each region
round(table(beer$review_overall) / length(beer$review_overall), 3) -
  round(table(samp.strat$review_overall) / length(samp.strat$review_overall), 3)

# Store histogram from sample without replacement data
plot.samp.strat <- plot_ly(samp.strat, x = ~review_overall, type = "histogram",
                           name = "Stratified")

#############
# End stratified sampling
#############

#############
# Draw stratified sampling plots
#############

# Store histogram of review_overall data from original table
plot.nosample <- plot_ly(beer, x = ~review_overall, type = "histogram",
                         name = "All review ratings")

# Draw plots for all sampling methods
subplot(plot.nosample, plot.samp.wor, plot.samp.syst, plot.samp.strat, nrows = 2)