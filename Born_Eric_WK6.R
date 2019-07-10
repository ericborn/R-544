# Eric Born
# CS 544
# Part 1)
# a)

# install and load stringr
if (!is.element("stringr", installed.packages()[,"Package"]))
  install.packages("stringr", repos="http://cran.us.r-project.org", 
                   dependencies = TRUE)

library(stringr)


# Initialize data
file <- "http://kalathur.com/cs544/data/lincoln.txt"
words <- scan(file, what=character())

# Detect and show all words that have a punctuation symbol
words[str_detect(words, "[:punct:]")]

# b) Replace all words with punctuation with an empty string
words <- str_replace_all(words, "[:punct:]", "")

# Output words with removed punctuation marks
words

# c)
# Display the frequencies of the word lengths
str_length(words)

hist(str_length(words), main = "Length of words",
     ylim = c(0, 70), xlim = c(1, 11))


# d)
# finds the maximum length of a word within the dataset
longest <- max(str_length(words))

# Output longest
longest

# Creates a regex using str_c and the longest word value found previously
# built it with paste before I saw the professors example using str_c
# paste("\\b[a-zA-Z0-9]{",longest,",",longest,"}\\b", sep = "")
filter <- str_c("\\b[a-zA-Z0-9]{",longest,",",longest,"}\\b")

# Returns the words that match the previously created filter
words[str_detect(words, filter)]

# e)
# Display all words starting with the letter p by using the carrot symbol
words[str_detect(words, "^p")]

# f)
# Display all words ending with the letter r by using the dollar sign symbol
words[str_detect(words, "r$")]

# g)
# All words that start with p by using ^p to start with p, 
# followed by .* to indicate zero or more of any character
# Finally r$ to find words that end with r
words[str_detect(words, "(^p)(.*)(r$)")]

# Part 2
# install and load tidyverse
if (!is.element("tidyverse", installed.packages()[,"Package"]))
  install.packages("tidyverse", repos="http://cran.us.r-project.org", 
                   dependencies = TRUE)
if (!is.element("plotly", installed.packages()[,"Package"]))
  install.packages("plotly", repos="http://cran.us.r-project.org", 
                   dependencies = TRUE)

library(tidyverse)
library(plotly)

# a)
# Set wd
setwd("C:/Users/TomBrody/Desktop/School/544/wk6")

# Online CSV
# temps.df <- read.csv("http://people.bu.edu/kalathur/usa_daily_avg_temps.csv")

# Local csv
temps.df <- read.csv("usa_daily_avg_temps.csv")

# convert df to tibble
usaDailyTemps <- as_tibble(temps.df)

# b)
# Find the maximum temperature for each year using group and max() on avgtemp column
maxTemps.year <- usaDailyTemps %>%
                    group_by(year) %>%
                    summarise(maxTemp = max(avgtemp))

# output max temps by year
maxTemps.year

# Plot the max temp data
# Bar chart
# Easier to see the temps with a line graph
#plot_ly(maxTemps.year, type = "bar", x = ~year, y = ~maxTemp)

# Line graph
plot_ly(maxTemps.year, type = 'scatter', mode = 'lines', x = ~year, y = ~maxTemp)

# c)
# Find the maximum temperature for each year using group and max() on avgtemp column
maxTemps.state <- usaDailyTemps %>%
                    group_by(state) %>%
                    summarise(maxTemp = max(avgtemp))

# output max temps by state
maxTemps.state

# Plot the max temp data
# Bar chart
plot_ly(maxTemps.state, type = "bar", x = ~state, y = ~maxTemp)

# Line chart
# Easier to see the max temps by state with a bar graph
# plot_ly(maxTemps.state, type = 'scatter', mode = 'lines', x = ~state, y = ~maxTemp)

# d)
# Filter on city == Boston
bostonDailyTemps <- usaDailyTemps %>%
                      filter(city == 'Boston')

# Output Boston data
bostonDailyTemps

# e)
# Boston average temps by month
boston.avg <- bostonDailyTemps %>%
                group_by(month) %>%
                summarise(monthAvg = mean(avgtemp))

# Output Boston average monthly temps
boston.avg

# Bar chart
plot_ly(boston.avg, type = "bar", x = ~month, y = ~monthAvg)
