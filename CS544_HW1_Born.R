# CS544 HW 1
# Eric Born

# Part 1
# Assign values to the vector called 'scores'
scores <- c(58, 52, 93, 55, 54, 99, 68, 69, 98, 70)

# print the values in scores
scores

# a)
# print the total number of values in the vector
length(scores)

# Finds the middle of the vector by taking the length and dividing by half
# Then does the same but adds 1 to find the next score in the tuple
scores[c(length(scores)/2,((length(scores)/2)+1))]

# b)
# Median from scores vector
median(scores)

# Using the less than comparison operator to print True or False if the score is less than the median
scores < median(scores)

# c)
# Print all scores less than the median
scores[scores < median(scores)]

# Print all scores greater than or equal to the median
scores[scores >= median(scores)]

# d)
#Selects every other value from the vector by telling R to select the value (true), then not select (false) and repeat
scores[c(TRUE, FALSE)]

# e)
# Using paste0 concatenates LETTERS the equal sign and the vector scores together with no spaces.
paste0(LETTERS, "=", scores)

# f)
# Converts the scores vector into a matrix containing 2 columns and 5 rows. Since the data was presented as a single
# Line of numbers, I'm using false on byrow which fills the matrx by column instead.
scores.matrix <- matrix(scores, nrow = 5, ncol = 2, byrow = FALSE)
scores.matrix

# g)
# Uses 1 to grab the first row and nrow to grab the last row with a blank column indicator 
# to grab the full first and last row of the matrix.
scores.matrix[c(1, nrow(scores.matrix)), ]

# h)
# By using paste0, nrow, ncol and indexing the number of students and quizzes can dynamically be populated from 1 to the 
# max number of columns and rows.
dimnames(scores.matrix) <- list(paste0("Student", 1:nrow(scores.matrix)), paste0("Quiz", 1:ncol(scores.matrix)))

scores.matrix

# Part 2
# a)
# Creating all of the vectors to store the data before moving into a dataframe
weather.month <- c("January", "February", "March",  "April", "May", "June", "July", "August", "September", "October", "November", "December")
weather.mon_avg <- c(4.7, 6.1, 12.8, 23.9, 35.5, 45.0, 49.1, 48.1, 41.6, 30.2, 20.7, 10.1)
weather.dalmax_avg <- c(13.6, 14.7, 20.7, 30.4, 41.3, 50.4, 54.1, 53.3, 47.1, 36.4, 28.1, 18.4)
weather.dalmin_avg <- c(-4.1, -2.4, 5.0, 17.4, 29.8, 39.5, 44.0, 43.0, 36.1, 24.0, 13.3, 1.7)
weather.rec_high <- c(48, 43, 54, 60, 66, 72, 71, 72, 69, 62, 52, 47)
weather.rec_low <- c(-47, -46, -38, -20, -2, 8, 24, 20, 9, -5, -20, -46)

# Creating the dataframe using data stored in the vectors
weather.info <- data.frame(
  Month = weather.month,
  Monthly_Avg = weather.mon_avg,
  DailyMax_Avg = weather.dalmax_avg,
  DailyMin_Avg = weather.dalmin_avg,
  Record_High = weather.rec_high,
  Record_Low = weather.rec_low)

# b)
# Sum of columns from the data frame
sum(weather.info$Monthly_Avg) 
sum(weather.info$DailyMax_Avg)
sum(weather.info$DailyMin_Avg) 
sum(weather.info$Record_High)
sum(weather.info$Record_Low)

# c)
# Only data from Month, Record_High, and Record_Low columns
weather.info[c(1,5,6)]

# d)
# First and last row using 1 and nrow to find the total number of rows in the dataframe
weather.info[c(1, nrow(weather.info)), ]

# e)
# Only display rows where the daily max average is greater than 40 
weather.info[weather.info$DailyMax_Avg > 40, ]

# f)
# Adding new column Record_Deviation to the data frame
weather.info$Record_Deviation <- 
  weather.info$Record_High - weather.info$Record_Low

weather.info