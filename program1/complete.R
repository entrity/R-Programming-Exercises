# This file is an exercise for Coursera's course in R programming, presented by
# Johns Hopkins University. Written by Markham Anderson.

# Write a function that reads a directory full of files and reports the number
# of completely observed cases in each data file. The function should return a
# data frame where the first column is the name of the file and the second
# column is the number of complete cases. A prototype of this function follows

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
# 
## 'id' is an integer vector indicating the monitor ID numbers
## to be used
# 
## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
complete <- function(directory, id = 1:332) {
	dir0 <- getwd()
	setwd(directory)
	output <- data.frame()
	i <- 0
	for (number in id) {
		i <- i + 1
		data <- read.csv(sprintf("%03d.csv", as.integer(number)))
		output[i, "id"] <- as.integer(number)
		output[i, "nobs"] <- length(data[complete.cases(data), "ID"])
	}
	setwd(dir0)
	output
}
