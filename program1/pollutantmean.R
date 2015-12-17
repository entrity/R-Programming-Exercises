# This file is an exercise for Coursera's course in R programming, presented by
# Johns Hopkins University. Written by Markham Anderson.

# Write a function named 'pollutantmean' that calculates the mean of a pollutant
# (sulfate or nitrate) across a specified list of monitors. The function
# 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'.
# Given a vector monitor ID numbers, 'pollutantmean' reads that monitors'
# particulate matter data from the directory specified in the 'directory'
# argument and returns the mean of the pollutant across all of the monitors,
# ignoring any missing values coded as NA. A prototype of the function is as
# follows
# 
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
# 
## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".
# 
## 'id' is an integer vector indicating the monitor ID numbers
## to be used
# 
## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
## NOTE: Do not round the result!
pollutantmean <- function (directory, pollutant, id) {
	sum <- 0
	count <- 0
	for (file in list.files(directory)) {
		fname <- paste(directory, file, sep="/")
		data <- read.csv(fname, header=TRUE)
		pollutantlevels <- data[any(data["ID"] == id) & !is.na(data[pollutant]), pollutant]
		sum <- sum + sum(pollutantlevels)
		count <- count + length(pollutantlevels)
	}
	sum / count
}
