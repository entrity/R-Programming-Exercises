# Function "rankall" takes two arguments: an outcome name (outcome) and a
# hospital ranking (num). The function reads the outcome-of-care-measures.csv
# file and returns a 2-column data frame containing the hospital in each state
# that has the ranking specified in num. For example the function call
# rankall("heart attack", "best") would return a data frame containing the names
# of the hospitals that are the best in their respective states for 30-day heart
# attack death rates. The function should return a value for every state (some
# may be NA). The first column in the data frame is named hospital, which
# contains the hospital name, and the second column is named state, which
# contains the 2-character abbreviation for the state name. Hospitals that do
# not have data on a particular outcome should be excluded from the set of
# hospitals when deciding the rankings.

# The rankall function should handle ties in the 30-day mortality rates in the same way
# that the rankhospital function handles ties.
# The function should use the following template.


rankall <- function(outcome, num = "best") {
	# Validate args
	if (class(state) != "character")
		stop("invalid state")
	if (nchar(state) != 2)
		stop("invalid state")
	if (outcome == "heart attack")
		colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	else if (outcome == "heart failure")
		colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	else if (outcome == "pneumonia")
		colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	else
		stop("invalid outcome")
	# Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
	byState <- split(data, data$State)
	# For each state, find the hospital of the given rank
	output <- data.frame()
	for (state in names(byState)) {
		values <- byState[[state]]
		rankings <- order(values[colname], values["Hospital.Name"], decreasing=(num=="worst"))
		output[state, "hospital"] <-
			if (num == "best" | num == "worst")
				values[rankings[1], "Hospital.Name"]
			else
				values[rankings[num], "Hospital.Name"]
		output[state, "state"] <- state
	}
	# Return a data frame with the hospital names and the
	# (abbreviated) state name
	output
}