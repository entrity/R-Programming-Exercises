# Function "rankhospital" takes three arguments: the 2-character abbreviated
# name of a state (state), an outcome (outcome), and the ranking of a hospital
# in that state for that outcome (num). The function reads the outcome-of-
# care-measures.csv file and returns a character vector with the name of the
# hospital that has the ranking specified by the num argument. For example,
# the call

## rankhospital("MD", "heart failure", 5)

# would return a character vector containing the name of the hospital with the
# 5th lowest 30-day death rate for heart failure. The num argument can take
# values “best”, “worst”, or an integer indicating the ranking (smaller
# numbers are better). If the number given by num is larger than the number of
# hospitals in that state, then the function should return NA. Hospitals that
# do not have data on a particular outcome should be excluded from the set of
# hospitals when deciding the rankings.

# It may occur that multiple hospitals have the same 30-day mortality rate for
# a given cause of death. In those cases ties should be broken by using the
# hospital name. For example, in Texas (“TX”), the hospitals with lowest
# 30-day mortality rate for heart failure are shown here.

rankhospital <- function(state, outcome, num = "best") {
	# Validate args
	if (class(state) != "character")
		stop("invalid state")
	if (nchar(state) != 2)
		stop("invalid state")
	if (outcome == "heart attack")
		colname = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	else if (outcome == "heart failure")
		colname = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	else if (outcome == "pneumonia")
		colname = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	else
		stop("invalid outcome")
	# Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
	rowSelect <- suppressWarnings(!is.na(as.numeric(data[,colname]))) & data$State == state
	colSelect <- c(colname, "Hospital.Name")
	values <- data[rowSelect, colSelect]
	# Check that state is valid
	if (nrow(data[data$State == state,]) == 0)
		stop("invalid state")
	# Return hospital name in that state with the given rank
	# 30-day death rate
	rankings <- order(values[colname], values$Hospital.Name, decreasing=(num=="worst"))
	if (num == "best" | num == "worst")
		values[rankings[1], "Hospital.Name"]
	else
		values[rankings[num], "Hospital.Name"]
}
