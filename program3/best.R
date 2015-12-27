# Function "best" takes two arguments: the 2-character abbreviated name of a
# state and an outcome name. The function reads the outcome-of-care-
# measures.csv file and returns a character vector with the name of the
# hospital that has the best (i.e. lowest) 30-day mortality for the specified
# outcome in that state. The hospital name is the name provided in the
# Hospital.Name variable. The outcomes can be one of “heart attack”, “heart
# failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the
# rankings.

# If there is a tie for the best hospital for a given outcome, then the hospital
# names should be sorted in alphabetical order and the first hospital in that
# set should be chosen (i.e. if hospitals “b”, “c”, and “f” are tied for best,
# then hospital “b” should be returned).

best <- function(state, outcome) {
	# check validity of arguments
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
	# Check that state is valid
	if (nrow(data[data$State == state,]) == 0)
		stop("invalid state")
	# Get relevant values
	rowSelect <- suppressWarnings(!is.na(as.numeric(data[,colname]))) & data$State == state
	colSelect <- c(colname, "Hospital.Name")
	values <- data[rowSelect, colSelect]
	values[,colname] <- as.numeric(values[,colname])
	values <- values[!is.na(values[colname]),]
	# Return hospital name in that state with lowest 30-day death rate
	maxval <- max(values[colname])
	besties <- values[values[colname] == maxval, "Hospital.Name"]
	sort(besties)[1]
}
