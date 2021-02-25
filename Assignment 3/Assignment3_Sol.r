## Import libraries
library(dplyr)
library(stringr)
library(quantmod)
library(lubridate)
library(tidyr)
library(reshape2)
library(ggplot2)

# ## Import data
setwd("C:/R_ws/R_Spring21/Assignment 3")
# Q1
# Write a function to summarize the statistics (mean, std. dev, max and min) for a specified subset of call
# types (totalCalls, answeredCalls and/or sales) for a specified subset of id numbers.
# Inputs: The function should accept two arguments – callTypes and id
# • callTypes: a character vector indicating one or more of the types of call (totalCalls,
# answeredCalls, sales) to summarize. “All” may be entered, in which case all three call types are
# summarized.
# • id: an integer vector indicating the employee id numbers to be included in the summary
# Output: The function should return a dataframe that has a row for each callType specified and a column
# for each summary statistic (mean, std. dev., max and min). See sample output for more details.
# • The function should verify that all of the elements of the callTypes argument are valid entries
# and return an error message if this condition is not met

callMeans <- function(callTypesArg, id){

	# Initialize empty data frame where we would merge all the CSVs
	# We need to initialize an empty data frame because this can later be used to merge data easily with CD <- rbind(CD, CD_ID)
	# In the below data frame, each entry if defined by their "data type". It could be integer, numeric, character, etc
	CD <- data.frame(dates= as.Date(character()), id= integer(0), totalCalls = integer(0), answeredCalls = integer(0), sales = integer(0))
	
	# Run for loop for all the "id" given in the function argument
	# if id = [1 , 2, 6, 10] , val would be [1, 2, 6, 10]
	for (val in id)
	{
		# Because of the filename, we want to convert integers 2 or 8 in form 002 and 008
		val_id = sprintf("%03d", val)
		# paste0 combines 2 strings together. For instance: paste0("Hi","Shlok") would be "HiShlok". It is used here to get the path of the file.
		path <- paste0(paste0("callData/",val_id), ".csv")
		# read the CSV with particular id
		CD_ID <- read.csv(path, header = TRUE)
		# merge all the rows into the existing data frame. This is later used to get the summary
		CD <- rbind(CD, CD_ID)
	}

	# Initialize an empty data frame to store all the summary information. This is again needed if we want to run rbind
	result <- data.frame(callTypes= numeric(0), mean= numeric(0), sd = numeric(0), max = numeric(0), min = numeric(0))
	
	# If there is only one argument and it contains "all", make your own column vector by adding all the columns
	if(length(callTypesArg) == 1)
	{
		if(callTypesArg == "all")
		{
			callTypesArg = c("totalCalls", "answeredCalls", "sales")
		}
	}

	# for loop over the "merged" columns
	for (val in callTypesArg)
	{

		# This needs to be done because the function argument is a string and the mean, sd, max and min function
		# need the data of the column and not the name of the column. If you pass it a string, you would be passing it the name of the column.
		# In turn, you want to pass it the data in the column.
		if(val == "totalCalls" || val == c("totalCalls")){
			cData <- CD$totalCalls
		} else if(val == "answeredCalls" || val == c("answeredCalls")){
			cData <- CD$answeredCalls
		} else if(val == "sales" || val == c("sales")) {
			cData <- CD$sales
		} else {
			# If the column names don't match, it is an invalid argument
			stop("invalid call type")
		}

		# summarize the merged data by mean, sd, max and min.
		SCD <- as.data.frame(CD %>%  dplyr::summarize( 
														mean = mean(cData, na.rm = TRUE),
														sd = sd(cData, na.rm = TRUE),
														max = max(cData, na.rm = TRUE),
														min = min(cData, na.rm = TRUE))
									)
		
		# store the summary in the form of a data type because the output needs to be in this format
		res <- data.frame(callTypes = val, mean = SCD$mean, sd = SCD$sd, max = SCD$max, min = SCD$min)
		# merge the data frame in the existing data frame.
		result <- rbind(result, res)
	}

	# return the result
	return(result)
}

# sample function calls
op <- callMeans(callTypesArg = "all", id = 1:5)
print(op)
op <- callMeans(callTypesArg = c("totalCalls", "sales"), id = 100:125)
print(op)
op <- callMeans(callTypesArg = "Calls", id = 1:175)
print(op)
op <- callMeans(callTypesArg = c("answeredCalls"), id = c(2,6,17,29,100))
print(op)

## Q2
# Write a function to summarize the statistics on employee productivity for a specified subset of locations
# (states).
# Inputs: The function should accept two arguments –empData and state
# • empData: the empData dataframe loaded from empData.csv
# • state: a character vector indicating one or more states to summarize. “All” may be entered, in
# which case all states are summarized.
# Output: The function should return a dataframe that has a row for each city in each of the states
# specified in the state argument and columns containing the following summary statistics.
# • Employees: the total number of employees for the city
# • totalCalls: the total number of “totalCalls” for all employees in the city
# • totalDays: the total number of days on which calls were placed by all employees in the city
# • avgCallsPerEmployee: totalCalls / # of Employees
# • avgDaysPerEmployee: totalDays / # of Employees
# • avgCallsPerDay: totalCalls / totalDays

callVolume <- function(empData, state){

	# Init
	result <- NULL

	# Filter data based on state
	for (state_val in state) {
	   
		state_data = filter(empData, empData$State == state_val)
		for (city_val in unique(state_data$City)) {
			
			# Filter data based on city
			city_data = filter(state_data, state_data$City == city_val)


			# empty initialize CD
			CD <- NULL
			# get all employee information for that city
			for(i in city_data$EmpID){
				# Because of the filename, we want to convert integers 2 or 8 in form 002 and 008
				val_id = sprintf("%03d", i)
				# paste0 combines 2 strings together. For instance: paste0("Hi","Shlok") would be "HiShlok". It is used here to get the path of the file.
				path <- paste0(paste0("callData/",val_id), ".csv")
				# read the CSV with particular id
				CD_ID <- read.csv(path, header = TRUE)
				# remove NAN rows to calculate number of days each employee worked
				CD_ID <- na.omit(CD_ID)
				# merge all the rows into the existing data frame. This is later used to get the summary
				CD <- rbind(CD, CD_ID)
		}

			totalDays <- nrow(CD)
			totalCalls <- sum(CD$totalCalls)
			numEmployees <- nrow(city_data)
			avgCallsPerEmployee <- round(totalCalls/numEmployees)
			avgDaysPerEmployee <- round(totalDays/numEmployees)
			avgCallsPerDay <- totalCalls/totalDays
			
			res <- data.frame(city = city_val, employees = numEmployees, totalDays = totalDays, totalCalls = totalCalls, avgCallsPerEmployee = avgCallsPerEmployee, avgDaysPerEmployee = avgDaysPerEmployee, avgCallsPerDay = avgCallsPerDay)
			result <- rbind(result, res)
		}
	}

	return(result)
}

emp <- read.csv("empData.csv", header = TRUE)

op <- callVolume(empData = emp, state = "FL")
print(op)

op <- callVolume(empData = emp, state = "FL")
print(op)

op <- callVolume(empData = emp, state = c("TX", "PA", "GA"))
print(op)
