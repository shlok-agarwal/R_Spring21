## Importing libraries
library(dplyr)
library(stringr)
library(quantmod)
library(lubridate)
library(tidyr)
library(reshape2)
library(ggplot2)

## Importing data
setwd("C:/Users/Dell/Desktop/Programming Analytics/Quiz 3")
airportD <- read.csv("airportData.csv", header = TRUE)
carrierD <- read.csv("carrierData.csv", header = TRUE)
flightD <- read.csv("flightData.csv", header = TRUE)

## Q1. What is the month of the year with the longest mean arrival delay?
mean_arr_del <- as.data.frame(flightD %>% 
                              dplyr::group_by(month) %>%
                              dplyr::summarize(mean = mean(arr_delay, na.rm = TRUE)))
mean_arr_del
mean_arr_ael$month[mean_arr_del$mean == max(mean_arr_del$mean)]


## Q2. 
# Part 1: Merge the flightData data file with the carrierData datafile.
names(flightD)[10] <- "carrier_code"
merged_CF <- merge(carrierD, flightD, by.x = "carrier_code", by.y = "carrier_code")

# Part 2: Find each airline's mean departure delay by month.  In what month did Frontier Airlines have its longest mean departure delay?
FA <- merged_CF[merged_CF$airline == "Frontier Airlines", ]
FA_summary <- as.data.frame(FA %>% 
                              dplyr::group_by(month) %>%
                              dplyr::summarize(mean = mean(dep_delay, na.rm = TRUE)))
FA_summary
FA_summary$month[FA_summary$mean == max(FA_summary$mean)]


## Q3. Complete the table below showing how many airlines had their longest monthly mean departure delay in each month.

# incorrect attempt 
Month_airline <- as.data.frame(merged_CF %>%
                                 dplyr::group_by(month) %>%
                                 dplyr::summarize(airline) %>%
                                 dplyr::summarize(mean = mean(dep_delay, na.rm = TRUE)))
                               

# incorrect attempt
merged_CF$airline[sapply(split(merged_CF$month, merged_CF$dep_delay), mean, na.rm = T)]





## Q4. kpi Gain - Which airlines have an average Gain for the entire time period of the data between 0 and 3 minutes?

# was done in the test, answer incorrect
merged_CF$Gain <- (merged_CF$arr_delay - merged_CF$dep_delay)
gain_summary <- as.data.frame(merged_CF %>% 
                                dplyr::group_by(airline) %>%
                                dplyr::summarize(mean = mean(Gain, na.rm = TRUE)))

gain_summary


# Attempt 2, not getting appropriate result

merged_CF$sched_fly_time <- merged_CF$sched_arr_time - merged_CF$sched_dep_time
merged_CF$act_fly_time <- merged_CF$arr_time - merged_CF$dep_time
merged_CF$Gain <- merged_CF$sched_fly_time - merged_CF$act_fly_time
head(merged_CF, 15)

avgGain_airline1 <- merged_CF %>%
  group_by(month, year, airline) %>%
  summarise(avgGain1 = mean(Gain, na.rm = TRUE))
avgGain_airline1

avgGain_airline <- avgGain_airline1 %>%
  group_by(airline) %>%
  summarise(avgGain = mean(avgGain1, na.rm = TRUE))
avgGain_airline


## Q5. What is the GainRank and GainPerHourRank for Hawaiian Airlines?

# was done in the test, but QUITE WRONG
merged_CF$GainHour <- (merged_CF$Gain/merged_CF$air_time)
gain_summary2 <- as.data.frame(merged_CF %>% 
                                 dplyr::group_by(airline) %>%
                                 dplyr::summarize(mean1 = mean(Gain, na.rm = TRUE),
                                                  mean2 = mean(GainHour, na.rm = TRUE)))

gain_summary2


## Q6.
# Part 1: Merge the flightData datafile on its 'dest' column with the airportData datafile.
names(airportD)[1] <- "dest"
Fl_Air_Merge <- merge(flightD, airportD, by.x = "dest", by.y = "dest")
head(Fl_Air_Merge)

# Part 2: Write a function that accepts two arguments: state and measure.
# State refers to the state in which the destination airport for each flight is located.
# Measure can be one of 5 functions: sum, mean, sd, median, range.
# The function should return the value of departure delay aggregated by the function selected for the state selected.

delay_state <- function(state = c(unique(Fl_Air_Merge$state)), measure)
  {
  
  measure <- as.data.frame(Fl_Air_Merge %>%  dplyr::summarize( 
    sum = sum(Fl_Air_Merge$dep_delay, na.rm = T),
    mean = mean(Fl_Air_Merge$dep_delay, na.rm = T),
    sd = sd(Fl_Air_Merge$dep_delay, na.rm = T),
    median = median(Fl_Air_Merge$dep_delay, na.rm = T),
    range = range(Fl_Air_Merge$dep_delay, na.rm = T))
  )
  result <- data.frame(state = Fl_Air_Merge$state, measure)
  return(result)
}


x <- delay_state(state = "California", mean)
print(x)


# sum = measure$sum || mean = measure$mean || sd = measure$sd || median = measure$median || range = measure$range