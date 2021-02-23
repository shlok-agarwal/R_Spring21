## Import libraries
library(dplyr)
library(stringr)
library(quantmod)
library(lubridate)
library(tidyr)
library(reshape2)
library(ggplot2)

# ## Import data
setwd("C:/R_ws/R_Spring21/Quiz 3")
airportD <- read.csv("airportData.csv", header = TRUE)
carrierD <- read.csv("carrierData.csv", header = TRUE)
flightD <- read.csv("flightData.csv", header = TRUE)

# merge two data frames by ID
# Q2
names(flightD)[10] <- "carrier_code"
CF <- merge(carrierD,flightD,by="carrier_code")

# find entries for frontier airlines
FA <- CF[CF$airline == "Frontier Airlines", ]
FA_summary <- as.data.frame(FA %>% 
                                 dplyr::group_by(month) %>%
                                 dplyr::summarize(mean = mean(dep_delay, na.rm = TRUE)))


# Q3
# Complete the table below showing how many airlines had their longest monthly mean
# departure delay in each month.

# initialize empty array. One entry for each month
monthly_freq_calculator = numeric(12)

for(i in unique(CF$carrier_code)){
  # filter entry based on carrier code. "carrier" would have data specific only to the particular carrier  
  carrier = filter(CF,CF$carrier_code==i)
  # group the carrier data by month and get the mean of "dep_delay" in the other column
  monthly_carrier_data = carrier %>% group_by(month) %>% summarize(mean = mean(dep_delay, na.rm = T))
  # get the index of month with maximum delay, [,2] used to get the second column
  max_monthly_index=which(monthly_carrier_data[,2]== max(monthly_carrier_data[,2]))
  # add to the frequency
  monthly_freq_calculator[max_monthly_index] = monthly_freq_calculator[max_monthly_index] + 1
}

# Output arranged from Jan - Dec
# 2 0 0 0 1 6 5 0 0 0 0 2
print(monthly_freq_calculator)

# Q4
# A kpi for flight performance is Gain, defined as the amount of time a flight made up (or lost) in
# the air.
# For example: a flight scheduled to depart at 6:08pm that actually departed at 6:32pm was
# scheduled to arrive at 7:28pm and actually arrived at 7:40pm. This flight “gained” 12 minutes
# in the air. In other words, its arrival time was 12 minutes earlier than would have been
# predicted by its actual departure time. A flight that left on time and arrived late would have a
# negative gain.
# My Q - Which airlines have an average Gain for the entire time period of the data between 0 and 3
# minutes?

# gain is departure time minus the arrival time. It is the time "gained" in the air. 
# Eg- If flight departs early and arrives late, the gain would be negative
CF$gain=CF$dep_delay-CF$arr_delay
# identify average gain grouped by the airline carrier
avg_gain = CF %>% group_by(airline) %>% summarize(mean = mean(gain, na.rm = T))
# filter mean betwen 0 and 3 as asked in the question
avg_gain[which(avg_gain$mean>=0 && avg_gain$mean<=3),]


# Q5
# Gain/hour is also measured, defined as the flight’s Gain per hour of air time.
# Let GainRank be the ordinal ranking for the carriers by Gain (1 = carrier with highest value for
# Gain) and GainPerHourRank be the ordinal ranking for the carriers by Gain/Hour (1 = carrier
# with the highest value for Gain/Hour)
# My Q - What is the GainRank and GainPerHourRank for Hawaiian Airlines?

# calculate the hours of airtime
a=format(strptime(sprintf('%04d', CF$dep_time), format='%H%M'), '%H:%M')
b=format(strptime(sprintf('%04d', CF$arr_time), format='%H%M'), '%H:%M')
# change 
r1 <- strptime(a, format = "%H:%M")
r2 <- strptime(b, format = "%H:%M")
c=difftime(r2,r1,units = "hours")

# hours from above calculation. We only want the numeric part of it
# CF$hrs <- as.numeric(difftime(r2,r1,units = "hours"))
CF$hrs <- CF$air_time/60
CF$gain=CF$dep_delay-CF$arr_delay
# gain per hour is gain divided by number of hours
CF$gain_hr <- (CF$gain/CF$hrs)

# group by carrier airlines and summarize by the sum of gain and gain per hour
val = CF %>% group_by(airline) %>% summarize(mean_gain = mean(gain, na.rm = T),
                                            mean_hr = mean(gain_hr, na.rm = T))
                                    
# to sort by gain per hour rank
val1 <- arrange(val, desc(mean_hr)) %>%
          mutate(rank_gain_hr = 1:nrow(val))

# to sort by gain rank
val2 <- arrange(val, desc(mean_gain)) %>%
          mutate(rank_gain = 1:nrow(val))


## Q6
# rename the faa column and merge both data frames
names(airportD)[1] <- "dest"
FA <- merge(flightD,airportD,by="dest")

# Out here FUN can be substituted for mean, median, etc
delay_state <- function(stt, FUN) {
  # filter data based on state
  ar=filter(FA,FA$state==stt)
  # return the mean, std, etc from the filterd data
  return (FUN(ar$dep_delay, na.rm = T))
}

# examples
# delay_state("California", mean)
# delay_state("Florida", range)
