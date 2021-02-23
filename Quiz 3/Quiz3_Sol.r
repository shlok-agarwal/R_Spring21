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
merged_CF <- merge(carrierD,flightD,by="carrier_code")

# find entries for frontier airlines
FA <- merged_CF[merged_CF$airline == "Frontier Airlines", ]
FA_summary <- as.data.frame(FA %>% 
                                 dplyr::group_by(month) %>%
                                 dplyr::summarize(mean = mean(dep_delay, na.rm = TRUE)))


# Q4 (NAVEEEN)
All_airline_summary <- as.data.frame(merged_CF %>% 
                                 dplyr::group_by(month) %>%
                                 dplyr::summarize(mean = mean(dep_delay, na.rm = TRUE)))


merged_CF$Gain <- (merged_CF$arr_delay - merged_CF$dep_delay)
gain_summary <- as.data.frame(merged_CF %>% 
                                 dplyr::group_by(airline) %>%
                                 dplyr::summarize(mean = mean(Gain, na.rm = TRUE)))

# Q6
merged_CF$GainHour <- (merged_CF$Gain/merged_CF$air_time)
gain_summary2 <- as.data.frame(merged_CF %>% 
                                 dplyr::group_by(airline) %>%
                                 dplyr::summarize(mean1 = mean(Gain, na.rm = TRUE),
                                 mean2 = mean(GainHour, na.rm = TRUE)))
