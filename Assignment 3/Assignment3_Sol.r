
## Import libraries
library(dplyr)
library(stringr)
library(quantmod)
library(lubridate)
library(tidyr)
library(reshape2)
library(ggplot2)

## Import data
callData001 <- read.csv(file.choose(), header = T)
SCD001 <- as.data.frame(callData001 %>%  dplyr::summarize( 
                                                      mean = mean(totalCalls, na.rm = TRUE),
                                                      sd = sd(totalCalls, na.rm = TRUE),
                                                      max = max(totalCalls, na.rm = TRUE),
                                                      min = min(totalCalls, na.rm = TRUE))
                                 )

result <- data.frame(callTypes = 'totalCalls', mean = SCD001$mean, sd = SCD001$sd, max = SCD001$max, min = SCD001$min)

print(statscallData001)

## Clean data
