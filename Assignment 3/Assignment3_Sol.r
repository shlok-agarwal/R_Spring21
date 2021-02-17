
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
statscallData001 <- as.data.frame(callData001 %>%  dplyr::summarize( 
                                                      mean = mean(totalCalls, na.rm = TRUE),
                                                      sd = sd(totalCalls, na.rm = TRUE),
                                                      max = max(totalCalls, na.rm = TRUE),
                                                      min = min(totalCalls, na.rm = TRUE))
                                 )

print(statscallData001)

## Clean data
