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
callMeans <- function(callTypes, id){
    ## Check for valid input
    ## Read all .csv files associated with id numbers included in the id argument
    id = sprintf("%03d", id)
    path <- paste0(paste0("callData/",id), ".csv")
    CD_ID <- read.csv(path, header = TRUE)
    SCD001 <- as.data.frame(CD_ID %>%  dplyr::summarize( 
                                                      mean = mean(totalCalls, na.rm = TRUE),
                                                      sd = sd(totalCalls, na.rm = TRUE),
                                                      max = max(totalCalls, na.rm = TRUE),
                                                      min = min(totalCalls, na.rm = TRUE))
                                 )

    result <- data.frame(callTypes = 'totalCalls', mean = SCD001$mean, sd = SCD001$sd, max = SCD001$max, min = SCD001$min)
    return(result)
}

a <- callMeans(callTypes = "totalCalls", id = 002)
print(a)