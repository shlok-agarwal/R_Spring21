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
callMeans <- function(callTypesArg, id){
    ## Check for valid input
    ## Read all .csv files associated with id numbers included in the id argument
    id = sprintf("%03d", id)
    path <- paste0(paste0("callData/",id), ".csv")
    CD_ID <- read.csv(path, header = TRUE)
    result <- data.frame(callTypes= numeric(0), mean= numeric(0), sd = numeric(0), max = numeric(0), min = numeric(0))
    
    # Sample the call type argument
    if(callTypesArg == "all"){
        callTypesArg = c("totalCalls", "answered", "sales")
    }

    for (val in callTypesArg)
    {

        if(val == "totalCalls"){
            cName <- CD_ID$totalCalls
        } else if(val == "answered"){
            cName <- CD_ID$answered
        } else if(val == "sales") {
            cName <- CD_ID$sales
        } else {
            stop("invalid call type")
        }

        SCD <- as.data.frame(CD_ID %>%  dplyr::summarize( 
                                                        mean = mean(cName, na.rm = TRUE),
                                                        sd = sd(cName, na.rm = TRUE),
                                                        max = max(cName, na.rm = TRUE),
                                                        min = min(cName, na.rm = TRUE))
                                    )
        res <- data.frame(callTypes = val, mean = SCD$mean, sd = SCD$sd, max = SCD$max, min = SCD$min)
        result <- rbind(result, res)
    }

    return(result)
}

# a <- callMeans(callTypesArg = c("totalCalls", "sales"), id = 002)
# a <- callMeans(callTypesArg = "all", id = 002)
a <- callMeans(callTypesArg = "shlok", id = 002) # should say invalid

print(a)
