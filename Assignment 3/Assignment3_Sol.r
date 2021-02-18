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

    ## Read all .csv files associated with id numbers included in the id argument

    # Initialize empty data frame
    CD <- data.frame(dates= as.Date(character()), id= integer(0), totalCalls = integer(0), answeredCalls = integer(0), sales = integer(0))
    for (val in id)
    {
        val_id = sprintf("%03d", val)
        path <- paste0(paste0("callData/",val_id), ".csv")
        CD_ID <- read.csv(path, header = TRUE)
        CD <- rbind(CD, CD_ID)
    }

    result <- data.frame(callTypes= numeric(0), mean= numeric(0), sd = numeric(0), max = numeric(0), min = numeric(0))
    
    # Sample the call type argument
    if(length(callTypesArg) == 1)
    {
        if(callTypesArg == "all")
        {
            callTypesArg = c("totalCalls", "answeredCalls", "sales")
        }
    }

    for (val in callTypesArg)
    {

        if(val == "totalCalls" || val == c("totalCalls")){
            cName <- CD$totalCalls
        } else if(val == "answeredCalls" || val == c("answeredCalls")){
            cName <- CD$answeredCalls
        } else if(val == "sales" || val == c("sales")) {
            cName <- CD$sales
        } else {
            stop("invalid call type")
        }

        SCD <- as.data.frame(CD %>%  dplyr::summarize( 
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

op <- callMeans(callTypesArg = "all", id = 1:5)
print(op)
op <- callMeans(callTypesArg = c("totalCalls", "sales"), id = 100:125)
print(op)
op <- callMeans(callTypesArg = "Calls", id = 1:175)
print(op)
op <- callMeans(callTypesArg = c("answeredCalls"), id = c(2,6,17,29,100))
print(op)
