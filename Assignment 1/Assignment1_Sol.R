mr <-  read.csv("C:/Users/elizabeth.mohr/Dropbox/GBA 464/Spring A 2021/Assignments/HW 1/MR_data.csv",header=TRUE)
head(mr)
class(mr$InvoiceDate)

##q1 which divisions offered web design services in 2018 for their clients?
table(mr$Division, mr$ProjectType)

##q2 what is the average Client Feedback Score in the Atlanta division for projects which required more than 100 hours?
mean(mr[mr$HoursRequired > 100 & mr$Division == "Atlanta", "ClientFeedbackScore"])

##q3 what percent of Portland's 2018 projects were with repeat clients?
mean(mr[mr$Division == "Portland", "RepeatClient"])

##q3 what is the average hourly rate across all divisions for market research projects? What is the range (max and min) of hourly rates for market research projects?
mr$HourlyRate <- mr$InvoiceAmount / mr$HoursRequired
mean(mr[mr$ProjectType == "Market_Research", "HourlyRate"])
min(mr[mr$ProjectType == "Market_Research", "HourlyRate"])
max(mr[mr$ProjectType == "Market_Research", "HourlyRate"])

##q4 what was the invoice number and invoice date for the market research project which had the lowest hourly rate?
min(mr[mr$ProjectType == "Market_Research", "HourlyRate"])
mr[mr$HourlyRate == min(mr[mr$ProjectType == "Market_Research", "HourlyRate"]), c("InvoiceNumber", "InvoiceDate","HourlyRate")]


##q5 An analyst at corporate headquarters took 2018 projects across all divisions
## and divided them into quintiles (bottom 20%, ..., top 20%) by Client Feedback Score.

##q5 Which division had more projects in the bottom 20% by Client Feedback Score than any other division?
mr$ClientFeedbackQuintile <- cut(mr$ClientFeedbackScore, quantile(mr$ClientFeedbackScore, seq(0,1,.2)), include.lowest = TRUE)
table(mr$Division, mr$ClientFeedbackQuintile)

##q6 THe analyst created the table below, showing the distribution of projects' Client Feedback Score by Division
## (for example, 25% of Portland's projects had scores between 94 and 100, which was the top 20% of scores across all divisions)
##SUbmit the R code to recreate this table.
t <- table(mr$Division, mr$ClientFeedbackQuintile)
round(t / rowSums(t),2)

rowSums(t)
sapply(split(mr$Division, mr$Division), length)
as.vector(table(mr$Division))



##q7
library(lattice)
bwplot(~ClientFeedbackScore | RepeatClient, data = mr)
mean(mr[mr$RepeatClient == 1, "ClientFeedbackScore"])
mean(mr[mr$RepeatClient == 0, "ClientFeedbackScore"])

histogram(~HoursRequired | ProjectType, data = mr)





