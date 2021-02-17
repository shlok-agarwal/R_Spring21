library(stringr)
library(quantmod)
library(lubridate)
library(tidyr)
library(dplyr)
library(reshape2)

##1 Load survey_data.csv into R.
df <- read.csv(file.choose(), header = TRUE)

# CREATE MULTIPLE COPIES OF DATAFRAME
df2 <- df
df3 <- df
df4 <- df
df5 <- df


##2 Subset data to include only surveys in 2018.

subsetdf <- df[df$Survey == '2018']
head(subsetdf)

survey_2018 <- subset(df, df$Survey == "2018")
survey_2018
df_survey2018 <- df[df$Survey]


##3 Remove column new_sp from data.
df$new_sp <- NULL
head(df)

##4 Melt data to a long data set, using "Question" and "Survey" for id variables and removing NA values from data.
dfmelt <- melt(df, id = c("Question", "Survey"), na.rm = TRUE)
head(dfmelt)

##5 Create "Gender" column from the demographic variable (m or f).
##6 Create "Zip" column from the demographic variable (five digit number)
dfmelt$variable <- str_split_fixed(dfmelt$variable, "", 2)
head(dfmelt)



##7 Order the columns: Question, Survey, Gender, Zip, Responses. Sort data by Question, Survey, Gender, Zip.
dfmelt <- arrange(dfmelt,  Question, Survey, variable, value)
head(dfmelt)


##8
surveyLookUp <- read.csv(file.choose(), header = TRUE)
newTable <- merge(dfmelt, surveyLookUp)
head(newTable)


##9
s_summary <- summarize(newTable, f = "variable.1", m = "variable.2")
s_summary
