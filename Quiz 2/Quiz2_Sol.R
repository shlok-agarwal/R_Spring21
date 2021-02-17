library(stringr)
library(reshape2)
library(dplyr)

##Instructions:
  
##1. Load survey_data.csv into R.
##2. Subset data to include only surveys in 2018.
##3. Remove column new_sp from data.
##4. Melt data to a long data set, using "Question" and "Survey" for id variables and removing NA values from data.
##5. Create "Gender" column from the demographic variable (m or f).
##6. Create "Zip" column from the demographic variable (five digit number)
##7. Order the columns: Question, Survey, Gender, Zip, Responses. Sort data by Question, Survey, Gender, Zip.
##8. Load survey_state_lookup.csv into R.  Merge survey_state_lookup with your tidy data set, using zip code as the key field.
##9. Create new summary data frame called s_summary.  s_summary should have one row for each question and county and 3 data columns:  male responses, female responses, and total responses (total across all surveys and genders).

##Step 1
setwd("C:/Users/elizabeth.mohr/Dropbox/GBA 464/Spring A 2021/Assignments/Quiz 2/")
s <- read.csv("survey_data.csv")
names(s)
head(s)
save <- s

##Step 2
# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/substr
unique(s$Survey)
substr(s$Survey,1,4)
s <- s[substr(s$Survey,1,4) == "2018", ]
# Below might also work
# s <- s[str_sub(s$Survey, 1, 4) == "2018", ]

##Step 3
s$new_sp <- NULL


##Step 4
library(reshape2)
sMelt <- melt(s, id = c("Question", "Survey"), na.rm = TRUE, value.name = "Responses")
head(sMelt)


##Step 5
# https://stringr.tidyverse.org/reference/str_sub.html
library(stringr)
sTidy <- sMelt
sTidy$Gender <- str_sub(sMelt$variable, 1, 1)
head(sTidy)

##Step 6
sTidy$Zip <- str_sub(sTidy$variable, 2)
head(sTidy)

##Step 7
library(dplyr)
sTidy <- select(sTidy, Question, Survey, Gender, Zip, Responses)
sTidy <- arrange(sTidy, Question, Survey, Gender, Zip)
sTidy[1:20, ]


##Step 8
s_lookup <- read.csv("survey_state_lookup.csv", header = TRUE)
head(s_lookup)
sMerge <- merge(sTidy, s_lookup, by.x = "Zip", by.y = "Zip")
head(sMerge)
sMerge$Zip <- NULL

#Step 9
s_summary <- dcast(sMerge, Question + County ~ Gender, value.var = "Responses", fun.aggregate = sum)
s_summary$Total <- s_summary$f + s_summary$m
s_summary[1:10, ]


