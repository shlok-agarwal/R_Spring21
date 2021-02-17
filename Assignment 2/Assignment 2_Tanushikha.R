library(dplyr)
library(stringr)
library(quantmod)
library(lubridate)
library(tidyr)
library(reshape2)
library(ggplot2)


zillow <- read.csv(file.choose(), header = T)
regions <- read.csv(file.choose(), header = T)

zillow_copy <- zillow
regions_copy <- regions


## 1. Clean data, transform into 'tidy' data set

zMelt <- melt(zillow, id = c("RegionID"), value.name = "MedHouse", na.rm = T)
head(zMelt)
zMelt$variable <- gsub("X","",zMelt$variable)
zTidy <- zMelt
zMelt_copy <- zMelt
class(zTidy$variable)
class(zMelt$variable)

zTidy$Year <- str_sub(zMelt$variable, 1, 4)
zTidy$Month <- str_sub(zTidy$variable, 6)
zTidy$variable <- NULL
head(zTidy)

zTidy <- select(zTidy, RegionID, Year, Month, MedHouse)
head(zTidy)

head(regions)

zrMerge <- merge(regions, zTidy, by.x = "RegionID", by.y = "RegionID", all = T)
head(zrMerge)

zrMerge <- arrange(zrMerge, Year, Month, SizeRank)
head(zrMerge, 15)
class(zrMerge)


## 2. Tabulate data - creating summary table

zrMerge_save <- zrMerge

zrMerge_County1 <- zrMerge %>%
  group_by(State, CountyName, Year, Month) %>%
  summarize(MedHouse = median(MedHouse, na.rm = T))

zrMerge_County <- zrMerge_County1 %>%  
  group_by(State,CountyName) %>%
  summarize(medCounty = median(MedHouse, na.rm = T), sdCounty = sd(MedHouse, na.rm = T), maxCounty = max(MedHouse, na.rm = T), minCounty = min(MedHouse, na.rm = T))

zrMerge_County$RanksdCounty <- rank(desc(zrMerge_County$sdCounty), na.last = T, ties.method = "max")

head(zrMerge_County)

zrMerge_State1 <- zrMerge %>%
  group_by(State, Year, Month) %>%
  summarize(MedHouse = median(MedHouse, na.rm = T)) %>%
  
  zrMerge_State <- zrMerge_State1 %>%  
  group_by(State) %>%
  summarize(medState = median(MedHouse, na.rm = T), sdState = sd(MedHouse, na.rm = T), maxState = max(MedHouse, na.rm = T), minState = min(MedHouse, na.rm = T))

zrMerge_State$RanksdState <- rank(desc(zrMerge_State$sdState), na.last = T, ties.method = "max")

head(zrMerge_State)

# Summary table sorted by State, County

csMerge <- merge(zrMerge_County, zrMerge_State, by.x = "State", by.y = "State", all.x = T)
head(csMerge)

# Data formatting
temp <- paste0(csMerge$CountyName, ".", csMerge$State) 
csMerge$CountyName <- NULL
csMerge$State <- temp
names(csMerge)[1] <- "County.State"

csMerge_copy <- csMerge
head(csMerge_copy, 10)

csMerge <- select(csMerge, County.State, medCounty, sdCounty, maxCounty, minCounty, RanksdCounty, medState, sdState, maxState, minState, RanksdState)

csMerge_1 <- csMerge
csMerge_2 <- csMerge

# Summary table sorted by County Rank (RanksdCounty)

csMerge_RanksdCounty <- csMerge[order(csMerge$RanksdCounty),]
head(csMerge_RanksdCounty, 10)

# Summary table sorted by State Rank, then County Rank (RanksdState)

csMerge_RanksdState <- csMerge[order(csMerge$RanksdState, csMerge$RanksdCounty),]
head(csMerge_RanksdState, 10)


# Counties that are among (top) 25% most volatile counties but located in one of the (bottom) 25% least volatile states

x <- csMerge

quantile(x$RanksdCounty)
quantile(x$RanksdState)

table(cut(x$RanksdCounty, quantile(x$RanksdCounty), include.lowest = T), cut(x$RanksdState, quantile(x$RanksdState), include.lowest = T))

county_quant <- subset(x, x$RanksdCounty <= 455 & x$RanksdState >= 38, c("County.State","sdCounty","RanksdCounty","sdState","RanksdState"))
county_quant

# States that are represented on list of top 50 most volatile counties

state_50 <- subset(csMerge_copy, csMerge_copy$RanksdCounty <= 50, c("State"))
unique(state_50)


## 3. Data Visualization

zrMerge_1 <- zrMerge
head(zrMerge_1)

subdata <- zrMerge_1[zrMerge_1$SizeRank <= 10, ]
subdata$Date <- paste0(subdata$Year, ".", subdata$Month)
head(subdata, 20)

any(is.na(subdata))
g1 <- ggplot(subdata, aes(x = Date, y = MedHouse, group = RegionName, color = RegionName)) + geom_line()
g2 <- g1 + labs(x = "Date(Year.Month)", y = "Median House Value per sq ft", title = "Median Value per sq ft for Top 10 Regions: 1996-2019")
g2 + xlim(subdata$Date)

subdata1 <- subdata[,c(2,7,9)]
s_melt = melt(subdata1, id = c("RegionName", "Year"))
head(s_melt, 10)
