##HW2
library(dplyr)
library(reshape2)

setwd("C:/Users/elizabeth.mohr/Dropbox/GBA 464/Spring A 2021/Assignments/HW 2")
zillow <- read.csv("zillow.csv", header = TRUE)
regions <- read.csv("regions.csv", header = TRUE)

##Part 1: Data Cleaning

z <- merge(zillow, regions, by.x = "RegionID", by.y = "RegionID")

zm <- melt(z, id = c("RegionID", "RegionName", "State", "Metro", "CountyName", "SizeRank"), variable.name = "Year.Month", value.name = "MedHouse")
head(zm)
zm$Year.Month <- substr(zm$Year.Month, 2, 9)
zm$Year <- substr(zm$Year.Month, 1, 4)
zm$Month <- substr(zm$Year.Month, 6, 9)
zm$Year.Month <- as.factor(zm$Year.Month)
zm <- zm[order(zm$Year, zm$Month, zm$SizeRank), ]
zm$County.State <- paste0(zm$CountyName, '.', zm$State)
(zm[1:15, c(1,2,3,4,5,6,9,10,8)])

## Part 2: Tabulation
## create summary table by county,state
statsCounty <- as.data.frame(zm %>% dplyr::group_by(County.State, Year.Month) %>%
                                     dplyr::summarize(medHouseCounty.Year.Month = median(MedHouse, na.rm = TRUE)) %>%
                                     dplyr::group_by(County.State) %>%
                                     dplyr::summarize(sdCounty = sd(medHouseCounty.Year.Month, na.rm = TRUE), 
                                                      medCounty = median(medHouseCounty.Year.Month, na.rm = TRUE),
                                                      maxCounty = max(medHouseCounty.Year.Month, na.rm = TRUE),
                                                      minCounty = min(medHouseCounty.Year.Month, na.rm = TRUE))
                                 )

##add ranking and ranking group
statsCounty$State <- substr(statsCounty$County.State, nchar(statsCounty$County.State) - 1, nchar(statsCounty$County.State))
statsCounty <- arrange(statsCounty, desc(sdCounty))
statsCounty$RanksdCounty <- 1:nrow(statsCounty)
statsCounty$RankGroupCounty <- cut(statsCounty$RanksdCounty, quantile(statsCounty$RanksdCounty), labels = c(1,2,3,4), include.lowest = TRUE)

##create summary table by state
statsState <- as.data.frame(zm %>% dplyr::group_by(State, Year.Month) %>%
                                 dplyr::summarize(medHouseState.Year.Month = median(MedHouse, na.rm = TRUE)) %>%
                                 dplyr::group_by(State) %>%
                                 dplyr::summarize(sdState = sd(medHouseState.Year.Month, na.rm = TRUE), 
                                                  medState = median(medHouseState.Year.Month, na.rm = TRUE),
                                                  maxState = max(medHouseState.Year.Month, na.rm = TRUE),
                                                  minState = min(medHouseState.Year.Month, na.rm = TRUE)))

##add ranking and ranking group
statsState <- arrange(statsState, desc(sdState))
statsState$RanksdState <- 1:nrow(statsState)
statsState$RankGroupState <- cut(statsState$RanksdState, quantile(statsState$RanksdState), labels = c(1,2,3,4), include.lowest = TRUE)
head(statsState)
tail(statsState)

## combine county and state stats
stats <- merge(statsCounty, statsState, by.x = "State", by.y = "State")
stats[order(stats$State, stats$County), ][1:10, c("County.State", "medCounty", "sdCounty", "maxCounty", "minCounty", "RanksdCounty", "medState", "sdState", "maxState", "minState", "RanksdState")]
stats[order(stats$RanksdCounty, stats$RanksdState), ][1:10, c("County.State", "medCounty", "sdCounty", "maxCounty", "minCounty", "RanksdCounty", "medState", "sdState", "maxState", "minState", "RanksdState")]
stats[order(stats$RanksdState, stats$RanksdCounty), ][1:10,c("County.State", "medCounty", "sdCounty", "maxCounty", "minCounty", "RanksdCounty", "medState", "sdState", "maxState", "minState", "RanksdState") ]


## question 1
stats[stats$RankGroupCounty == 1 & stats$RankGroupState == 4,c("County.State","sdCounty", "RanksdCounty", "sdState", "RanksdState") ]

##question 2
stats <- stats[order(stats$RanksdCounty), ]
unique(stats[1:50,"State"])

## Part 3: Visualization
regions <- zm %>% dplyr::group_by(RegionID) %>%
                  dplyr::summarize(median = median(MedHouse, na.rm = TRUE), max = max(MedHouse, na.rm = TRUE))
regions <- as.data.frame(regions)
regions <- merge(regions, zm, by.x = c("RegionID", "max"), by.y = c("RegionID", "MedHouse"))
regions <- arrange(regions, RegionID, Year.Month)
regions <- as.data.frame(regions %>% dplyr::group_by(RegionID) %>% slice(which.max(Year.Month)))

       
regions <- (dplyr::rename(regions, Peak.Date = Year.Month, Peak.Year = Year, Peak.Month = Month))
regions$Recover <- regions$Peak.Year >= 2018
regions[regions$Recover == TRUE, "Recover"] <- "Recovery" 
regions[regions$Recover == FALSE, "Recover"] <- "No Recovery"

library(ggplot2)
zn <- zm[zm$SizeRank <= 10, ]
topRegions <- regions[regions$SizeRank <=10, ]
zn <- merge(zn, topRegions)
zn <- arrange(zn, SizeRank, Year.Month)
zn$Recover <- zn$Peak.Year >= 2018
zn[zn$Recover == TRUE, "Recover"] <- "Recovery" 
zn[zn$Recover == FALSE, "Recover"] <- "No Recovery"

topRegions$Peak.Label <- topRegions$Peak.Date
topRegions[topRegions$Recover == "Recovery", "Peak.Label"] <- ""

ggplot(data = zn, aes(x = Year.Month, y = MedHouse, group = RegionName)) +
        geom_line(aes(color = RegionName)) + 
        scale_x_discrete(breaks = c("1996.04", "2001.01", "2006.01",
                                    "2011.01", "2016.01","2019.07")) +
        facet_wrap(~Recover) +
        geom_point(data = topRegions, aes(x = Peak.Date, y = max, color = RegionName)) +
        geom_text(data = topRegions, aes(x = Peak.Date, y = max, label = Peak.Label, color = RegionName), vjust = -1, size = 4) +
        geom_text(data = topRegions, aes(x = "2018.06", y = max, label = RegionName, color = RegionName), vjust = 1, hjust = 1, size = 4) +
        theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1), 
              panel.spacing = unit(2, "lines"), panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(color = "light grey")) +
        labs(x = "Date(Year.Month", y = "Median House Value per sq. ft.", title = "Median Value per sq ft for Top 10 Regions: 1996 - 2019")
    


