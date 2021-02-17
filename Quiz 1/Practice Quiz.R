##Practice

MR_data <- read.csv(file.choose(), header = TRUE)
head(MR_data)
division <- subset(MR_data,MR_data$ProjectType=="Web_Design",c("Division"))
division
unique(division)


a <- subset(MR_data, MR_data$HoursRequired > 100, c("Division"))
a
b <- order(MR_data$Division == "Atlanta")
b
MR_data[b,]



##PRACTICE QUIZ


##Q2

USArrests <- read.csv(file.choose(), header = TRUE)
USArrests
a <- subset(USArrests, USArrests$Assault >= 300)
a


##Q3

USArrests
b <- head(USArrests[order(USArrests$Murder, decreasing = TRUE), c("Murder", "Assault", "UrbanPop", "Rape")])
b


##Q4

USArrests
difference <- abs(USArrests$Murder - USArrests$Rape)
head(difference)
USArrests[difference == max(difference),]


##Q5
 
table(cut(USArrests$Murder, quantile(USArrests$Murder), include.lowest = TRUE), cut(USArrests$UrbanPop, quantile(USArrests$UrbanPop), include.lowest = TRUE))

USArrests[which(cut(USArrests$Murder, quantile(USArrests$Murder), include.lowest = TRUE) == levels(cut(USArrests$Murder, quantile(USArrests$Murder), include.lowest = TRUE))[1]
         &
           cut(USArrests$UrbanPop, quantile(USArrests$UrbanPop), include.lowest = TRUE) == levels(cut(USArrests$UrbanPop, quantile(USArrests$UrbanPop), include.lowest = TRUE))[4]), ]



##Q6

USArrests
