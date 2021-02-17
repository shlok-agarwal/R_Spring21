MR <- read.csv(file.choose(), header = TRUE)
head(MR)

##Q1

div <- subset(MR, MR$ProjectType == "Web_Design", c("Division"))
div
unique(div)

##Q2

AT <- MR[MR$Division =='Atlanta', ]
AT

AT_100 <- AT[AT$HoursRequired > 100,]
AT_100

mean(AT_100$ClientFeedbackScore)


##Q3

MR$HR <- (MR$InvoiceAmount / MR$HoursRequired)
head(MR)

Mini <- min(MR$HR[MR$ProjectType=='Market_Research'])
Mini
Maxi <- max(MR$HR[MR$ProjectType=='Market_Research'])
Maxi
range(MR$HR[MR$ProjectType=='Market_Research'])

AvgHR <- mean(MR$HR[MR$ProjectType=='Market_Research'])
AvgHR


##Q4

MR$InvoiceNumber[MR$ProjectType == 'Market_Research' & MR$HR == Mini]
MR$InvoiceDate[MR$ProjectType == 'Market_Research' & MR$HR == Mini]


##Q5

Q <- quantile(MR$ClientFeedbackScore, probs = seq(0, 1, 1/5))
QuintTable <- cut(MR$ClientFeedbackScore, quantile(MR$ClientFeedbackScore, probs = seq(0, 1, 1/5)), include.lowest = TRUE)
A <- table(QuintTable, MR$Division)
A
max(A[1, ])


##Q6

percent_CFS <- function(CFS_data, val_low, val_high) {
  num <- length(CFS_data)
  num_in_range <- CFS_data[CFS_data > val_low & CFS_data <= val_high]
  percent <- length(num_in_range)/num
  return(percent)
}

atlanta_CFS = MR[MR$Division == "Atlanta", c("ClientFeedbackScore")]
a1 <- percent_CFS(atlanta_CFS, 11, 72)
a2 <- percent_CFS(atlanta_CFS, 72, 84.2)
a3 <- percent_CFS(atlanta_CFS, 84.2, 91)
a4 <- percent_CFS(atlanta_CFS, 91, 97)
a5 <- percent_CFS(atlanta_CFS, 97, 100)
A <- round(c(a1, a2, a3, a4, a5), digits = 2)
A

denver_CFS = MR[MR$Division == "Denver", c("ClientFeedbackScore")]
d1 <- percent_CFS(denver_CFS, 11, 72)
d2 <- percent_CFS(denver_CFS, 72, 84.2)
d3 <- percent_CFS(denver_CFS, 84.2, 91)
d4 <- percent_CFS(denver_CFS, 91, 97)
d5 <- percent_CFS(denver_CFS, 97, 100)
D <- round(c(d1, d2, d3, d4, d5), digits = 2)
D

mobile_CFS = MR[MR$Division == "Mobile", c("ClientFeedbackScore")]
m1 <- percent_CFS(mobile_CFS, 11, 72)
m2 <- percent_CFS(mobile_CFS, 72, 84.2)
m3 <- percent_CFS(mobile_CFS, 84.2, 91)
m4 <- percent_CFS(mobile_CFS, 91, 97)
m5 <- percent_CFS(mobile_CFS, 97, 100)
M <- round(c(m1, m2, m3, m4, m5), digits = 2)
M

portland_CFS <- MR[MR$Division == "Portland", c("ClientFeedbackScore")]
p1 <- percent_CFS(portland_CFS, 11, 72)
p2 <- percent_CFS(portland_CFS, 72, 84.2)
p3 <- percent_CFS(portland_CFS, 84.2, 91)
p4 <- percent_CFS(portland_CFS, 91, 97)
p5 <- percent_CFS(portland_CFS, 97, 100)
P <- round(c(p1, p2, p3, p4, p5), digits = 2)
P

CFStable <- matrix(c(A, D, M, P), nrow = 4, byrow = TRUE)

rname <- c("Atlanta", "Denver", "Mobile", "Portland")
rownames(CFStable) <- rname
cname <- c("[11,72]", "(72,84.2]", "(84.2,91]", "(91-97]", "(97-100]")
colnames(CFStable) <- cname

CFStable


##Q7
any(is.na(MR))
library(lattice)
bwplot(MR$RepeatClient ~ MR$ClientFeedbackScore, dataset = MR, xlab = " Customer Satisfaction", 
       ylab = "1: New client      2: Repeat client", col = "red")


##Q8
any(is.na(MR))
library(ggplot2)
ggplot(MR, aes(x = HoursRequired, y = ProjectType)) + geom_boxplot(col = "red")

