EXP <- read.csv(file.choose(), header = TRUE)
head(EXP)

##Q1
All_Hotel_exp <- EXP[EXP$account_description == "hotel", c("account_description", "gross_amount")]
All_Hotel_exp

Avg_Hotel_exp <- mean(All_Hotel_exp$gross_amount)
Avg_Hotel_exp

##Q4
stay <- EXP[EXP$account_description == "hotel", ]
stay[stay$gross_amount > 200, ]
length(stay$gross_amount)

##Q5
Add <- EXP[EXP$trip == 2549, ]
sum(Add$gross_amount)

##Q6
rental_data <- EXP[EXP$account_description == "rental car", c("last_name", "gross_amount")]
rental_sum_data = aggregate(gross_amount~last_name,data=rental_data,FUN=sum)
rental_sum_data[ order(rental_sum_data$gross_amount), ]
