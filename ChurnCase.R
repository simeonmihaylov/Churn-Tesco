# Import data ----
install.packages("psych")
library(dplyr)
library(psych)
setwd("C:\\Users\\Alex\\Documents\\R\\CHURN_CASE")
rm(list = ls())
dd = read.csv('Data.csv', na.strings = c("NA", "", " ", "  ", "   "))

#summary
ddcle = data.frame(nm= names(dd), cl = sapply(dd,class), nas = colSums(is.na(dd)) )
which(is.na(dd$TotalCharges) == T)

#kolko ot tezi s lipsvashtite nabludeniq sa churn
dd$Churn[which(is.na(dd$TotalCharges)==T)]

# kolko sa churn
table(dd$Churn)

#moje da mahnem lipsvashtite nablu. zashtoto sa malko i nqma da okajat vliqnie

#prekodirame sinior citizen

dd$SeniorCitizen = as.factor(ifelse(dd$SeniorCitizen == 1, "Yes", "No"))

dd$customerID = as.character(dd$customerID)

#check for dublicates

sum(duplicated(dd$customerID))

#premahvane na lipsvashtite stoinosti
dd = na.omit(dd)

#Data understanding ----

#look
windows()
boxplot(tenure ~ Churn, data = dd, col = c("blue", "red"))

windows()
boxplot(TotalCharges ~ Churn, data = dd, col = c("blue", "red"))

windows()
boxplot(MonthlyCharges~Churn, data = dd, col = c("blue", "red"))


windows()
cor.plot(cor(dd[, c(6, 19, 20)]), numbers = T, las = 2)


# check for monthly charges above $80 - how many churned

baba = dd[which(dd$MonthlyCharges > 80),]
sum(sum$Churn == "Yes")


baba2 = dd[which(dd$MonthlyCharges <= 80),]
sum(baba2$Churn == "Yes")


# flag variable
#Gender
table(dd$gender, dd$Churn)
round(prop.table(table(dd$gender,dd$Churn), margin=1), 2)
chisq.test(dd$gender,dd$Churn)

#Payment method
round(prop.table(table(dd$PaymentMethod,dd$Churn), margin=1), 2)
chisq.test(dd$PaymentMethod,dd$Churn)


for (i in 2:18) {
  round(prop.table(table(dd[, i],dd$Churn), margin=1), 2)
  chisquared = chisq.test(dd[, i], dd$Churn)
}

#baseline prob
length(dd$Churn[dd$Churn=="Yes"])/nrow(dd)
#0.265785 bazova proporciq
      

      