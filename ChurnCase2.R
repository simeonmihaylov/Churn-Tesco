
# Import data ----
setwd("C:\\Users\\User\\Documents\\University\\R studio\\Churn solution")
rm(list = ls())
library(psych)
library(dplyr)
dd = read.csv('Data.csv', na.strings = c("NA","", " ", "  ","   "))

#summary
ddcle = data.frame(nm= names(dd), cl = sapply(dd,class), nas = colSums(is.na(dd)) )
which(is.na(dd$TotalCharges)==T)

#kolko ot tezi s lipsvashtite nabludeniq sa churn
dd$Churn[which(is.na(dd$TotalCharges)==T)]

# kolko sa churn
table(dd$Churn)

#moje da mahnem lipsvashtite nablu. za]oto sa malko i nqma da okajat vliqnie

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
boxplot(tenure~Churn, data = dd, col = c("blue", "red"))
windows()
boxplot(TotalCharges~Churn, data = dd, col = c("blue", "red"))
windows()
boxplot(MonthlyCharges~Churn, data = dd, col = c("blue", "red"))

cor(dd$TotalCharges,dd$tenure)
windows()
cor.plot(cor(dd[,c(6,19,20)]), numbers = T, las = 2)
which(colnames(dd)=="tenure")

#pairs(dd$Churn~., data = dd[,c(6,19,20)])

#chek for monthly charges above 80$ - how many churn
a = dd[which(dd$MonthlyCharges>80),]
sum(a$Churn == "Yes") 
sum(a$Churn=="No")
2665/7032

b = dd[which(dd$MonthlyCharges<=80),]
sum(b$Churn == "Yes") 
4367/7032

0.6210182*0.2205175
# flag variable
#Gender
table(dd$gender, dd$Churn)
round(prop.table(table(dd$gender,dd$Churn), margin=1), 2)
chisq.test(dd$gender,dd$Churn)

#Payment method
round(prop.table(table(dd$PaymentMethod,dd$Churn), margin=1), 2)
chisq.test(dd$PaymentMethod,dd$Churn)

#
for (i in 2:18) {
  round(prop.table(table(dd[,i],dd$Churn), margin=1), 2)
  chisq.test(dd[,i],dd$Churn)
}

#baseline prob
length(dd$Churn[dd$Churn=="Yes"])/nrow(dd)
#0.265785 bazova proporciq




nn = dd
#replacing "Yes" "No" with 1,0
nn[,c(3,4,5,7,17)]=ifelse(nn[,c(3,4,5,7,17)] == "Yes",1,0)
#replacing "Yes" "No" "No internet service" with 1,0,3
nn[,c(10:15)] = ifelse(nn[,c(10:15)]=="Yes",1, ifelse(nn[,c(10:15)]=="No",0,3))
#replacing "Yes" "No" "No phone service" with 1,0,3
nn[,8] = ifelse(nn[,8]=="Yes",1, ifelse(nn[,8]=="No",0,3))
#replacing DSL, Fiber optic, No with 10,11,3
nn[,9] = ifelse(nn[,9]=="DSL",10, ifelse(nn[,9]=="Fiber optic",11,3))
#replacing Month-to-month, One year, Two year with 30,12,24
nn[,16] = ifelse(nn[,16]=="Month-to-month",30, ifelse(nn[,16]=="One year",12,24))
#replacing (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic))
# with 111,112,113,114
nn[,18] = ifelse(nn[,18]=="Electronic check",111,
                 ifelse(nn[,18]=="Mailed check",112,
                        ifelse(nn[,18]=="Bank transfer (automatic)",113,114)))

nn$Churn=ifelse(nn$Churn=="Yes",1,0)

#Lasso
ss = nn[,-c(1,2)]
ss$MonthlyCharges = scale(nn$MonthlyCharges)
ss$tenure = scale(nn$tenure)
ss$TotalCharges = scale(nn$TotalCharges)
library(glmnet)
x=model.matrix(Churn~.,ss)
x=x[,-1] # mahame kolona edno zashtoto lm/glm avtomatichno dobavqt stojnosti 1 za beta o
x=scale(x,center=T,scale=F) #standartizirame dannite
y=nn$Churn

# Split data into training and test set
set.seed(1)
lambda_seq <- 10^seq(2, -2, by = -.1)
train=sample(nrow(ss),round(nrow(ss)*3/4))
#Determin optimal lambda
cv=cv.glmnet(x[train,],y[train],family = "binomial",lambda = lambda_seq, alpha=1, standartize=F)
windows()
plot(cv)
cv$lambda.min
eq=glmnet(x[train,],y[train],family = "binomial",alpha = 1, 
          lambda = cv$lambda.min,standardize = F) 
eq$beta


#classification tree
library(tree)
train=sample(nrow(dd),round(nrow(dd)*3/4))
eq=tree(Churn~ TotalCharges,data=dd, subset=train)
windows()
plot(eq)
text(eq)
eq

eq2=tree(Churn~ tenure,data=dd, subset=train)
windows()
plot(eq2)
text(eq2)
eq2

eq3=tree(Churn~ MonthlyCharges,data=dd, subset=train)
windows()
plot(eq3)
text(eq3)
eq3

#cluster
library(dplyr)
library(klaR)

ss=nn[,-c(6,19,20)]
clus = kmodes(ss, modes = 10, iter.max = 10, fast = TRUE)
clus$size
clus$modes
View(clus$modes)

#PCA
x=scale(dd[,c(6,19,20)])
pcd=prcomp(x) #tazi funkciq izpylnqva analiza na glavnite komponenti
names(pcd) # v structurata "x" se namira matricata z ot prezentaciqta
pcd$sdev # standartno otklonenie na dvata komponenta
pcd$sdev^2
pcd$sdev^2/sum(pcd$sdev^2) # variance explained s tova vijdame vsqka ot promenlivite kakva obqsnitelna sila ima
pcd$rotation 