#Loading packages

library(dplyr)
library(DescTools)
library(caret)
library(Hmisc)
library(imputeMissings)
library(purrr)
library(factoextra)
library(naivebayes)
library(rpart.plot)
library(randomForest)
library(fviz_cluster)
library(moments)

#_______________________________________________________________________________
#DATA CLEANING

#1. Storing dataset in new variable 'MyData'
str(LoanDefaulters)
MyData<-LoanDefaulters

#2. Converting categorical variables into factors
MyData$Gender<-as.factor(MyData$Gender)
MyData$approv_in_adv<-as.factor(MyData$approv_in_adv)
MyData$loan_type<-as.factor(MyData$loan_type)
MyData$loan_purpose<-as.factor(MyData$loan_purpose)
MyData$Credit_Worthiness<-as.factor(MyData$Credit_Worthiness)
MyData$open_credit<-as.factor(MyData$open_credit)
MyData$business_or_commercial<-as.factor(MyData$business_or_commercial)
MyData$credit_type<-as.factor(MyData$credit_type)
MyData$age<-as.factor(MyData$age)
MyData$Region<-as.factor(MyData$Region)
MyData$Security_Type<-as.factor(MyData$Security_Type)
MyData$Status<-as.factor(MyData$Status)

str(MyData)

#3. Removing 'ID' and 'Security Type' from dataset
MyData<-MyData[-c(1,18)]

#4. Convert tibble into dataframe
MyData<-as.data.frame(MyData)

#5. Removing duplicates
MyData<-MyData[!duplicated(MyData),]
MyData

#6. Checking for missing values
map(MyData,~sum(is.na(.)))
MyData<-impute(MyData,method="median/mode")
map(MyData,~sum(is.na(.)))

#7. Checking skewness-kurtosis values
sapply(MyData[c(8,9,10,11,13,15)],skewness)
sapply(MyData[c(8,9,10,11,13,15)],kurtosis)

#8. Checking boxplots for outliers

outL<-boxplot(MyData$loan_amount)$out
length(outL)
outP<-boxplot(MyData$property_value)$out
length(outP)
outI<-boxplot(MyData$income)$out
length(outI)

#_______________________________________________________________________________
#Treating Outliers:

#9. Using Variable Transformation
#a) Using square root
MyData3<-MyData
MyData3$loan_amount<sqrt(MyData3$loan_amount)
MyData3$property_value<-sqrt(MyData3$property_value)
MyData3$income<-sqrt(MyData3$income)
sapply(MyData3[c(8,10,11)],skewness)
sapply(MyData3[c(8,10,11)],kurtosis)

#b) Using log and inverse
MyData4<-MyData
MyData5<-MyData
MyData4$income<-log(MyData4$income)
MyData5$income<-1/(MyData5$income)
sapply(MyData4[c(8,10,11)],skewness)
sapply(MyData4[c(8,10,11)],kurtosis)
sapply(MyData5[c(8,10,11)],skewness)
sapply(MyData5[c(8,10,11)],kurtosis)
#log and inverse of 0 is not defined

#10. By imputing the outliers
MyData2<-MyData

#a) Converting outliers into missing values
MyData2[MyData2$loan_amount %in% outL, "loan_amount"] = NA
MyData2[MyData2$property_value %in% outP, "property_value"] = NA
MyData2[MyData2$income %in% outI, "income"] = NA
map(MyData2, ~sum(is.na(.)))

#b) Replacing missing values with central tendency measure median
MyData2<-impute(MyData2,method="median/mode")
map(MyData2, ~sum(is.na(.)))

#11. Finally checking skewness-kurtosis after imputing:
sapply(MyData2[c(8,10,11)],skewness)
sapply(MyData2[c(8,10,11)],kurtosis)

#_______________________________________________________________________________
#MODEL BUILDING:
#A. Creating Models with imputed outliers

#Data Partition:
set.seed(100)
TrainLoan<-createDataPartition(MyData2$Status, p=0.8, list=FALSE)
Testing1<-MyData2[-TrainLoan ,]
Training1<-MyData2[TrainLoan ,]
str(Training1)

#Models 1-5 after imputing
Model1<-train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, data=Training1, method="glm",family="binomial")
summary(Model1)
ModelS1<-train(Status~Gender+approv_in_adv+loan_type+loan_purpose+open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+age+LTV, data=Training1, method="glm",family="binomial")
summary(ModelS1)
Model2<-train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, data=Training1, method="naive_bayes")
Model3<-train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, data=Training1, method="rpart")
Model4<-train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, data=Training1, method="rpart", parms = list(split = "information"))
Model5<-train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, data=Training1, method="rf")
summary(Model5)

#Plotting Decision Trees
rpart.plot(Model3$finalModel)
rpart.plot(Model4$finalModel)

#Predicting Values
Pred1<-predict(Model1,newdata = Testing1)
PredS1<- predict(ModelS1,newdata=Testing1)
Pred2<-predict(Model2,newdata = Testing1)
Pred3<-predict(Model3,newdata = Testing1)
Pred4<-predict(Model4,newdata = Testing1)
Pred5<-predict(Model5,newdata = Testing1)


#checking the predictions
confusionMatrix(Pred1, Testing1$Status,positive="1")
confusionMatrix(PredS1, Testing1$Status,positive="1")
confusionMatrix(Pred2, Testing1$Status,positive="1")
confusionMatrix(Pred3, Testing1$Status,positive="1")
confusionMatrix(Pred4, Testing1$Status,positive="1")
confusionMatrix(Pred5, Testing1$Status,positive="1")

#_______________________________________________________________________________
#B. Creating Models without treating outliers

#Data Partition:
set.seed(100)
TrainLoan2<-createDataPartition(MyData$Status, p=0.8, list=FALSE)
Testing3<-MyData[-TrainLoan2 ,]
Training3<-MyData[TrainLoan2 ,]
str(Training3)

#Models 6-10 without treating outliers
ModelW1<-train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, data=Training3, method="glm",family="binomial")
ModelSW1<-train(Status~Gender+approv_in_adv+loan_type+loan_purpose+open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+age+LTV, data=Training3, method="glm",family="binomial")
ModelW2<-train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, data=Training3, method="naive_bayes")
ModelW3<-train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, data=Training3, method="rpart")
ModelW4<-train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, data=Training3, method="rpart",parms=list(split="information"))
ModelW5<-train(Status~Gender+approv_in_adv+loan_type+loan_purpose+Credit_Worthiness+open_credit+loan_amount+rate_of_interest+property_value+income+credit_type+Credit_Score+age+LTV+Region, data=Training3, method="rf")

#Plotting Decision Trees
rpart.plot(ModelW3$finalModel)
rpart.plot(ModelW4$finalModel)

#Predicting Values
PredW1<-predict(ModelW1,newdata = Testing3)
PredSW1<-predict(ModelSW1,newdata = Testing3)
PredW2<-predict(ModelW2,newdata = Testing3)
PredW3<-predict(ModelW3,newdata = Testing3)
PredW4<-predict(ModelW4,newdata = Testing3)
PredW5<-predict(ModelW5,newdata = Testing3)

#checking the predictions
confusionMatrix(PredW1, Testing3$Status,positive="1")
confusionMatrix(PredSW1, Testing3$Status,positive="1")
confusionMatrix(PredW2, Testing3$Status,positive="1")
confusionMatrix(PredW3, Testing3$Status,positive="1")
confusionMatrix(PredW4, Testing3$Status,positive="1")
confusionMatrix(PredW5, Testing3$Status,positive="1")
#_______________________________________________________________________________

