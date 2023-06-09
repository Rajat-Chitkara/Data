attach(Valuation)
#Q-1
MyData<-Valuation
duplicated(MyData)
MyData[duplicated(MyData),] 
MyData<-MyData[!duplicated(MyData),]
MyData
MyData<-as.data.frame(MyData)
MyData
map(MyData, ~sum(is.na(.)))
MyData<-impute(MyData, method="median/mode")
map(MyData, ~sum(is.na(.)))
MyData<-MyData[-1]
skewness(MyData$Numberofcompetitorstores)
skewness(MyData$AverageHouseholdIncome)
kurtosis(MyData$Numberofcompetitorstores)
kurtosis(MyData$AverageHouseholdIncome)
skewness(MyData$Investment)
str(MyData)
MyData$Investment<-as.numeric(MyData$Investment)
MyData$EstimatedPopulationinvicinity<-as.numeric(MyData$EstimatedPopulationinvicinity)
MyData$ValuationofBusiness<-as.numeric(MyData$ValuationofBusiness)
skewness(MyData$Investment)
kurtosis(MyData$Investment)
skewness(MyData$EstimatedPopulationinvicinity)
kurtosis(MyData$EstimatedPopulationinvicinity)
skewness(MyData$ValuationofBusiness)
kurtosis(MyData$ValuationofBusiness)
MyData2<-MyData
MyData2$ValuationofBusiness<- log(MyData2$ValuationofBusiness)
MyData2$Investment<- log(MyData2$Investment)
MyData2$EstimatedPopulationinvicinity<- log(MyData2$EstimatedPopulationinvicinity)
skewness(MyData2$ValuationofBusiness)
skewness(MyData2$Investment)
skewness(MyData2$EstimatedPopulationinvicinity)
kurtosis(MyData2$Investment)
kurtosis(MyData2$ValuationofBusiness)
kurtosis(MyData2$EstimatedPopulationinvicinity)


#Q-2
LeveneTest(MyData2$ValuationofBusiness~MyData2$Citytype)
t.test(MyData2$ValuationofBusiness~MyData2$Citytype, var.equal=TRUE, data = MyData2)
t.test(MyData2$Investment~MyData2$TypeofLocation, var.equal=TRUE, data = MyData2)
k<-rcorr(cbind(MyData2$ValuationofBusiness,MyData2$Investment,MyData2$Numberofcompetitorstores, MyData2$EstimatedPopulationinvicinity, MyData2$AverageHouseholdIncome))
k
a<-table(MyData2$Citytype,MyData2$TypeofStore)
print(chisq.test(a))
MyData2$TypeofStore<-as.factor(MyData2$TypeofStore)
MyData2$TypeofLocation<-as.factor(MyData2$TypeofLocation)
MyData2$Citytype<-as.factor(MyData2$Citytype)
#Q-3

Model1<-lm(ValuationofBusiness~Investment+Numberofcompetitorstores+EstimatedPopulationinvicinity+AverageHouseholdIncome+TypeofStore+TypeofLocation+Citytype,data=MyData2)
summary(Model1)
pred<-predict(Model1, newdata = data.frame(Investment=50000,Numberofcompetitorstores = 5, EstimatedPopulationinvicinity=5000,AverageHouseholdIncome=20000,TypeofStore=factor("Large"),TypeofLocation=factor("Commercial"),Citytype=factor("Metro")))
pred

#Q-4
Model2<-glm(TypeofLocation~TypeofStore+Investment+Numberofcompetitorstores+EstimatedPopulationinvicinity,data=MyData2,family = "binomial")
summary(Model2)
pred2<-predict(Model2, newdata = data.frame(TypeofStore=factor("Large"),Investment=50000, Numberofcompetitorstores=5, EstimatedPopulationinvicinity= 5000),type="response")
pred2
