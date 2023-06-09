attach(Valuation)
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
outI<-boxplot(MyData$Investment)$out
outN<-boxplot(MyData$Numberofcompetitorstores)$out
outE<-boxplot(MyData$EstimatedPopulationinvicinity)$out
outA<-boxplot(MyData$AverageHouseholdIncome)$out
outV<-boxplot(MyData$ValuationofBusiness)$out
length(outI)
length(outN)
length(outE)
length(outA)
length(outV)
MyData2<-MyData
map(MyData2, ~sum(is.na(.)))
MyData2[MyData2$Investment %in% outI, "Investment"] = NA
MyData2[MyData2$EstimatedPopulationinvicinity %in% outE, "EstimatedPopulationinvicinity"] = NA
MyData2[MyData2$ValuationofBusiness %in% outV, "ValuationofBusiness"] = NA
map(MyData2, ~sum(is.na(.)))
MyData2<-impute(MyData, method="median/mode")
map(MyData2, ~sum(is.na(.)))
