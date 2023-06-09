Valuation<-as.data.frame(Valuation)

#Checking for duplicates
sum(duplicated(Valuation,))

#Checking for missing values
sum(is.na(Valuation))
map(Valuation, ~sum(is.na(.)))

#Imputing missing values
Valuation<-impute(Valuation,method = "median/mode")   #From ImputeMissings package
str(Valuation)

#Converting to categorical variable
Valuation<-Valuation[-1]
Valuation$TypeofStore<-as.factor(Valuation$TypeofStore)
Valuation$TypeofLocation<-as.factor((Valuation$TypeofLocation))
Valuation$Citytype<-as.factor(Valuation$Citytype)
str(Valuation)

#Checking skewness/kurtosis (Moments Package)
x<-Valuation[c(4,5,6,7,8)]
sapply(x, skewness)
sapply(x, kurtosis)

#Bringing Skewness/Kurtosis of Business Value under control
logvalue<-log10(Valuation$ValuationofBusiness)
skewness(logvalue)
kurtosis(logvalue)

#Q2. A
Valuation %>%  
  group_by(Citytype) %>%
  summarise(skewness(log10(ValuationofBusiness)), #Normality (Dplyr package)
            kurtosis(log10(ValuationofBusiness)))

LeveneTest(logvalue,Valuation$Citytype)  #Homogeneity (DescTools package)
t.test(logvalue ~ Valuation$Citytype,var.equal = T,alternative="greater")  #Testing

#Q2. B
Valuation %>%                                                           
  group_by(TypeofLocation) %>%
  summarise(skewness(Investment),     #Normality
            kurtosis(Investment))

LeveneTest(Valuation$Investment,Valuation$TypeofLocation) # Homogeneity
t.test(Valuation$Investment ~ Valuation$TypeofLocation,var.equal = T)  #Testing

#Q2. C
rcorr(as.matrix(x)) #Hmisc package

#Q2. D
chisq.test(Valuation$Citytype,Valuation$TypeofStore)

#Q3.
attach(Valuation)
y<-lm(logvalue~TypeofStore+TypeofLocation+Citytype+Investment+Numberofcompetitorstores+EstimatedPopulationinvicinity+AverageHouseholdIncome)
summary(y)
lm.beta(y) #From lm.beta package

predict(y,newdata = data.frame(TypeofStore="Large",TypeofLocation="Commercial",Citytype="Metro",Investment=50000,Numberofcompetitorstores=5,
                               EstimatedPopulationinvicinity=5000,AverageHouseholdIncome=20000))

#Actual Valuation: 10^4.959399 = 91074.96 units since the dependent variable is in log10 form  

#Q4.
levels(TypeofLocation)
TypeofLocation<-relevel(TypeofLocation,ref = "Residential")
#OR
res<-ifelse(TypeofLocation=="Residential",1,0)

z<-glm(res~TypeofStore+Investment+Numberofcompetitorstores+EstimatedPopulationinvicinity ,data = Valuation,family = "binomial")
summary(z)

predict(z,newdata = data.frame(TypeofStore="Large",Investment=50000,Numberofcompetitorstores=5,EstimatedPopulationinvicinity=5000),type = "response")
