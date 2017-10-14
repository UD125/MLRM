#---------------Multiple Linear Regression Modelling (MLRM)----------------#

#Problem Statement :
#To examine which are the factors which influence Customer Lifetime Value (CLV) of Insurance Premium Company,
#based on the given attributes of the customer.

#Business Context :
#What are the attributes of Customer, who have a higher Customer Lifetime Value ?

#---------------Preparing the environment for MLRM--------------------#

list.of.packages <- c("boot","car","QuantPsyc","lmtest","sandwich","vars","nortest","MASS","caTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] #If not in the List of Packages it will install it.
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.rstudio.com/")

library(boot)
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)


#--------------Setting Working Directory-------------------------------#
Path <- "E:/R/MLRM"
setwd(Path)
getwd()

data <- read.csv("WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv")
data1 <- data #To create backup of orignal data

#-------------Basic Exploration of the data-----------------------#
str(data1)
summary(data1) #In summary I notice that my Dependent/Target Varaible(Customer Lifetime Value) has the possibility of outlier
dim(data1)

#Renaming the Dependent Var
colnames(data1)[which(names(data)=="Customer.Lifetime.Value")] = "clv"

#--------------Outlier Treatment through quantile method---------------#

quantile(data1$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
#Here is a high jump after 90% in values but between 99.5% - 100% there is very high change in value. So, we gradually move in treatment of outliers.
#Here, You will notice that between 99.5% - 100% there is very a huge jump in values as compared with other quantile values.
#So, they are definately an outlier.

data2 <- data1[data1$clv <36000,] #Creating a new dataframe with a cap on outliers.
summary(data2)
nrow(data1)
nrow(data2)
nrow(data1)-nrow(data2)

quantile(data2$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
#Here, You will notice that there is high change between 90%-95% and 95%-99% values as compared with other quantile values. So, there is an outlier.

data3 <- data2[data2$clv <14722,]

nrow(data3)
nrow(data1)-nrow(data3)
quantile(data3$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
str(data3)

#--------------------Missing Values Identification and Treatment-----------------------#

sapply(data3,function(x)sum(is.na(x))) #One way to check
as.data.frame(colSums(is.na(data3))) #Other Way
#Here we find that there is no missing value in the dataset.

#-------------------Dropping the redundant variables from the dataframe
library(dplyr) #For manipulation of data , keeping and dropping variables.
as.data.frame(colnames(data3))
data4 <- select(data3, - c(Customer,State,Effective.To.Date))
str(data4)

#-----------Exporting the treated data into CSV file--------------# (For backup)
write.csv(data4,"MLdata.csv")

#-----------Splitting the data into Training and Test data set----------#

set.seed(144) #This is used to produce reproducible results, everytime we run the model.
spl = sample.split(data4$clv,0.7) #Splits the overall data into train and test data in 70:30 ratio.

orignal.data <- subset(data4,spl==TRUE)
str(orignal.data)
dim(orignal.data)

test.data <- subset(data4,spl==FALSE)
str(test.data)
dim(test.data)

#---------------------------Fitting the model----------------------------#

#Iteration.1 We start with testing all variables
#Iterate means to remove the variable which has higher P-value

LinearModel0 <- lm(clv ~ . , data = orignal.data) # '.' refers to all varaibles , #lm is the function to built Linear regression model
summary(LinearModel0)

#Removing the first two insignificant variables : policy and policy types
#Iteration.2
LinearModel1 <- lm(clv ~ Response+ Coverage+ Education+ EmploymentStatus+ Gender+ Income+ Location.Code+ Marital.Status+ Monthly.Premium.Auto+ Months.Since.Last.Claim+ Months.Since.Policy.Inception+ Number.of.Open.Complaints+ Number.of.Policies+ Renew.Offer.Type+ Sales.Channel+ Total.Claim.Amount+ Vehicle.Class+ Vehicle.Size, data= orignal.data)
summary(LinearModel1)

#Removing the insignificant variables: Total Claim, Sales Channel, Months Since Last Claim, Marital Status, Location, Income
#Iteration.3
LinearModel2 <- lm(clv ~ Coverage+ Education+ EmploymentStatus+ Monthly.Premium.Auto+ Months.Since.Policy.Inception+ Number.of.Open.Complaints+ Number.of.Policies+ Renew.Offer.Type+ Vehicle.Class, data = orignal.data )
summary(LinearModel2)

#Iteration.4
LinearModel3 <- lm(clv ~ Coverage+ I(Education== "College")+ I(Education== "High School or Below")+ EmploymentStatus+ Monthly.Premium.Auto+ Months.Since.Policy.Inception+ Number.of.Open.Complaints+ Number.of.Policies+ Renew.Offer.Type+ Vehicle.Class, data = orignal.data)
summary(LinearModel3)

#---------------------------Final Model---------------------#
FinalModel <-lm(clv ~ Coverage+ I(Education== "College")+ I(EmploymentStatus== "Medical Leave")+ I(EmploymentStatus== "Retired")+ I(EmploymentStatus== "Unemployed")+ Monthly.Premium.Auto+ Number.of.Open.Complaints+ Number.of.Policies+ Renew.Offer.Type+ I(Vehicle.Class== "SUV"),data = orignal.data )
summary(FinalModel)



#Checking the Multicollinearity in the model.

vif(FinalModel)

##Get the predicted or fitted values
fitted(FinalModel)

par(mfrow=c(2,2)) #To show 4 plots at a time.
plot(FinalModel)

#Plot1 - Residuals vs Fitted shows No Pattern (a Straight type line) which means that there is a Linear Relationship.
#Plot2 - Normal Q-Q shows that Standard Residuals or Errors are Normally Distributed.
#Plot3 - Scale-location shows that there is a straight line pattern and showing less variance in Residual term.
#Plot4 - Residual vs Leverage shows that there are less other significant Outliers in the model.



#Mape
orignal.data$pred <- fitted(FinalModel)
write.csv(orignal.data,"mape.csv")

#Calculating Mape
attach(orignal.data)
MAPE <- print((sum((abs(clv - pred))/clv))/nrow(orignal.data))

#MAPE - It is a summary statistic showing how different is your Prediction from Actual Result.
# MAPE (0.3636168) - It shows that Prediction is 36% different from Actual Result.

#---------------------Residual Analysis----------------------#
res <- orignal.data
res$stu_res <- studres(FinalModel) #Studentized Residuals
res$stud.del.resids <- rstudent(FinalModel) #Studentized deleted Residuals
res$leverage <- hatvalues(FinalModel) #Leverage values (hi)
res$cook_dis <- cooks.distance(FinalModel) #Cook's Distance
res$dffits <- dffits(FinalModel) #Dffit
res$dfbetas <- dfbetas(FinalModel) #Dfbetas
res$cov_ratio <- covratio(FinalModel) #Covariance Ratio

write.csv(res,"res.csv")

#---------------Checking of Assumption--------------------#

#Residual should be uncorrelated ##AutoCorrelation
#Null H0: residuals from a linear regression are uncorrelated. Value should be close to 2
#Less then 1 or greater then 3 <-- concern
##Should get a high P-value

durbinWatsonTest(FinalModel)
dwt(FinalModel)

#Since, the p-value > 0.05, we fail to reject H0: No Autocorrelation

# Checking Multicollinearity
vif(FinalModel) #Should be within 2, If it is greater then 10 then serious problem.

#--------Constant error variance----------------# (Homoscedasticity) & (Hetroscadisticity)

#Breusch-Pagan Test
bptest(FinalModel) #Null Hypothesis -> error is non-homogenious (it shows that hetroscadasticity) (p-value should be more than 0.05)

#Cook-weisberg test for Homoscedasticity test
#Non-constant Variance Score Test
ncvTest(lm(clv ~ Coverage+ I(Education== "College")+ I(EmploymentStatus== "Medical Leave")+ I(EmploymentStatus== "Retired")+ I(EmploymentStatus== "Unemployed")+ Monthly.Premium.Auto+ Number.of.Open.Complaints+ Number.of.Policies+ Renew.Offer.Type+ I(Vehicle.Class== "SUV"),data = orignal.data ))

#---------Normality Testing , Null Hypothesis --> Data is Normal
resids <- FinalModel$residuals
resids

ad.test(resids) #get Anderson-Darling test for Normality
cvm.test(resids) #get Cramer-Von Mises test for Normality
lillie.test(resids) #get  Lilliefors test for Normality
pearson.test(resids) #get pearson chi-square test for Normality
sf.test(resids) #get Shapiro-Francis test for Normality

qqnorm(resids) #It shows residuals are normally distributed

#--------------------Checking the model on Test Data--------------------#
##############################################################################
fit1 <- lm(clv ~ Coverage+ I(Education== "College")+ I(EmploymentStatus== "Medical Leave")+ I(EmploymentStatus== "Retired")+ I(EmploymentStatus== "Unemployed")+ Monthly.Premium.Auto+ Number.of.Open.Complaints+ Number.of.Policies+ Renew.Offer.Type+ I(Vehicle.Class== "SUV"),data = test.data )
summary(fit1)

#Remove I(Education== "College") , I(EmploymentStatus== "Medical Leave") , I(EmploymentStatus== "Retired")
fit1 <- lm(clv ~ Coverage+ I(EmploymentStatus== "Unemployed")+ Monthly.Premium.Auto+ Number.of.Open.Complaints+ Number.of.Policies+ Renew.Offer.Type+ I(Vehicle.Class== "SUV"),data = test.data )
summary(fit1)

#Remove Coverage Extended
fit1 <- lm(clv ~ I(Coverage== "Premium")+ I(EmploymentStatus== "Unemployed")+ Monthly.Premium.Auto+ Number.of.Open.Complaints+ Number.of.Policies+ Renew.Offer.Type+ I(Vehicle.Class== "SUV"),data = test.data )
summary(fit1)

#-------------Final Equation (Conclusion)------------------------#
FinalEq <- lm(clv ~ I(Coverage== "Premium")+ I(EmploymentStatus== "Unemployed")+ Monthly.Premium.Auto+ Number.of.Open.Complaints+ Number.of.Policies+ Renew.Offer.Type+ I(Vehicle.Class== "SUV"),data = test.data )
summary(FinalEq)

##Conclusion:
##The Factors or Attributes that have high "Customer Lifetime Value (clv)" :-
# 1- Coverage == "Premium"
# 2- EmploymentStatus == "Unemployed"
# 3- Monthly.Premium.Auto
# 4- Number.of.Open.Complaints
# 5- Number.of.Policies
# 6- Renew.Offer.Type
# 7- Vehicle.Class == "SUV"

## Conclusion - These above attributes of customer have high "Customer Lifetime Value". 
