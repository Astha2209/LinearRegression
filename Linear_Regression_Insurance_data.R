## Setting Working Directory
setwd("E:/Analytics Acceralotor Program/1- Nov 26th - Introduction to R and Linear Regression")

library(lmtest)
library(car)

## Reading the data in the file
DefaultData <- read.csv("Linear_Regression.csv")

## Checking the contents in the file
View(DefaultData)

## Checking if the data is imported properly
head(DefaultData)
tail(DefaultData)

## Checking the summary of the file
summary(DefaultData)

## Generating a plot of Dependent variable (Losses)
plot(DefaultData$Losses)

## Checking the quantile to find out the outlier limit
quantile(DefaultData$Losses, c(0,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))

## Creating the Capped Losses column with 1200 cap
DefaultData$CappedLosses<-ifelse(DefaultData$Losses>1200,1200,DefaultData$Losses)
plot(DefaultData$CappedLosses)

## Checking if Capped Losses column has been created properly or not
summary(DefaultData)

## Creating a new object by deleting Policy_Num and Losses columns
DefaultData2<-DefaultData[,-c(1,9)]

## Checking the headings of the new object
names(DefaultData2)

## Generating plots to see the relation between the independent variables and the dependent variable
plot(DefaultData2$Age,DefaultData2$CappedLosses)
plot(DefaultData2$Years_Drv_Exp,DefaultData2$CappedLosses)
plot(DefaultData$Number_Vehicles,DefaultData2$CappedLosses)
plot(DefaultData2$Gender,DefaultData2$CappedLosses)
plot(DefaultData2$Married,DefaultData2$CappedLosses)
plot(DefaultData2$Vehicle_Age,DefaultData2$CappedLosses)
plot(DefaultData2$Fuel_Type,DefaultData2$CappedLosses)

## Creating dummy variables for categorical variables in the dataset
DefaultData2$Gender_Dummy  <- ifelse(DefaultData2$Gender == "M" , 1, 0)
DefaultData2$Married_Dummy  <- ifelse(DefaultData2$Married == "Married" , 0 , 1)
DefaultData2$Fuel_Type_Dummy  <- ifelse(DefaultData2$Fuel_Type == "G" , 0 , 1)

## Removing the categorical variables from the dataset after adding the dummy variables for the same
DefaultData3 <- DefaultData2[, -c(4,5,7)]

## Looking at the column Headings
names(DefaultData3)

## Creating linear function for vif
vif_data<-lm(CappedLosses~Age+Years_Drv_Exp+Number_Vehicles+Gender_Dummy+Married_Dummy+Vehicle_Age+Fuel_Type_Dummy,data=DefaultData3)

## Checking Vif, vif>2 means presence of multicollinearity
vif(vif_data)

## Comparing R-square of Age and Years_Drv_Exp to check which performs better
age1<-lm(CappedLosses~Age,data=DefaultData3)
drv1<-lm(CappedLosses~Years_Drv_Exp,data=DefaultData3)
summary(age1)
summary(drv1)

## Keep Average_Age and remove Years_Drv_Exp
## Run Linear Regression w/o Years_Drv_Exp
lin_reg1<-lm(CappedLosses~Age+Number_Vehicles+Gender_Dummy+Married_Dummy+Vehicle_Age+Fuel_Type_Dummy,data=DefaultData3)

## Looking at the results
summary(lin_reg1)

## Remove Number_Vehicles as it is not significant in the calculation and run linear regression again
lin_reg<-lm(CappedLosses~Age+Gender_Dummy+Married_Dummy+Vehicle_Age+Fuel_Type_Dummy,data=DefaultData3)

## Checking the summary
summary(lin_reg)

## To check the presence of heteroskedasticity
bptest(lin_reg)

## Creating a Variance Covariance Matrix
library("sandwich")
vcovHC(lin_reg,omega=NULL, type="HC4")

## Fixing Heteroskedasticity using "Variance-Covariance" matrix
library("lmtest")
coeftest(lin_reg,df=Inf,vcov=vcovHC(lin_reg,type="HC4"))

summary(lin_reg)

"Now the model is fixed for any error due to multicollinearity and heteroskedasticity.

CappedLosses = 539.76 - 4.37*Age + 46.88*Gender_Dummy + 73.89*Married_Dummy -11.37*Vehicle_Age + 296.09*Fuel_Type_Dummy
"