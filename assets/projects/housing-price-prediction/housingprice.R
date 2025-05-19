install.packages("car")
library(caret)
library(base)
library(car)
library(ModelMetrics)
library(pls)

#Reading the data
housingData <- read.csv("housingData.csv")
#Converting data to string
str(housingData)
#Printing the first five rows
head(housingData)
#Printing the summary of data
summary(housingData)

#Question 1(a)-i
#Creating a hold-out validationSet using the first 100 observations in the data and the trainingSet using the remaining 900 observations form the data set "housingData"
validationSet<- housingData[1:100,]
trainingSet<- housingData[101:1000,]

#Creating the ols model using lm for remaining 100 observations
#creating ols model1
ols_model1 <- lm(log(SalePrice) ~YearBuilt + SaleType+ YrSold+MoSold+OverallQual+OverallCond, data = trainingSet)
#creating ols model2
ols_model2<- lm(log(SalePrice)~ YearRemodAdd + BsmtCond + Fence + GarageQual + LotConfig + Neighborhood+ YearBuilt + OverallQual + OverallCond, data= trainingSet)
#Creating ols model3
ols_model3<- lm(log(SalePrice)~ BsmtQual+ExterQual+ KitchenQual+ TotRmsAbvGrd + GarageArea + SaleType +YearRemodAdd + Fence + Neighborhood + YearBuilt + OverallQual+ OverallCond, data= trainingSet)
#Creating ols model4
ols_model4<- lm(log(SalePrice)~Foundation+ Functional + CentralAir+ Electrical + PavedDrive+ SaleType +BsmtQual +ExterQual+ KitchenQual+TotRmsAbvGrd+GarageArea+YearRemodAdd + Fence + Neighborhood + YearBuilt + OverallQual+ OverallCond , data=trainingSet)
#Creating ols model5
ols_model5<-lm(log(SalePrice)~Foundation+ CentralAir +PavedDrive +BsmtQual +ExterQual +KitchenQual +TotRmsAbvGrd +GarageArea +Neighborhood +YearBuilt +OverallQual, data=trainingSet)

#Printing the summary of all five ols models
summary(ols_model1)
summary(ols_model2)
summary(ols_model3)
summary(ols_model4)
summary(ols_model5)

#Printing the residuals of all five ols models
residuals1= residuals(ols_model1)
residuals2= residuals(ols_model2)
residuals3= residuals(ols_model3)
residuals4= residuals(ols_model4)
residuals5= residuals(ols_model5)

#For model 1
anova(ols_model1)
residualSE1<-sqrt(anova(ols_model1)[[3]][5])
residualSE1
#Calculating the RMSE value
RMSE_1<- sqrt(mean(ols_model1$residuals^2))
RMSE_1
str(ols_model1)
summary(ols_model1)
#The AIC value for ols model
AIC(ols_model1)
#Calculating the BIC for ols model
BIC(ols_model1)
#Calculating the VIF  for ols model 
vif_values1<-vif(ols_model1)

#For model 2
anova(ols_model2)
residualSE2<-sqrt(anova(ols_model2)[[3]][5])
residualSE2
#Calculating the RMSE value
RMSE_2<- sqrt(mean(ols_model2$residuals^2))
RMSE_2
str(ols_model2)
summary(ols_model2)
#The AIC value for ols model
AIC(ols_model2)
#Calculating the BIC for ols model
BIC(ols_model2)
#Calculating the VIF  for ols model 
vif_values2<-vif(ols_model2)

#For model 3
anova(ols_model3)
residualSE3<-sqrt(anova(ols_model3)[[3]][5])
residualSE3
#Calculating the RMSE value
RMSE_3<- sqrt(mean(ols_model3$residuals^2))
RMSE_3
str(ols_model3)
summary(ols_model3)
#The AIC value for ols model
AIC(ols_model3)
#The BIC value for ols model
BIC(ols_model3)
#The VIF value for ols model
vif_values3<-vif(ols_model3)

#For model 4
anova(ols_model4)
residualSE4<-sqrt(anova(ols_model4)[[3]][5])
residualSE4
#Calculating the RMSE value
RMSE_4<- sqrt(mean(ols_model4$residuals^2))
RMSE_4
str(ols_model4)
summary(ols_model4)
#The AIC value for ols model
AIC(ols_model4)
#The BIC value for ols model
BIC(ols_model4)
#The VIF value for ols model
vif_values4<-vif(ols_model4)

#For model 5
anova(ols_model5)
residualSE5<-sqrt(anova(ols_model5)[[3]][5])
residualSE5
#Calculating the RMSE value
RMSE_5<- sqrt(mean(ols_model5$residuals^2))
RMSE_5
str(ols_model5)
summary(ols_model5)
#The AIC value for ols model
AIC(ols_model5)
#The BIC value for ols model
BIC(ols_model5)
#The VIF value for ols model
vif_values5<-vif(ols_model5)

#ols model [5] is taken as the best model among the five models as it has the best rmse value among all other five models.
# The RMSE value(residualSE5) for ols model- [5], is 0.1452105 is and the p- value and adjusted R^2 values are 2.2e^-16 and 0.8255. We take these values as the final values as they are much better.



#Question 1(a)-ii

#Residuals for ols model 1
residuals_ols1<- residuals(ols_model1)
#Residuals for ols model 2
residuals_ols2<-residuals(ols_model2)
#Residuals for ols model 3
residuals_ols3<-residuals(ols_model3)
#Residuals for ols model 4
residuals_ols4<- residuals(ols_model4)
#Residuals for ols model 5
residuals_ols5<-residuals(ols_model5)

# Plotting the ols models
plot(ols_model1)
plot(ols_model2)
plot(ols_model3)
plot(ols_model4)
plot(ols_model5)

# We take OLS model [5] as the best model
plot(ols_model5)
qqline(ols_model5$residuals)

residualPlots(ols_model5)

#OlS -[5] is a strong model by depicting the residuals and when we break down the residuals by factor , most of them are evenly distributed above and below the horizontal axis with little bit non linear relation ship of 'OverallQual'.
#If we transform the OverallQual variable , we may get improved model performance , and also to make the model more efficient , outlier handling can be considered.



#Question 1 (b)

#Finding the count of missing values
missing_count <- sapply(housingData, function(x) sum(length(which(is.na(x)))))
max_missing <- missing_count[missing_count>200]
#Removing the missing values from data
housingData1 <- housingData[,!names(housingData) %in% names(max_missing)]
act_housingData <- na.omit(housingData1)

#Creating PLS model with hyper-prara meter tuning with number of components as 20 and using cross validation with method as oscorepls
set.seed(5103)
pls_model1 <- plsr(log(SalePrice)~., 20, data = act_housingData, method = "oscorespls", validation = "CV")
#Generating the summary of PLS model 1
summary(pls_model1)

#Choosing number of components as 8 as best by analysing pls_model1

#Creating pls model for the best components with crossvalidation and method as oscorespls
pls_model2 <- plsr(log(SalePrice)~., 8, data = act_housingData, method = "oscorespls", validation = "CV")
#Creating the summary of PLS model 2
summary(pls_model2)
R2(pls_model2)

#Plotting the pls model 2
plot(pls_model2, plottype = "scores", comps = 1:8)
#Finding the beta values 
beta_pls <- drop(coef(pls_model2))
beta_pls
#Finding the residuals
residuals_pls <- drop(pls_model2$resid)[,8]
residuals_pls

# The RMSE value with 8 components is 0.1512
#The number of components are 8

