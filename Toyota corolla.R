#Housekeeping
library(readr)
Corolla<- read.csv(file.choose()) # choose the ToyotaCorolla.csv data set
View(Corolla) #view dataset
summary(Corolla) #summary of dataset/EDA

#Invoke all required packages
# Important libraries
install.packages("rJava")
install.packages("car")      # to evaluate regression conditions
install.packages("biglm")
install.packages("leaps")    # to perform all subsets regression
install.packages("stargazer")
library(readr)

library(rJava)
library(car)      # to evaluate regression conditions
library(biglm)
library(leaps)    # to perform all subsets regression
library(stargazer)
library(readr)

#Overall Summary statistics
stargazer(Corolla["Price"], type = "text", title="Summary statistics of Car Price",
          digits=0, median=TRUE, iqr=TRUE, min.max=TRUE)

# Add factor variables for fuel type  Diesel and Petrol by taking CNG as a base 
Corolla$Fuel_Type= factor(Corolla$Fuel_Type,
                          levels = c('Diesel','Petrol','CNG'),
                          labels = c(1,2,3))
View(Corolla)

#Subset Model Selection
var= Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
summary(var) #summary selected dataset

# Split the data into training and test data for validation purpose  
training_size= round(nrow(var)* 0.7)   # the size of training dataset (70%)
test_size= nrow(var)-training_size     # the size of  test data set 
training_sample= sample(nrow(var), training_size,replace = FALSE, set.seed(1)) # select training data set

training_data= var[training_sample, ]
test_data=var[-training_sample, ]

#all subset regression plot
SubReg=regsubsets(Price ~., data=training_data[, -c(1,2,5,6,8,11,15)], nbest=2) # chooses the best two models for each subset size 
plot(SubReg, scale = "adjr2")
View(SubReg)

View(training_data)
View(test_data)

#Scatterplot of input Vs otput
pairs(var)

library(GGally)
ggpairs(var)

#Correlation coef.matrix-strength & Direction of correlation
cor(var)

#partial correlation matrix - pure correlation b/n variables
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(var))


#Fitting the Model
model1= lm(Price~., data= training_data)


#Summary of regression model
summary(model1)
#cc & Doors variable are not siginificant as values are more than 0.05 and rest other variables are significant
#R^2 value is more than 0.80 so we can say that model is best fit.
#As Probability value for cc,Doors are insignificant in above model so we need to check its multicollinearity individually
# Multicollinearity check

# Model based on only cc 
model1.cc<-lm(Price~cc,data=training_data)
summary(model1.cc) # cc became significant


# Model based on only Doors 
model1.Doors<-lm(Price~Doors,data=training_data)
summary(model1.Doors) # Doors became significant

# Model based on only cc+Doors 
model1.cd<-lm(Price~ cc+Doors,data=training_data)
summary(model1.cd) # Doors&cc became significant in combination so we need to look out for outliers in these columns

library(tidyverse)
library(caret)
library(car)
# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
vif(lm(Price~., data= training_data)) # Original model
## vif>10 then there exists collinearity among all the variables 
#as vif value is less than 10 so there is no autocollinearity in variables, we need to check about influencing entries

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model1,id.n=5,id.cex=0.7)
# The above plots will reveal whether the Output Price
# has an effect by changing the inputs
# From the graph we can see there is change at all with 
# respect to Doors

influenceIndexPlot(model1)
influencePlot(model1)

#from avplot it is clear that Doors variable is least contributing towards predicting price so need to remove it 
#but as it is better to delete only influencing observation rather than deleting whole so we check influence index plot
#From influence index plot we can say that 81,961,222 no.observation are influencing badly so we need to remove those  
# Preparing new models by excluding above observations we check for any improvements in model or not

#model after deleting 81 no. influencing observations
model_1= lm(Price~., data= training_data[-81,])
summary(model_1)

#model after deleting 81, 961 & 222 no. influencing observations
model_2= lm(Price~., data= training_data[-c(81,222,961)])
summary(model_2)
#from summary it is seen that variable cc and doors are still insignificant so by the ref of avplot 
#check model by deleting doors
model_3= lm(Price~.-Doors, data= training_data)
summary(model_3)
#from summary it is seen that variable cc is still insignificant after removing Doors
#so final model is by deleting doors & cc
model_4= lm(Price~.-Doors-cc, data= training_data)
summary(model_4)
#from summary it is seen that all variable are significant after and r^2 value is also good
#so we cosider model_4 as final model 

sum(model_4$residuals)
plot(model_4)
training.rmse<-sqrt(mean(model_4$residuals^2))

#but we have to check with predicted values
predtest <- predict(model_4,test_data)

actual<- test_data$Price

error<- actual-predtest
error

#test RMSE
test.rmse<-sqrt(mean(error^2))


#Check for error associated with each obs.
sum(error)

#check for mean of sum of errors is equal to 0.
mean(error)
hist(error) # check errors are normally distributed or not.

#Check for RMSE value
sqrt(sum(error^2)/nrow(training_data))  #RMSE
sqrt(mean(error^2))

# visualization

ggplot(data = Corolla, aes(x =Price+Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, y = `Price`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Corolla, aes(x=Price+Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight , y=pred))

#Train RMSE
training.rmse<-sqrt(mean(model_4$residuals^2))
sqrt(mean(model_4$residuals^2))
#Test RMSE
test.rmse<-sqrt(mean(error^2))
sqrt(mean(error^2))
#as values of train & test are close to each other so this model_4 is best fit model.
