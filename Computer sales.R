library(readr)
Computersales1<- read.csv(file.choose()) # choose the Computer_Data.csv data set
Computersales<-Computersales1[,-1]
View(Computersales)
summary(Computersales)

library(dummies)
#creating dummy for "cd" column
Computersalescd <- data.frame(x=c('yes','no'),rank=c(1,0))
#ifelse(df$colomn=='No',0,1)
Computersales[,6]<-ifelse(Computersales$cd=='no',0,1)
View(Computersales)

#creating dummy for "multi" column
Computersalesmulti <- data.frame(x=c('yes','no'),rank=c(1,0))
Computersales[,7]<-ifelse(Computersales$multi=='no',0,1)
table(Computersales$multi)
View(Computersales)

#creating dummy for "premium" column
Computersalespremium <- data.frame(x=c('yes','no'),rank=c(1,0))
Computersales[,8]<-ifelse(Computersales$premium=='no',0,1)
table(Computersales$premium)
View(Computersales)


# Find the correlation b/n Output (Profit) & Input-Scatter plot
pairs(Computersales)

library(GGally)
ggpairs(Computersales)

# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Computersales)

### Partial Correlation matrix - Pure Correlation  b/n the variables
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Computersales))

# Split the data into training and test data for validation purpose  
training_size= round(nrow(Computersales)* 0.7)   # the size of training dataset (70%)
test_size= nrow(Computersales)-training_size     # the size of  test data set 
training_sample= sample(nrow(Computersales), training_size,replace = FALSE, set.seed(1)) # select training data set

training_data= Computersales[training_sample, ]
test_data=Computersales[-training_sample, ]

# The Linear Model of interest with all the columns
model_1 <- lm(price~.,data=training_data)

summary(model_1)
#as above all variables are siginificant as values are less than 0.05.
#R^2 value is 0.769 which is less than 0.80 so we can say that model is needs improvement.
#As the  R^2 value is less so we need to check its multicollinearity individually
# Multicollinearity check
install.packages("car")
library(car)
# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
vif(lm(price~., data= training_data)) # Original model
## vif>10 then there exists collinearity among all the variables 
#as vif value is less than 10 so there is no autocollinearity in variables, we need to check about influencing entries

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model_1,id.n=5,id.cex=0.7)
# The above plots will reveal whether the Output Price
# has an effect by changing the inputs
# From the graph we can see all the variables are contributing towards predicting price
#but as it is better to delete only influencing observation rather than deleting whole so we check influence index plot
influenceIndexPlot(model_1)
influencePlot(model_1)
#From influence index plot we can say that 1441,1701 no.observation are influencing badly so we need to remove those  
# Preparing new models by excluding above observations we check for any improvements in model or not

#model after deleting 1441,1701 no. influencing observations
model_2= lm(price~., data= training_data[-c(1441,1701),])
summary(model_2)
#as above all variables are siginificant as values are less than 0.05.
#R^2 value is 0.769 which is less than 0.80 so we can say that model needs improvement.
# applying transformation


# Logarithamic Model

# x = log(training_data); y = price

#Scatterplot of input Vs output
pairs(log(training_data))
#From plot we can say that data is linearity is there,strength is moderate (sub to check r value),
#& Direction is positive

#Correlation between output to input
cor(log(training_data))
# Simple Linear Regression model-log transform
model_log <- lm(price ~ log(speed)+log(hd)+log(ram)+log(screen)+cd+multi+premium , data = training_data)# lm(Y ~ log(X)
names(training_data)
#Summary of regression model

summary(model_log)
#as above all variables other than multi are siginificant as values are less than 0.05.
#R^2 value is 0.5081 which is less than 0.80 so we can say that model needs improvement.
# applying transformation


# Square root Model

# x = (training_data)^1/2 ; y = price

#Scatterplot of input Vs output
pairs((training_data)^1/2)
#From plot we can say that data is linearity is there,strength is moderate (sub to check r value),
#& Direction is positive

#Correlation between output to input
cor((training_data)^1/2)
# Simple Linear Regression model-log transform
model_root <- lm(price ~ sqrt(speed)+sqrt(hd)+sqrt(ram)+sqrt(screen)+sqrt(cd)+sqrt(multi)+sqrt(premium)+sqrt(ads)+sqrt(trend),data = training_data) 
names(training_data)
#Summary of regression model
summary(model_root)
#as above all variables are siginificant as values are less than 0.05.
#R^2 value is 0.7808 which is less than 0.80 so we can say that model needs improvement.
# applying transformation

# cube root Model

# x = (training_data)^1/3 ; y = price

#Scatterplot of input Vs output
pairs((training_data)^1/3)
#From plot we can say that data is linearity is there,strength is moderate (sub to check r value),
#& Direction is positive

#Correlation between output to input
cor((training_data)^1/3)
# Simple Linear Regression model-log transform
model_root_3 <- lm(price ~ (I(speed^(1/3))+I(hd^(1/3))+I(ram^(1/3))+I(screen^(1/3))+I(cd^(1/3))+I(multi^(1/3))+I(premium^(1/3))+I(ads^(1/3))+I(trend^(1/3))),data = training_data) 
names(training_data)
#Summary of regression model
summary(model_root_3)
#as above all variables are siginificant as values are less than 0.05.
#R^2 value is 0.7737 which is less than 0.80 so we can say that model needs improvement.
# applying transformation

# Degree 3 Model

# x = (training_data)^3 ; y = price

#Scatterplot of input Vs output
pairs((training_data)^3)
#From plot we can say that data is linearity is there,strength is moderate (sub to check r value),
#& Direction is positive

#Correlation between output to input
cor((training_data)^3)
# Simple Linear Regression model-log transform
model_power_3 <- lm(price ~ (I(speed^(3))+I(hd^(3))+I(ram^(3))+I(screen^(3))+I(cd^(3))+I(multi^(3))+I(premium^(3))+I(ads^(3))+I(trend^(3))),data = training_data) 
#Summary of regression model
summary(model_power_3)
#as above all variables other than multi are siginificant as values are less than 0.05.
#R^2 value is 0.5576 which is less than 0.80 so we can say that model needs improvement.
# applying transformation

# Degree 2 Model

# x = (training_data)^3 ; y = price

#Scatterplot of input Vs output
pairs((training_data)^2)
#From plot we can say that data is linearity is there,strength is moderate (sub to check r value),
#& Direction is positive

#Correlation between output to input
cor((training_data)^2)
# Simple Linear Regression model-log transform
model_power_2 <- lm(price ~ (I(speed^(2))+I(hd^(2))+I(ram^(2))+I(screen^(2))+I(cd^(2))+I(multi^(2))+I(premium^(2))+I(ads^(2))+I(trend^(2))),data = training_data) 
#Summary of regression model
summary(model_power_2)
#as above all variables are siginificant as values are less than 0.05.
#R^2 value is 0.6793 which is less than 0.80 so we can say that model needs improvement.
# applying transformation
 
#we are finalising model_root as final model.


sum(model_root$residuals)
plot(model_root)
training.rmse<-sqrt(mean(model_root$residuals^2))

#but we have to check with predicted values
predtest <- predict(model_root,test_data)

actual<- test_data$price

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

#training RMSe
sqrt(mean(model_root$residuals^2))

#test RMSE
sqrt(mean(error^2))
