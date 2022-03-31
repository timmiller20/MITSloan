#-----------------------
# ANALYTICS EDGE
# HW1
# TIM MILLER
#-----------------------

####### PROBLEM 1a #######
ames <- read.csv('AmesSales.csv')
ames$SalePrice = as.numeric(ames$SalePrice)
summary(ames$SalePrice)
hist(ames$SalePrice)

####### PROBLEM 1b #######
library(Rcpp)
library(caret)
RNGkind(sample.kind = "Rounding")
set.seed(219)
idx = createDataPartition(ames$SalePrice, p = 0.60, list = FALSE)
train = ames[idx,]
test = ames[-idx,]

#check split
mean(train$SalePrice)

#linear regression
mod <- lm(SalePrice ~ . , data=train)
summary(mod)

####### PROBLEM 1c #######

#plot model
par(mfrow=c(2,2))
plot(mod)
par(mfrow=c(1,1))

#Outliers
outliers=c("1451", "2114", "2115")
print(train[outliers,])
#Data summary
summary(train)

####### PROBLEM 1d #######

#remove outliers
train2 = ames[setdiff(idx,outliers), ]
mod2 <- lm(SalePrice ~ . , data=train2)
summary(mod2)

#test correlation
train2_cor <- subset(train2, select = -c(BldgType))
cor(train2_cor)


####### PROBLEM 1g #######
mod3 <- lm(SalePrice ~ BldgType+YearBuilt+Fireplaces+GarageArea+PoolArea+LivArea, data=train2)
summary(mod3)

### Make predictions - problem 1g ###

# Predictions on the training set
pred_train2 = predict(mod3, newdata=train2)

# Predictions on the test set
pred_test <- predict(mod3, newdata=test)


### Calculate OSR2 - problem 1g ###

#SSR of the test data
SSR_test = sum((test$SalePrice - pred_test)^2)

#baseline model
baseline_train = mean(train2$SalePrice)

# SST (total sum of squares) of the test set
SST_test = sum((test$SalePrice - baseline_train)^2)

# Finally, we can calculate the out-of-sample R2 
OSR2 = 1 - SSR_test / SST_test
OSR2





### Make predictions - problem 1d ###

# Predictions on the training set
pred_train2 = predict(mod2, newdata=train2)

# Predictions on the test set
pred_test <- predict(mod2, newdata=test)


### Calculate OSR2 - problem 1d ###

#SSR of the test data
SSR_test = sum((test$SalePrice - pred_test)^2)

#baseline model
baseline_train = mean(train2$SalePrice)

# SST (total sum of squares) of the test set
SST_test = sum((test$SalePrice - baseline_train)^2)

# Finally, we can calculate the out-of-sample R2 
OSR2 = 1 - SSR_test / SST_test
OSR2


####### PROBLEM 2a #######

# Load packages:
library(caret) # for randomly splitting training/test 
library(rpart) # for building CART model
library(rpart.plot) # a library for an alternative way of plotting CART trees
library(dplyr) # for data processing (we use dplyr for the rename function here)

#CART model

#factor categorical variable
train2$BldgType = as.factor(train2$BldgType)

PriceTree <- rpart(SalePrice ~ ., data=train2)
prp(PriceTree)

####### PROBLEM 2b #######

r2_osr2 <- function(tree, TrainData, TestData, yvar) {
  PredictTrain = predict(tree, newdata = TrainData)
  PredictTest = predict(tree, newdata = TestData)
  ymean = mean(TrainData[,yvar])
  
  SSETrain = sum((TrainData[,yvar] - PredictTrain)^2)
  SSTTrain = sum((TrainData[,yvar] - ymean)^2)
 
  # R2 is 1 minus the ratio of these terms
  R2 = 1 - SSETrain/SSTTrain
  print(paste0("R2=",R2))
  
  #OSR2
  SSETest = sum((TestData[,yvar] - PredictTest)^2)
  SSTTest = sum((TestData[,yvar] - ymean)^2)
  OSR2 = 1 - SSETest/SSTTest
  print(paste0("OSR2=",OSR2))
}
  
r2_osr2(tree=PriceTree, TrainData=train2, TestData=test, yvar="SalePrice")
  
  
####### PROBLEM 2c #######
tree.model2 = rpart(SalePrice ~ ., data=train2, control=rpart.control(cp=0.0001))
prp(tree.model2)
barplot(tree.model2$variable.importance, cex.names=0.7)

####### PROBLEM 2d #######
r2_osr2(tree=tree.model2, TrainData=train2, TestData=test, yvar="SalePrice")