#-----------------------
# ANALYTICS EDGE
# HW4
# TIM MILLER
#-----------------------

# Packages 
library(caret)
library(glmnet)
library(leaps)
library(ggcorrplot)
library(gridExtra)
library(xgboost)


## PROBLEM 1a ##

#read data
options(stringsAsFactors = FALSE)
Hitters_raw <- read.csv("Hitters.csv")

#correlation matrix
matrix = cor(Hitters_raw[,2:18])
matrix

#plot correlation matrix
ggcorrplot(matrix)


## PROBLEM 1c ##

# Normalize data
pp <- preProcess(Hitters_raw, method=c("center", "scale"))
Hitters <- predict(pp, Hitters_raw)
# Train/test
RNGkind(sample.kind = "Rejection")
set.seed(299)
train.obs <- createDataPartition(Hitters$Salary, p = 0.70, list = FALSE)
train <- Hitters[train.obs,2:21]
test <- Hitters[-train.obs,2:21]

# Prepare the data matrices for glmnet functions
x.train=model.matrix(Salary~.,data=train)
y.train=train$Salary
x.test=model.matrix(Salary~.,data=test) 
y.test=test$Salary

mean(y.train)

#compare x.train to train dataframe
x.train
train

## PROBLEM 1c ##

#regression
train
mod <- lm(Salary ~ ., data = train)
summary(mod)

# Make predictions on test and train sets
PredictTrain = predict(mod, newdata = train)
PredictTest = predict(mod, newdata = test)

# Calculate R-Squared
SSTTrain = sum((train$Salary - mean(train$Salary))^2)
SSETrain = sum((PredictTrain - train$Salary)^2)
R2_LR <- 1 - SSETrain/SSTTrain

#Calculate OSR-Squared
SSTTest = sum((test$Salary - mean(test$Salary))^2)
SSETest = sum((PredictTest - test$Salary)^2)
OSR2_LR <- 1 - SSETest/SSTTest


## PROBLEM 1f ##

# ridge regression cross-validation
all.lambdas <- c(exp(seq(15, -10, -.1)))
set.seed(300)
ridge.cv=cv.glmnet(x.train, y.train, alpha=0, lambda=all.lambdas)
plot(ridge.cv, main="Ridge regression MSE\n")

#find min lambda
ridge.cv$lambda.min

# ridge regression re-fit
best.lambda.ridge <- ridge.cv$lambda.min 
ridge.model=glmnet(x.train,y.train,alpha=0,lambda=best.lambda.ridge)

beta.ridge = ridge.model$beta
sum(beta.ridge != 0)


## PROBLEM 1g ##

# LASSO cross-validation
all.lambdas <- c(exp(seq(15, -10, -.1)))
set.seed(301)
lasso.cv=cv.glmnet(x.train, y.train, alpha=1, lambda=all.lambdas)
plot(lasso.cv, main="LASSO regression MSE\n")

#find min lambda
lasso.cv$lambda.min

# lasso regression re-fit
best.lambda.lasso <- lasso.cv$lambda.min 
lasso.model=glmnet(x.train,y.train,alpha=1,lambda=best.lambda.lasso)

beta.lasso = lasso.model$beta
sum(beta.lasso != 0)

## PROBLEM 1h ##

# Coefficient plot 
coeff.matrix = as.matrix(cbind(ridge.model$beta, lasso.model$beta))
barplot(t(coeff.matrix[order(ridge.model$beta),]), 
        beside=TRUE, horiz = TRUE, las=1, cex.names=.7, 
        legend.text = c("Ridge","LASSO"), main = "Model coefficients")


## PROBLEM 1i ##

##RIDGE

# Make predictions on test and train sets
PredictTrain = predict(ridge.model, newx = x.train)
PredictTest = predict(ridge.model, newx = x.test)

# Calculate R-Squared
SSTTrain = sum((y.train - mean(y.train))^2)
SSETrain = sum((PredictTrain - y.train)^2)
R2_Ridge <- 1 - SSETrain/SSTTrain
R2_Ridge

#Calculate OSR-Squared
SSTTest = sum((y.test - mean(y.test))^2)
SSETest = sum((PredictTest - y.test)^2)
OSR2_Ridge <- 1 - SSETest/SSTTest
OSR2_Ridge

##LASSO

# Make predictions on test and train sets
PredictTrain = predict(lasso.model, newx = x.train)
PredictTest = predict(lasso.model, newx = x.test)

# Calculate R-Squared
SSTTrain = sum((y.train - mean(y.train))^2)
SSETrain = sum((PredictTrain - y.train)^2)
R2_LASSO <- 1 - SSETrain/SSTTrain
R2_LASSO

#Calculate OSR-Squared
SSTTest = sum((y.test - mean(y.test))^2)
SSETest = sum((PredictTest - y.test)^2)
OSR2_LASSO <- 1 - SSETest/SSTTest
OSR2_LASSO


## PROBLEM 1j ##

# Elastic net cross-validation
set.seed(302)
elnet.cv=train(Salary~.,train, method = "glmnet",
               trControl = trainControl(method = "cv", number = 10),
               tuneGrid=expand.grid(alpha=seq(0,1,.1), lambda=all.lambdas) )

#select best alpha and lambda
best.elnet.params=elnet.cv$bestTune
best_alpha = best.elnet.params$alpha
best_lambda = best.elnet.params$lambda

best_alpha
best_lambda

## PROBLEM 1k ##

# Elastic Net re-fit
elnet.model=glmnet(x.train,y.train,alpha=best.elnet.params$alpha,lambda=best.elnet.params$lambda)


##ELASTIC NET

# Make predictions on test and train sets
PredictTrain = predict(elnet.model, x.train)
PredictTest = predict(elnet.model, x.test)

# Calculate R-Squared
SSTTrain = sum((y.train - mean(y.train))^2)
SSETrain = sum((PredictTrain - y.train)^2)
R2_ELNET <- 1 - SSETrain/SSTTrain
R2_ELNET

#Calculate OSR-Squared
SSTTest = sum((y.test - mean(y.test))^2)
SSETest = sum((PredictTest - y.test)^2)
OSR2_ELNET <- 1 - SSETest/SSTTest
OSR2_ELNET

## PROBLEM 1l ##

# Coefficient plot 2
coeff.matrix = as.matrix(cbind(ridge.model$beta, elnet.model$beta, ridge.model$beta))
barplot(t(coeff.matrix[order(ridge.model$beta),]),
        beside=TRUE, horiz = TRUE, las=1, cex.names=.7,
        legend.text = c("Ridge","ElNet","LASSO"), main = "Model coefficients")


## PROBLEM 1m ##

# Feature selection
set.seed(308)
fs <- train(Salary~., train, method = "leapForward",
            trControl = trainControl(method = "cv", number = 10),
            tuneGrid = expand.grid(.nvmax=seq(1,15)))

fs$bestTune$nvmax

nvars.fs = fs$bestTune$nvmax
coef(fs$finalModel, nvars.fs)

## PROBLEM 1n ##

# Make predictions on test and train sets
PredictTrain = predict(fs, newdata = train)
PredictTest = predict(fs, newdata = test)

# Calculate R-Squared
SSTTrain = sum((train$Salary - mean(train$Salary))^2)
SSETrain = sum((PredictTrain - train$Salary)^2)
R2_LR2 <- 1 - SSETrain/SSTTrain
R2_LR2

#Calculate OSR-Squared
SSTTest = sum((test$Salary - mean(test$Salary))^2)
SSETest = sum((PredictTest - test$Salary)^2)
OSR2_LR2 <- 1 - SSETest/SSTTest
OSR2_LR2


## PROBLEM 1o ##

# XGBoost
set.seed(304)
xgb.cv <-  train(Salary~.,train,
                 method = "xgbTree", objective = "reg:squarederror",
                 trControl = trainControl(method="cv", number=10))

best.xgb.params=xgb.cv$bestTune
best.xgb.params

xgb.cv

## PROBLEM 1p ##

xgboost.model = xgb.cv$finalModel
xgboost.model

xgboost.model = xgboost(data = x.train,
                        label = y.train,
                        params = list(alpha=1,
                                      colsample_bytree = 0.8,
                                      eta = 0.3,
                                      gamma = 0,
                                      lambda = 1,
                                      max_depth = 2,
                                      min_child_weight=1,
                                      subsample = .5),
                        nrounds = 50,
                        verbose = F)
  
# Make predictions on test and train sets
PredictTrain = predict(xgboost.model, x.train)
PredictTest = predict(xgboost.model, x.test)

# Calculate R-Squared
SSTTrain = sum((y.train - mean(y.train))^2)
SSETrain = sum((PredictTrain - y.train)^2)
R2_XGB <- 1 - SSETrain/SSTTrain
R2_XGB

#Calculate OSR-Squared
SSTTest = sum((y.test - mean(y.test))^2)
SSETest = sum((PredictTest - y.test)^2)
OSR2_XGB <- 1 - SSETest/SSTTest
OSR2_XGB