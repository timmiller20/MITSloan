#-----------------------
# ANALYTICS EDGE
# HW3
# TIM MILLER
#-----------------------

## PROBLEM #1a ##

library(randomForest)
library(rpart.plot)

# Read data
ames = read.csv("AmesExtended.csv", stringsAsFactors = TRUE)
ames$SalePrice = as.numeric(ames$SalePrice)

# Train/test
library(caret)
RNGkind(sample.kind = "Rejection")
set.seed(187)
idx = createDataPartition(ames$SalePrice, p = 0.70, list = FALSE)
train = ames[idx,]
test = ames[-idx,]
mean(train$SalePrice) #=177804.3

#linear regression
p1a.mod = lm(data=train, SalePrice ~ .)
summary(p1a.mod)


## PROBLEM #1b ##

# CART cross-validation
set.seed(932)
cv.trees = train(y = train$SalePrice,
                 x = subset(train, select=-c(SalePrice) ),
                 method = "rpart", 
                 trControl = trainControl(method = "cv", number = 10), 
                 tuneGrid = data.frame(.cp = seq(.00001,.0003,.000001)))

#plot cp vs RMSE
par(mar = c(4,4,1,1))
plot(cv.trees$results$cp, cv.trees$results$RMSE, type = "l", ylab = "RMSE", xlab = "cp") # line plot

#find min RMSE
cv.results[which.min(cv.results$RMSE),]

#train model
cart.mod = rpart(SalePrice ~ ., data = train, cp=0.000296)
prp(cart.mod)

#find important variables
CART_importance_scores = cart.mod$variable.importance
n_variables = 20 # how many variables to display?
barplot( tail( sort(CART_importance_scores), n_variables ),
         beside = TRUE,
         horiz = TRUE,
         las=1,
         main = paste("CART - top", n_variables, "importance scores"),
         cex.names =.7)


## PROBLEM #1c ##

# RF OOB
set.seed(527)
train.rf.oob <- train(y = train$SalePrice,
                      x = subset(train, select=-c(SalePrice)),
                      method="rf",
                      ntree=500, nodesize=25,
                      tuneGrid=data.frame(mtry=seq(20,30,2)),
                      trControl=trainControl(method="oob") )

plot(train.rf.oob$results$mtry, train.rf.oob$results$RMSE, type = "l", ylab = "RMSE", xlab = "mtry")

#find min RMSE
train.rf.oob$results[which.min(train.rf.oob$results$RMSE),]

## PROBLEM #1d ##

# RF variable importance

#extract best model
mod.rf = train.rf.oob$finalModel

RF_importance_scores = mod.rf$importance[,1]
n_variables = 20 # how many variables to display?
barplot( tail( sort(RF_importance_scores), n_variables ),
         beside = TRUE,
         horiz = TRUE,
         las=1,
         main = paste("Random Forest - top", n_variables, "importance scores"),
         cex.names =.7)

## PROBLEM #1e ##

# Variable importance comparison
all_variables = names(CART_importance_scores)
# Make a blank plot by setting col=0
plot(CART_importance_scores[all_variables], RF_importance_scores[all_variables], col=0,
     main = "CART vs RF importance scores",
     xlab="CART", ylab="RF")
# Label variables on the plot
text(CART_importance_scores[all_variables], RF_importance_scores[all_variables], all_variables, cex=.55)




## PROBLEM 1F ##


#LINEAR REGRESSION

#CART

# Make predictions on test and train sets
PredictTrain = predict(p1a.mod, newdata = train)
PredictTest = predict(p1a.mod, newdata = test)

# Calculate R-Squared and OSR-Squared
SSTTrain = sum((train$SalePrice - mean(train$SalePrice))^2)
SSETrain = sum((PredictTrain - train$SalePrice)^2)
R2_LR <- 1 - SSETrain/SSTTrain
SSTTest = sum((test$SalePrice - mean(test$SalePrice))^2)
SSETest = sum((PredictTest - test$SalePrice)^2)
OSR2_LR <- 1 - SSETest/SSTTest

R2_LR
OSR2_LR


#CART

# Make predictions on test and train sets
PredictTrain = predict(cart.mod, newdata = train)
PredictTest = predict(cart.mod, newdata = test)

# Calculate R-Squared and OSR-Squared
SSTTrain = sum((train$SalePrice - mean(train$SalePrice))^2)
SSETrain = sum((PredictTrain - train$SalePrice)^2)
R2_CART <- 1 - SSETrain/SSTTrain
SSTTest = sum((test$SalePrice - mean(test$SalePrice))^2)
SSETest = sum((PredictTest - test$SalePrice)^2)
OSR2_CART <- 1 - SSETest/SSTTest

R2_CART
OSR2_CART


#RANDOM FOREST

# Make predictions on test and train sets
PredictTrain = predict(mod.rf, newdata = train)
PredictTest = predict(mod.rf, newdata = test)

# Calculate R-Squared and OSR-Squared
SSTTrain = sum((train$SalePrice - mean(train$SalePrice))^2)
SSETrain = sum((PredictTrain - train$SalePrice)^2)
R2_RF <- 1 - SSETrain/SSTTrain
SSTTest = sum((test$SalePrice - mean(test$SalePrice))^2)
SSETest = sum((PredictTest - test$SalePrice)^2)
OSR2_RF <- 1 - SSETest/SSTTest

R2_RF
OSR2_RF





## PROBLEM 2a ##

trainA = train[,c("SalePrice", "ExterQual","KitchenQual","HeatingQC")]
str(trainA)

trainB = train[,c("SalePrice", "ExterQual","KitchenQual","HeatingQC")]
# Ordering, from worst to best 
ord.levels = c("Po","Fa","TA","Gd","Ex")

trainB = train[,c("SalePrice", "ExterQual","KitchenQual","HeatingQC")]
trainB$ExterQual = factor(trainB$ExterQual, ordered = TRUE, levels = ord.levels)
trainB$KitchenQual = factor(trainB$KitchenQual, ordered = TRUE, levels = ord.levels)
trainB$HeatingQC = factor(trainB$HeatingQC, ordered = TRUE, levels = ord.levels)
str(trainB)


sort(trainB$ExterQual)
trainB$ExterQual
sort(trainA$ExterQual)

## PROBLEM 2b ##


treeA= rpart(SalePrice ~ ., data = trainA, control = rpart.control(cp=0.001, maxdepth=3))
treeB= rpart(SalePrice ~ ., data = trainB, control = rpart.control(cp=0.001, maxdepth=3))

par(mfrow=c(2,1))
prp(treeA, type=4, extra=1, yesno.yshift=1)
mtext("A: Unordered factors", side = 3, adj=0)
prp(treeB,  type=4, extra=1, yesno.yshift=1)
mtext("B: Ordered factors", side = 3, adj=0)


## PROBLEM 2c ##

# Make predictions on test and train sets
PredictTrain = predict(treeA, newdata = trainA)

# Calculate R-Squared and OSR-Squared
SSTTrain = sum((trainA$SalePrice - mean(trainA$SalePrice))^2)
SSETrain = sum((PredictTrain - trainA$SalePrice)^2)
R2_RF_TREEA <- 1 - SSETrain/SSTTrain
R2_RF_TREEA


# Make predictions on test and train sets
PredictTrain = predict(treeB, newdata = trainB)

# Calculate R-Squared and OSR-Squared
SSTTrain = sum((trainB$SalePrice - mean(trainB$SalePrice))^2)
SSETrain = sum((PredictTrain - trainB$SalePrice)^2)
R2_RF_TREEB <- 1 - SSETrain/SSTTrain
R2_RF_TREEB


##PROBLEM 2D##

# Create test sets
testA = test[,c("SalePrice", "ExterQual","KitchenQual","HeatingQC")]

ord.levels = c("Po","Fa","TA","Gd","Ex")
testB = test[,c("SalePrice", "ExterQual","KitchenQual","HeatingQC")]
testB$ExterQual = factor(testB$ExterQual, ordered = TRUE, levels = ord.levels)
testB$KitchenQual = factor(testB$KitchenQual, ordered = TRUE, levels = ord.levels)
testB$HeatingQC = factor(testB$HeatingQC, ordered = TRUE, levels = ord.levels)





# Calculate R-Squared and OSR-Squared
PredictTest = predict(treeA, newdata = testA)
SSTTest = sum((testA$SalePrice - mean(testA$SalePrice))^2)
SSETest = sum((PredictTest - testA$SalePrice)^2)
OSR2_RF <- 1 - SSETest/SSTTest

OSR2_RFA


PredictTest = predict(treeB, newdata = testB)
SSTTest = sum((testB$SalePrice - mean(testB$SalePrice))^2)
SSETest = sum((PredictTest - testB$SalePrice)^2)
OSR2_RF <- 1 - SSETest/SSTTest

OSR2_RFB





