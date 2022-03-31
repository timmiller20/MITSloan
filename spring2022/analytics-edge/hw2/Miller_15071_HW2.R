#-----------------------
# ANALYTICS EDGE
# HW2
# TIM MILLER
#-----------------------

#Load packages
library(caTools)
library(caret)
library(ROCR)
library(ggplot2)
library(dplyr)
library(rpart.plot)

# Load data
data = read.csv("framingham.csv")

# Factor variables
data$TenYearCHD <- factor(data$TenYearCHD) 
data$male <- factor(data$male) 
data$currentSmoker <- factor(data$currentSmoker) 
data$BPMeds <- factor(data$BPMeds)
data$prevalentHyp <- factor(data$prevalentHyp) 
data$diabetes <- factor(data$diabetes) 
data$prevalentStroke <- factor(data$prevalentStroke)

# Train/test
set.seed(27)
N <- nrow(data)
idx = sample.split(data$TenYearCHD, 0.7) 
train <- data[idx,]
test = data[!idx,]

# Check!
mean(train$age)

####### PROBLEM 1a #######
mod <- glm(data=train, TenYearCHD ~ ., family='binomial')
summary(mod)

####### PROBLEM 1b #######

# Make two copies of the fourth row:
fourth.patient.copies = data[c(4,4),]

# Change data
fourth.patient.copies$currentSmoker[2] = 0

# Make predictions for both patients:
predict(mod, newdata=fourth.patient.copies, type = "response")

####### PROBLEM 1d #######
pred <- predict(mod, test, type = "response")
rocr.pred = prediction(pred, test$TenYearCHD)
plot(performance(rocr.pred, "tpr", "fpr"))

AUC = as.numeric(performance(rocr.pred, "auc")@y.values)
AUC

####### PROBLEM 2b #######

# PREDICTION

# Create new prediction column
test$probs = predict(mod, newdata=test, type="response")

# Cutoff
cutoff = 10000 / (190000 - 200000/3 + 10000/3)
cutoff

#Re-create a "user friendly" version of the 'outcome' column
test = mutate(test, actual_outcome = ifelse(TenYearCHD == 1,"CHD","NoCHD"))

# Prediction measure
test = mutate(test, prediction = ifelse(probs >= cutoff, "Medication","NoMedication"))

# view mutated dataframe
head(test)
tail(test)

# confusion matrix
confusion_matrix = table(test$prediction, test$actual_outcome)

# Take a look
confusion_matrix

# ACCURACY
accuracy = (confusion_matrix[1,1] + confusion_matrix[2,2]) / nrow(test)
accuracy


# TPR
TPR = confusion_matrix[2,2] / (confusion_matrix[1,2] + confusion_matrix[2,2]) 
TPR

# FPR

FPR = confusion_matrix[2,1] / (confusion_matrix[2,1] + confusion_matrix[2,2]) 
FPR

####### PROBLEM 3a #######

# Tree cross-validation
library(caret)
set.seed(54)
cv.trees = train(y = train$TenYearCHD,
                 x = subset(train, select=-c(TenYearCHD) ),
                 method = "rpart", 
                 trControl = trainControl(method = "cv", number = 10), 
                 tuneGrid = data.frame(.cp = seq(.005,.01,.00005)))
# Extract results
cv.results = cv.trees$results

plot(cv.trees$results$cp,cv.trees$results$Accuracy)
cv.trees$results$cp[cv.trees$results$Accuracy>=max(cv.trees$results$Accuracy)]
cv.trees$bestTune

cv.results[c(100,1),]

cv.trees$results$Accuracy>=max(cv.trees$results$Accuracy)
max(cv.trees$results$Accuracy)

####### PROBLEM 3b #######

CHD.mod = rpart(TenYearCHD~., data = train, cp=0.006)
prp(CHD.mod, digits=2, extra=107, type=2)

####### PROBLEM 3e #######
pred.CHD = prediction(predict(CHD.mod, type = "prob"), test$TenYearCHD)

rocr.class = prediction(pred.CHD, test$TenYearCHD)
plot(performance(rocr.pred.class, "tpr", "fpr"))

AUC = as.numeric(performance(rocr.pred, "auc")@y.values)
AUC

