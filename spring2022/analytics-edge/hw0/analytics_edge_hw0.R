#-----------------------
# ANALYTICS EDGE
# HW0
# TIM MILLER
#-----------------------

#PROBLEM 2
wine <- read.csv('wine-2.csv')
mod <- lm(LogAuctionIndex ~ . -Year, data=wine)
summary(mod)
