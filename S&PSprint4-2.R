#In File
sp500 <- read.csv(file.choose())
head(sp500)
#Percent Changes
library(dplyr)
sp500$Volume <- as.numeric(sp500$Volume)
sp500$Adj.Close <- as.numeric(sp500$Adj.Close)
SPperc <- as.data.frame(sp500$Volume/lag(sp500$Volume,1)-1)
SPperc$close <- sp500$Adj.Close/lag(sp500$Adj.Close,1)-1
SPperc$volume <- sp500$Volume/lag(sp500$Volume,1)-1
SPperc$`sp500$Volume/lag(sp500$Volume, 1) - 1` = NULL
#Binary Up or Down change
SPperc$Up <- ifelse(SPperc$close>0,1,0)
#create Test and Train sets
SPtrain <- SPperc[1:939,]
SPtest <- SPperc[940:1878,]
# RUN Logistic Reg
LogRegSP <- glm(Up~., data = SPtrain)
summary(LogRegSP)
#misclassification error
LogRegFit <- predict(LogRegSP, subset(SPtest, select=c(1:3)), type="response")
head(LogRegFit)
LogRegFit <- ifelse(LogRegFit>= 0.5,1,0)
head(LogRegFit)
head(SPtest)
MisClassErr <- mean(LogRegFit != SPtest)
1-MisClassErr
#Naive Bayes
library(MASS)
library(klaR)
library(lattice)
library(ggplot2)
library(caret)
SPtest$Up <- as.factor(SPtest$Up)
SPtrain$Up <- as.factor(SPtrain$Up)
NavBaySP <- NaiveBayes(Up~., data = SPtrain)
summary(NavBaySP)
NavBayFit <- predict(NavBaySP, SPtest)
confusionMatrix(SPtest$Up, NavBayFit$class)
