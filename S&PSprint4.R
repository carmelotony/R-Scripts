#In File
sp500 <- read.csv(file.choose())
head(sp500)
#Percent Changes
library(dplyr)
sp500$Volume <- as.numeric(sp500$Volume)
sp500$Adj.Close <- as.numeric(sp500$Adj.Close)
SPperc <- as.data.frame(sp500$Volume/lag(sp500$Volume,1))
SPperc$close <- sp500$Adj.Close/lag(sp500$Adj.Close,1)
SPperc$volume <- sp500$Volume/lag(sp500$Volume,1)
SPperc$`sp500$Volume/lag(sp500$Volume, 1)` = NULL
#LinReg
library(stats)
SPreg <- lm(SPperc$volume ~ SPperc$close)
summary(SPreg)
#Creat weekly block changes in price
sp500$closewk0 <- sp500$Adj.Close/lag(sp500$Adj.Close,1)
sp500$closewk1 <- sp500$Adj.Close/lag(sp500$Adj.Close,2)
sp500$closewk2 <- sp500$Adj.Close/lag(sp500$Adj.Close,3)
sp500$closewk3 <- sp500$Adj.Close/lag(sp500$Adj.Close,4)
sp500$closewk4 <- sp500$Adj.Close/lag(sp500$Adj.Close,5)
sp500$closewk5 <- sp500$Adj.Close/lag(sp500$Adj.Close,6)
#Shift week 0 cells up one row to compare future week to past
shift <- function(x, n){c(x[-(seq(n))], rep(NA, n))}
sp500$closewk0 <- shift(sp500$closewk0, 1)
#Linear Regression of weekly changes to following week
SPwkReg <- lm(sp500$closewk0 ~ sp500$closewk1 + sp500$closewk2 + sp500$closewk3 + sp500$closewk4 + sp500$closewk5)
summary(SPwkReg)
