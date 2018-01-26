BankData <- read.csv(file.choose())
plot(Salary~JobGrade, data=BankData)

BankData$Gender <- as.factor(BankData$Gender)
head(BankData)

BankData$Experience <- 95-BankData$YrHired + BankData$YrsPrior

BankData$EducLev <- as.factor(BankData$EducLev)
BankData$JobGrade <- as.factor(BankData$JobGrade)
BankData$PCJob <- as.factor(BankData$PCJob)

Regr <- lm(BankData$Salary ~ BankData$EducLev + BankData$JobGrade + BankData$Gender + BankData$PCJob + BankData$Experience)
summary(Regr)

plot(BankData$Salary ~ Regr$fitted.values)
