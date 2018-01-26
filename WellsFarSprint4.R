#In File
Concomp <- read.csv(file.choose())
head(Concomp)
#Subset Wells Fargo
Concomp <- subset(Concomp, Concomp$Company == "Wells Fargo & Company")
head(Concomp)
Concomp$Date.received <- as.Date(Concomp$Date.received, format="%m/%d/%Y")
head(Concomp)
#Aggregate by Month
library(zoo)
Concomp$Date.received <- as.yearmon(Concomp$Date.received)
head(Concomp)
Concomp <- cbind(Concomp, ones = 1)
head(Concomp)
Concomp.agg <- aggregate(Concomp$ones, list(Concomp$Date.received), FUN= sum)
head(Concomp.agg)
# Upload Historical Prices
WellsPrc <- read.csv(file.choose())
head(WellsPrc)
# create Merged Data Frame
Concomp.agg$Price <- WellsPrc$Close
head(Concomp.agg)
library(reshape)
Concomp.agg <- rename(Concomp.agg, c(x="Complaints"))
head(Concomp.agg)
Concomp.agg$Group.1 <- NULL
head(Concomp.agg)
Concomp.agg$Period <- seq.int(nrow(Concomp.agg))
head(Concomp.agg)
#LinReg
library(stats)
LinRegWells <- lm(Concomp.agg$Complaints~Concomp.agg$Price)
summary(LinRegWells)
cor(Concomp.agg$Complaints, Concomp.agg$Price)
