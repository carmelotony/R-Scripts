#load Crime Data and convert Factors to Strings
crime <- read.delim(file.choose(), sep = ",", stringsAsFactors = FALSE,na.strings = "")
#review first n row of Data
head(crime, n=6)
#Format Date from String to Date
crime$Date <- as.POSIXct(crime$Date, format="%m/%d/%Y %H:%M:%S %p")
#Creat new Dataframe from Subset of Crime Columns
crime.short <- data.frame(crime$Date, crime$Primary.Type)
#Create new Dataframe in the Date range
crime.datrang <- subset(crime.short, crime$Date>= "2004-1-1" & crime$Date<= "2013-12-31")
crime.actual <- subset(crime.short, crime$Date>= "2014-1-1" & crime$Date<= "2014-6-30")
#Create new Dataframe with Primary Type Theft
crime.theft <- subset(crime.datrang, crime.datrang$crime.Primary.Type == "THEFT")
crime.theft.act <- subset(crime.actual, crime.actual$crime.Primary.Type == "THEFT")
#Create new Dataframe with Primary Type Battery
crime.bat <- subset(crime.datrang, crime.datrang$crime.Primary.Type == "BATTERY")
crime.bat.act <- subset(crime.actual, crime.actual$crime.Primary.Type == "BATTERY")
#Create new DAtaframe with Primary Type Narcotics
crime.narc <- subset(crime.datrang, crime.datrang$crime.Primary.Type == "NARCOTICS")
crime.narc.act <- subset(crime.actual, crime.actual$crime.Primary.Type == "NARCOTICS")
#reverse Time Low to High
crime.bat$crime.Date <- rev(crime.bat$crime.Date)
crime.theft$crime.Date <- rev(crime.theft$crime.Date)
crime.narc$crime.Date <- rev(crime.narc$crime.Date)
crime.bat.act$crime.Date <- rev(crime.bat.act$crime.Date)
crime.theft.act$crime.Date <- rev(crime.theft.act$crime.Date)
crime.narc.act$crime.Date <- rev(crime.narc.act$crime.Date)
#Convert to Month Year
crime.bat$crime.Date <- as.yearmon(crime.bat$crime.Date)
crime.theft$crime.Date <- as.yearmon(crime.theft$crime.Date)
crime.narc$crime.Date <- as.yearmon(crime.narc$crime.Date)
crime.bat.act$crime.Date <- as.yearmon(crime.bat.act$crime.Date)
crime.theft.act$crime.Date <- as.yearmon(crime.theft.act$crime.Date)
crime.narc.act$crime.Date <- as.yearmon(crime.narc.act$crime.Date)
#Add count column
crime.bat <- cbind(crime.bat, ones = 1)
crime.theft <- cbind(crime.theft, ones = 1)
crime.narc <- cbind(crime.narc, ones = 1)
crime.bat.act <- cbind(crime.bat.act, ones = 1)
crime.theft.act <- cbind(crime.theft.act, ones = 1)
crime.narc.act <- cbind(crime.narc.act, ones = 1)
#Count events per Month Year
crime.bat.count <- aggregate(crime.bat$ones, list(crime.bat$crime.Date), FUN = sum)
crime.theft.count <- aggregate(crime.theft$ones, list(crime.theft$crime.Date), FUN = sum)
crime.narc.count <- aggregate(crime.narc$ones, list(crime.narc$crime.Date), FUN = sum)
crime.bat.count.act <- aggregate(crime.bat.act$ones, list(crime.bat.act$crime.Date), FUN = sum)
crime.theft.count.act <- aggregate(crime.theft.act$ones, list(crime.theft.act$crime.Date), FUN = sum)
crime.narc.count.act <- aggregate(crime.narc.act$ones, list(crime.narc.act$crime.Date), FUN = sum)
#Add Period column
crime.bat.count <- cbind(crime.bat.count, "Period"=1:nrow(crime.bat.count))
crime.theft.count <- cbind(crime.theft.count, "Period"=1:nrow(crime.theft.count))
crime.narc.count <- cbind(crime.narc.count, "Period"=1:nrow(crime.narc.count))
#Linear Reg
lm(crime.bat.count$x ~ crime.bat.count$Period)
lm(crime.theft.count$x ~ crime.theft.count$Period)
lm(crime.narc.count$x ~  crime.narc.count$Period)
#HoltWinter
TheftHW <- ts(crime.theft.count, start = c(1,1), frequency = 120)
plot(TheftHW)
TheftMod <-HoltWinters(TheftHW)
predict(TheftMod, n.ahead = 6)
BatHW <- ts(crime.bat.count, start = c(1,1), frequency = 120)
plot(BatHW)
BatMod <- HoltWinters(BatHW)
predict(BatMod, n.ahead = 6)
NarcHW <- ts(crime.narc.count, start = c(1,1), frequency = 120)
plot(NarcHW)
NarcMod <- HoltWinters(NarcHW)
predict(NarcMod, n.ahead = 6)
?HoltWinters
