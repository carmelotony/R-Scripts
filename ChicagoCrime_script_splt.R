#read in file, delimit by comma
crime=read.delim(file.choose(), sep=",", stringsAsFactors=FALSE, header=TRUE, na.strings="")
#Return first n rows of matrix
head(crime,n=6)
#Split Date into Date and Time
crime2 <- within(crime, Datesplt<-data.frame(do.call('rbind', strsplit(as.character(Date), ' ', fixed=TRUE))))
#Format Date
#crime3 <- as.Date(crime2$Datesplt.X1, format="%m/%d/%Y")
crime4 <- subset(crime2, Select=c(Datesplt.X1, Primary.Type))
