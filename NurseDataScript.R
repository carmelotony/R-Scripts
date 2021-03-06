Defic <- read.csv(file.choose())
head(Defic)
Defic.fl <- subset(Defic, Defic$state == "FL")
head(Defic.fl)
Defic.fl$survey_date_output <- as.Date(Defic.fl$survey_date_output, format="%Y-%m-%d")
Defic.fl <- subset(Defic.fl, Defic.fl$survey_date_output >= "2014-1-1" & Defic.fl$survey_date_output <= "2015-12-31")
head(Defic.fl)
Defic.fl <- cbind(Defic.fl, ones = 1)
Defic.fl$survey_date_output <- as.yearmon(Defic.fl$survey_date_output) 
Defic.fl.441 <- subset(Defic.fl, Defic.fl$tag == "441")
Defic.fl.282 <- subset(Defic.fl, Defic.fl$tag == "282")
Defic.fl.371 <- subset(Defic.fl, Defic.fl$tag == "371")
Defic.fl.431 <- subset(Defic.fl, Defic.fl$tag == "431")
Defic.fl.253 <- subset(Defic.fl, Defic.fl$tag == "253")
Defic.fl.309 <- subset(Defic.fl, Defic.fl$tag == "309")

Defic.fl.441.count <- aggregate(Defic.fl.441$ones, list(Defic.fl.441$survey_date_output), FUN = sum)
Defic.fl.282.count <- aggregate(Defic.fl.282$ones, list(Defic.fl.282$survey_date_output), FUN = sum)
Defic.fl.309.count <- aggregate(Defic.fl.309$ones, list(Defic.fl.309$survey_date_output), FUN = sum)
Defic.fl.371.count <- aggregate(Defic.fl.371$ones, list(Defic.fl.371$survey_date_output), FUN = sum)
Defic.fl.431.count <- aggregate(Defic.fl.431$ones, list(Defic.fl.431$survey_date_output), FUN = sum)


SeaFC.441 <- msts(Defic.fl.441.count, seasonal.periods=12)
fit.441 <- tbats(SeaFC.441)
fc.441 <- forecast(fit.441)
plot(fc)
