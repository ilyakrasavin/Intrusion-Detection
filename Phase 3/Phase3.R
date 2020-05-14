library(psych)
library(modeest)
library(ggplot2)
library(dplyr)
library(depmixS4)

set.seed(1)

######################################################

# I.Data Exploration.

# Choose best combination of observed responses.
# Determine the time window on weekdays & weekends

table <- read.table("C:/Users/Pheni/Documents/318/asgn1/cmpt318-assignment1/Phase 3/Data_Assignment3.txt", header = TRUE, sep = ",")

table$Date <- as.POSIXlt(table$Date, format = "%d/%m/%Y",na.rm = TRUE)

# Weekdays Data.
table_weekday <- table[which(weekdays(as.Date(table$Date, format = "%m/%d/%Y"))
                             %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
# Extracting the chosen time interval.
table_weekday_Hours <- table_weekday[strptime(table_weekday$Time, format = "%H:%M:%S") >= 
                                       strptime("6:00:00", format = "%H:%M:%S") & 
                                       strptime(table_weekday$Time, format = "%H:%M:%S") <=
                                       strptime("10:00:00", format = "%H:%M:%S"),]

ggplot(data=table_weekday, mapping=aes(x=as.POSIXct(table_weekday$Time, format = "%H"), y=Voltage)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday$Time, format = "%H"), 1)))

ggplot(data=table_weekday, mapping=aes(x=as.POSIXct(table_weekday$Time, format = "%H"), y=Global_active_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday$Time, format = "%H"), 1)))

ggplot(data=table_weekday, mapping=aes(x=as.POSIXct(table_weekday$Time, format = "%H"), y=Global_intensity)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday$Time, format = "%H"), 1)))



# Weekends Data.
table_weekend <- table[which(weekdays(as.Date(table$Date, format = "%m/%d/%Y"))
                             %in% c('Saturday','Sunday')), ]
# Extracting the chosen time interval.
table_weekend_Hours <- table_weekend[strptime(table_weekend$Time, format = "%H:%M:%S") >= 
                                       strptime("06:00:00", format = "%H:%M:%S") & 
                                       strptime(table_weekend$Time, format = "%H:%M:%S") <=
                                       strptime("10:00:00", format = "%H:%M:%S"),]


ggplot(data=table_weekend, mapping=aes(x=as.POSIXct(table_weekend$Time, format = "%H"), y=Voltage)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekend$Time, format = "%H"), 1)))

ggplot(data=table_weekend, mapping=aes(x=as.POSIXct(table_weekend$Time, format = "%H"), y=Global_active_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekend$Time, format = "%H"), 1)))

ggplot(data=table_weekend, mapping=aes(x=as.POSIXct(table_weekend$Time, format = "%H"), y=Global_intensity)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekend$Time, format = "%H"), 1)))


######################################################



# II. Model Training.
# Train + Test multivariate HMMs with different number of states.

# PARTITION THE DATASETS INTO TRAIN AND TEST DATA.
WeekdayTrain <- subset(table_weekday_Hours, as.Date(table_weekday_Hours$Date) >= as.Date("2006-12-18") & as.Date(table_weekday_Hours$Date) <= as.Date("2008-12-18") )
ntimesWeekdayTrain <- nrow(WeekdayTrain)

WeekendTrain <- subset(table_weekend_Hours, as.Date(table_weekend_Hours$Date) >= as.Date("2006-12-18") & as.Date(table_weekend_Hours$Date) <= as.Date("2008-12-18") )
ntimesWeekendTrain <- nrow(WeekendTrain)

WeekdayTest <- subset(table_weekday_Hours, as.Date(table_weekday_Hours$Date) >= as.Date("2008-12-18") & as.Date(table_weekday_Hours$Date) <= as.Date("2009-12-01") )
ntimesWeekdayTest <- nrow(WeekdayTest)

WeekendTest <- subset(table_weekend_Hours, as.Date(table_weekend_Hours$Date) >= as.Date("2008-12-18") & as.Date(table_weekend_Hours$Date) <= as.Date("2009-12-01") )
ntimesWeekendTest <- nrow(WeekendTest)


# TRAIN AND FIT THE TRAIN MODELS for Weekday/Weekend Data.
weekdayModelTrain4 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 4, ntimes=ntimesWeekdayTrain, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTrainFit4 <- fit(weekdayModelTrain4,em=em.control(maxit = 2000))

summary(weekdayModelTrain4)
print(weekdayModelTrainFit4)

weekendModelTrain4 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTrain, nstates = 4, ntimes=ntimesWeekendTrain, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTrainFit4 <- fit(weekendModelTrain4 , em=em.control(maxit = 2000))

summary(weekendModelTrain4)
print(weekendModelTrainFit4)

weekdayModelTrain5 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 5, ntimes=ntimesWeekdayTrain, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTrainFit5 <- fit(weekdayModelTrain5,em=em.control(maxit = 2000))

summary(weekdayModelTrainFit5)
print(weekdayModelTrainFit5)

weekendModelTrain5 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTrain, nstates = 5, ntimes=ntimesWeekendTrain, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTrainFit5 <- fit(weekendModelTrain5 , em=em.control(maxit = 2000))

summary(weekendModelTrain5)
print(weekendModelTrainFit5)

weekdayModelTrain6 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 6, ntimes=ntimesWeekdayTrain, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTrainFit6 <- fit(weekdayModelTrain6,em=em.control(maxit = 2000))

summary(weekdayModelTrain6)
print(weekdayModelTrainFit6)

weekendModelTrain6 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTrain, nstates = 6, ntimes=ntimesWeekendTrain, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTrainFit6 <- fit(weekendModelTrain6 , em=em.control(maxit = 2000))

summary(weekendModelTrain6)
print(weekendModelTrainFit6)

weekdayModelTrain7 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 7, ntimes=ntimesWeekdayTrain, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTrainFit7 <- fit(weekdayModelTrain7,em=em.control(maxit = 2000))

summary(weekdayModelTrain7)
print(weekdayModelTrainFit7)

weekendModelTrain7 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTrain, nstates = 7, ntimes=ntimesWeekendTrain, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTrainFit7 <- fit(weekendModelTrain7 , em=em.control(maxit = 2000))

summary(weekendModelTrain7)
print(weekendModelTrainFit7)

weekdayModelTrain8 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 8, ntimes=ntimesWeekdayTrain, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTrainFit8 <- fit(weekdayModelTrain8,em=em.control(maxit = 2000))

summary(weekdayModelTrain8)
print(weekdayModelTrainFit8)

weekendModelTrain8 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTrain, nstates = 8, ntimes=ntimesWeekendTrain, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTrainFit8 <- fit(weekendModelTrain8 , em=em.control(maxit = 2000))

summary(weekendModelTrain8)
print(weekendModelTrainFit8)

weekdayModelTrain9 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 9, ntimes=ntimesWeekdayTrain, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTrainFit9 <- fit(weekdayModelTrain9,em=em.control(maxit = 2000))

weekendModelTrain9 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTrain, nstates = 9, ntimes=ntimesWeekendTrain, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTrainFit9 <- fit(weekendModelTrain9 , em=em.control(maxit = 2000))

summary(weekendModelTrain9)
print(weekendModelTrainFit9)

weekdayModelTrain10 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 10, ntimes=ntimesWeekdayTrain, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTrainFit10 <- fit(weekdayModelTrain10,em=em.control(maxit = 2000))

weekendModelTrain10 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTrain, nstates = 10, ntimes=ntimesWeekendTrain, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTrainFit10 <- fit(weekendModelTrain10 , em=em.control(maxit = 2000))

summary(weekendModelTrain10)
print(weekendModelTrainFit10)

weekdayModelTrain11 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 11, ntimes=ntimesWeekdayTrain, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTrainFit11 <- fit(weekdayModelTrain11,em=em.control(maxit = 2000))

weekendModelTrain11 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTrain, nstates = 11, ntimes=ntimesWeekendTrain, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTrainFit11 <- fit(weekendModelTrain11 , em=em.control(maxit = 2000))

weekdayModelTrain12 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 12, ntimes=ntimesWeekdayTrain, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTrainFit12 <- fit(weekdayModelTrain12,em=em.control(maxit = 2000))

weekendModelTrain12 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTrain, nstates = 12, ntimes=ntimesWeekendTrain, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTrainFit12 <- fit(weekendModelTrain12 , em=em.control(maxit = 2000))

weekdayModelTrain13 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 13, ntimes=ntimesWeekdayTrain, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTrainFit13 <- fit(weekdayModelTrain13,em=em.control(maxit = 2000))

weekendModelTrain13 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTrain, nstates = 13, ntimes=ntimesWeekendTrain, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTrainFit13 <- fit(weekendModelTrain13 , em=em.control(maxit = 2000))

weekdayModelTrain14 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 14, ntimes=ntimesWeekdayTrain, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTrainFit14 <- fit(weekdayModelTrain14,em=em.control(maxit = 2000))

weekendModelTrain14 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTrain, nstates = 14, ntimes=ntimesWeekendTrain, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTrainFit14 <- fit(weekendModelTrain14 , em=em.control(maxit = 2000))

weekdayModelTrain15 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 15, ntimes=ntimesWeekdayTrain, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTrainFit15 <- fit(weekdayModelTrain15,em=em.control(maxit = 2000))

weekendModelTrain15 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTrain, nstates = 15, ntimes=ntimesWeekendTrain, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTrainFit15 <- fit(weekendModelTrain15 , em=em.control(maxit = 2000))

weekdayModelTrain16 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 16, ntimes=ntimesWeekdayTrain, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTrainFit16 <- fit(weekdayModelTrain16,em=em.control(maxit = 2000))

weekendModelTrain16 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTrain, nstates = 16, ntimes=ntimesWeekendTrain, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTrainFit16 <- fit(weekendModelTrain16 , em=em.control(maxit = 2000))


# Compare models using BIC and Loc likelihood. WEEKDAY data.
plot(1:12,c(BIC(weekdayModelTrainFit4),BIC(weekdayModelTrainFit5),BIC(weekdayModelTrainFit6),BIC(weekdayModelTrainFit7),BIC(weekdayModelTrainFit8),BIC(weekdayModelTrainFit9),BIC(weekdayModelTrainFit10),BIC(weekdayModelTrainFit11), BIC(weekdayModelTrainFit12), BIC(weekdayModelTrainFit13), BIC(weekdayModelTrainFit14),BIC(weekdayModelTrainFit15), BIC(weekdayModelTrainFit16)),ty="b")
# Compare models using BIC and Loc likelihood. WEEKEND data.
plot(1:12,c(BIC(weekendModelTrainFit4),BIC(weekendModelTrainFit5),BIC(weekendModelTrainFit6),BIC(weekendModelTrainFit7),BIC(weekendModelTrainFit8),BIC(weekendModelTrainFit9),BIC(weekendModelTrainFit10),BIC(weekendModelTrainFit11), BIC(weekendModelTrainFit12), BIC(weekendModelTrainFit13), BIC(weekendModelTrainFit14),BIC(weekendModelTrainFit15), BIC(weekendModelTrainFit16)),ty="b")

BIC_VALUE_WEEKDAY <- c(1206206,1137170,1082487,1060299,1031595,1010165,1003007,1004270,966987,950668,942754,926410,917576)
BIC_VALUE_WEEKEND <- c(431441,418641,398719,385247,376460,378381,372429,366227,359080,342552,338338,336650,330119)

LOG_VALUE_WEEKDAY <- c(-602874,-568267,-540826,-529621,-515145,-504295,-500570,-501043,-482231,-473890,-469739,-456727,-456727)
LOG_VALUE_WEEKEND <- c(-215681,-209266,-199288,-192553,-187630,-188466,-185355,-182108,-178378,-169947,-167661,-166628,-163163)


plot(BIC_VALUE_WEEKDAY, type ="o", col="blue")



######################################################

# III. Model Testing.

weekdayModelTest <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekdayTrain, nstates = 11, ntimes=ntimesWeekdayTest, family = list(gaussian(), gaussian(), gaussian()))
weekdayModelTest <- setpars(weekdayModelTest, getpars(weekdayModelTrainFit11))
logLik(weekdayModelTest)

weekendModelTest <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), data = WeekendTest, nstates = 9, ntimes=ntimesWeekendTest, family = list(gaussian(), gaussian(), gaussian()))
weekendModelTest <- setpars(weekendModelTest, getpars(weekendModelTrainFit9))
logLik(weekendModelTest)









