library(psych)
library(modeest)
library(ggplot2)
library(dplyr)
library(depmixS4)
library(corrplot)
library(zoo)

set.seed(1)
table <- read.table("C:/Users/Pheni/Documents/318/proj/cmpt318groupproject/TrainData.txt", header = TRUE, sep = ",")

#table <- read.table("D:/Documents/SFU/SPRING 2020/CMPT318/project/cmpt318groupproject/TrainData.txt", header = TRUE, sep = ",")
table$Date <- as.POSIXlt(table$Date, format = "%d/%m/%Y")

#table <- subset(table, as.Date(table$Date) >= as.Date("2007-03-18") & as.Date(table$Date) <= as.Date("2007-03-24") )



### Part 1

GAPmean = mean(table$Global_active_power, na.rm = TRUE)
GRPmean = mean(table$Global_reactive_power, na.rm = TRUE)
VOLmean = mean(table$Voltage, na.rm = TRUE)

GAPGEOmean = geometric.mean(table$Global_active_power, na.rm = TRUE)
GRPGEOmean = geometric.mean(table$Global_reactive_power, na.rm = TRUE)
VOLGEOmean = geometric.mean(table$Voltage, na.rm = TRUE)

GAPmed = median(table$Global_active_power, na.rm = TRUE)
GRPmed = median(table$Global_reactive_power, na.rm = TRUE)
VOLmed = median(table$Voltage, na.rm = TRUE)

GAPMode = mfv(table$Global_active_power)
GRPMode = mfv(table$Global_reactive_power)
VOLMode = mfv(table$Voltage)

GAPsd = sd(table$Global_active_power, na.rm = TRUE)
GRPsd = sd(table$Global_reactive_power, na.rm = TRUE)
VOLsd = sd(table$Voltage, na.rm = TRUE)

GAP = c(GAPmean,GAPGEOmean, GAPmed, GAPMode, GAPsd)
GRP = c(GRPmean,GRPGEOmean, GRPmed, GRPMode, GRPsd)
VOL = c(VOLmean,VOLGEOmean, VOLmed, VOLMode, VOLsd)

df = data.frame(GAP, GRP, VOL)

table_omit <- na.omit(table, cols=c("Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3"))
correlations <- cor(table_omit[3:9], method = "pearson")
disjoint_correlations <- correlation
corr <- round(cor(disjoint_correlations), 4)
ggcorrplot(corr)


ggplot(data=table_omit, mapping=aes(x=as.POSIXct(table_omit$Time, format = "%H"), y=Global_active_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_omit$Time, format = "%H"), 1)))

ggplot(data=table_omit, mapping=aes(x=as.POSIXct(table_omit$Time, format = "%H"), y=Global_reactive_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_omit$Time, format = "%H"), 1)))

ggplot(data=table_omit, mapping=aes(x=as.POSIXct(table_omit$Time, format = "%H"), y=Voltage)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_omit$Time, format = "%H"), 1)))

ggplot(data=table_omit, mapping=aes(x=as.POSIXct(table_omit$Time, format = "%H"), y=Global_intensity)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_omit$Time, format = "%H"), 1)))

ggplot(data=table_omit, mapping=aes(x=as.POSIXct(table_omit$Time, format = "%H"), y=Sub_metering_1)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_omit$Time, format = "%H"), 1)))

ggplot(data=table_omit, mapping=aes(x=as.POSIXct(table_omit$Time, format = "%H"), y=Sub_metering_2)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_omit$Time, format = "%H"), 1)))

ggplot(data=table_omit, mapping=aes(x=as.POSIXct(table_omit$Time, format = "%H"), y=Sub_metering_3)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_omit$Time, format = "%H"), 1)))


table_weekday_monday <- table_omit[which(weekdays(as.Date(table_omit$Date, format = "%m/%d/%Y"))
                                         %in% c('Monday')), ]
table_weekday_tuesday <- table_omit[which(weekdays(as.Date(table_omit$Date, format = "%m/%d/%Y"))
                                          %in% c('Tuesday')),  ]
table_weekday_wednesday <- table_omit[which(weekdays(as.Date(table_omit$Date, format = "%m/%d/%Y"))
                                            %in% c('Wednesday')), ]
table_weekday_thursday <- table_omit[which(weekdays(as.Date(table_omit$Date, format = "%m/%d/%Y"))
                                           %in% c('Thursday')),  ]
table_weekday_friday <- table_omit[which(weekdays(as.Date(table_omit$Date, format = "%m/%d/%Y"))
                                         %in% c('Friday')),  ]
table_weekend_saturday <- table_omit[which(weekdays(as.Date(table_omit$Date, format = "%m/%d/%Y"))
                                           %in% c('Saturday')),]
table_weekend_sunday <- table_omit[which(weekdays(as.Date(table_omit$Date, format = "%m/%d/%Y"))
                                         %in% c('Sunday')),]

# Plotting Monday for weekday and Saturday for weekend
ggplot(data=table_weekday_monday, mapping=aes(x=as.POSIXct(table_weekday_monday$Time, format = "%H"), y=Global_active_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_monday$Time, format = "%H"), 1)))
ggplot(data=table_weekday_monday, mapping=aes(x=as.POSIXct(table_weekday_monday$Time, format = "%H"), y=Voltage)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_monday$Time, format = "%H"), 1)))
ggplot(data=table_weekday_monday, mapping=aes(x=as.POSIXct(table_weekday_monday$Time, format = "%H"), y=Global_reactive_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_monday$Time, format = "%H"), 1)))
ggplot(data=table_weekday_monday, mapping=aes(x=as.POSIXct(table_weekday_monday$Time, format = "%H"), y=Global_reactive_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_monday$Time, format = "%H"), 1)))

ggplot(data=table_weekend_saturday, mapping=aes(x=as.POSIXct(table_weekend_saturday$Time, format = "%H"), y=Global_active_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekend_saturday$Time, format = "%H"), 1)))
ggplot(data=table_weekend_saturday, mapping=aes(x=as.POSIXct(table_weekend_saturday$Time, format = "%H"), y=Voltage)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekend_saturday$Time, format = "%H"), 1)))
ggplot(data=table_weekend_saturday, mapping=aes(x=as.POSIXct(table_weekend_saturday$Time, format = "%H"), y=Global_reactive_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekend_saturday$Time, format = "%H"), 1)))


#Plot Tuesday
ggplot(data=table_weekday_tuesday, mapping=aes(x=as.POSIXct(table_weekday_tuesday$Time, format = "%H"), y=Global_active_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_tuesday$Time, format = "%H"), 1)))

#Plot Wednesday
ggplot(data=table_weekday_wednesday, mapping=aes(x=as.POSIXct(table_weekday_wednesday$Time, format = "%H"), y=Global_active_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_wednesday$Time, format = "%H"), 1)))

#Plot Thursday
ggplot(data=table_weekday_thursday, mapping=aes(x=as.POSIXct(table_weekday_thursday$Time, format = "%H"), y=Global_active_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_thursday$Time, format = "%H"), 1)))

#Plot Friday
ggplot(data=table_weekday_friday, mapping=aes(x=as.POSIXct(table_weekday_friday$Time, format = "%H"), y=Global_active_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_friday$Time, format = "%H"), 1)))

#Plot Sunday
ggplot(data=table_weekend_sunday, mapping=aes(x=as.POSIXct(table_weekend_sunday$Time, format = "%H"), y=Global_active_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekend_sunday$Time, format = "%H"), 1)))


### Polynomial regression
poly_regression_degree <- 8
table_weekday_monday_GA_mean = table_weekday_monday %>%
  group_by(Time) %>%
  summarise(GAP_mean=(mean(Global_active_power)))
GAP_fit_polynomial_monday <- lm(formula=GAP_mean~poly(as.numeric(table_weekday_monday_GA_mean$Time),poly_regression_degree,raw=TRUE),data=table_weekday_monday_GA_mean)
prediction_polynomial_weekday_monday <- predict(GAP_fit_polynomial_monday, data=table_weekday_monday_GA_mean$Time)
table_weekday_monday_GA_mean$prediction_polynomial <- prediction_polynomial_weekday_monday

table_weekday_tuesday_GA_mean = table_weekday_tuesday %>%
  group_by(Time) %>%
  summarise(GAP_mean=(mean(Global_active_power)))
GAP_fit_polynomial_tuesday <- lm(formula=GAP_mean~poly(as.numeric(table_weekday_tuesday_GA_mean$Time),poly_regression_degree,raw=TRUE),data=table_weekday_tuesday_GA_mean)
prediction_polynomial_weekday_tuesday <- predict(GAP_fit_polynomial_tuesday, data=table_weekday_tuesday_GA_mean$Time)
table_weekday_tuesday_GA_mean$prediction_polynomial <- prediction_polynomial_weekday_tuesday

table_weekday_wednesday_GA_mean = table_weekday_wednesday %>%
  group_by(Time) %>%
  summarise(GAP_mean=(mean(Global_active_power)))
GAP_fit_polynomial_wednesday <- lm(formula=GAP_mean~poly(as.numeric(table_weekday_wednesday_GA_mean$Time),poly_regression_degree,raw=TRUE),data=table_weekday_wednesday_GA_mean)
prediction_polynomial_weekday_wednesday <- predict(GAP_fit_polynomial_wednesday, data=table_weekday_wednesday_GA_mean$Time)
table_weekday_wednesday_GA_mean$prediction_polynomial <- prediction_polynomial_weekday_wednesday

table_weekday_thursday_GA_mean = table_weekday_thursday %>%
  group_by(Time) %>%
  summarise(GAP_mean=(mean(Global_active_power)))
GAP_fit_polynomial_thursday <- lm(formula=GAP_mean~poly(as.numeric(table_weekday_thursday_GA_mean$Time),poly_regression_degree,raw=TRUE),data=table_weekday_thursday_GA_mean)
prediction_polynomial_weekday_thursday <- predict(GAP_fit_polynomial_thursday, data=table_weekday_thursday_GA_mean$Time)
table_weekday_thursday_GA_mean$prediction_polynomial <- prediction_polynomial_weekday_thursday

table_weekday_friday_GA_mean = table_weekday_friday %>%
  group_by(Time) %>%
  summarise(GAP_mean=(mean(Global_active_power)))
GAP_fit_polynomial_friday <- lm(formula=GAP_mean~poly(as.numeric(table_weekday_friday_GA_mean$Time),poly_regression_degree,raw=TRUE),data=table_weekday_friday_GA_mean)
prediction_polynomial_weekday_friday <- predict(GAP_fit_polynomial_friday, data=table_weekday_friday_GA_mean$Time)
table_weekday_friday_GA_mean$prediction_polynomial <- prediction_polynomial_weekday_friday

ggplot(mapping=aes(x=Time, y=GAP_mean)) +
  geom_line(data=table_weekday_monday_GA_mean, mapping=aes(x=Time, y=prediction_polynomial_weekday_monday,group=1,color="red"), size=1) +
  scale_color_discrete(name = "Weekends", labels = c("Saturday","Sunday"))

### weekends
table_weekend_saturday_GA_mean = table_weekend_saturday %>%
  group_by(Time) %>%
  summarise(GAP_mean=(mean(Global_active_power)))
GAP_fit_polynomial_saturday <- lm(formula=GAP_mean~poly(as.numeric(table_weekend_saturday_GA_mean$Time),poly_regression_degree,raw=TRUE),data=table_weekend_saturday_GA_mean)
prediction_polynomial_weekend_saturday <- predict(GAP_fit_polynomial_saturday, data=table_weekend_saturday_GA_mean$Time)
table_weekend_saturday_GA_mean$prediction_polynomial <- prediction_polynomial_weekend_saturday

table_weekend_sunday_GA_mean = table_weekend_sunday %>%
  group_by(Time) %>%
  summarise(GAP_mean=(mean(Global_active_power)))
GAP_fit_polynomial_sunday <- lm(formula=GAP_mean~poly(as.numeric(table_weekend_sunday_GA_mean$Time),poly_regression_degree,raw=TRUE),data=table_weekend_sunday_GA_mean)
prediction_polynomial_weekend_sunday <- predict(GAP_fit_polynomial_sunday, data=table_weekend_sunday_GA_mean$Time)
table_weekend_sunday_GA_mean$prediction_polynomial <- prediction_polynomial_weekend_sunday

ggplot(mapping=aes(x=Time, y=GAP_mean)) +
  geom_line(data=table_weekday_monday_GA_mean, mapping=aes(x=as.POSIXct(table_omit$Time, format = "%H"), y=prediction_polynomial_weekday_saturday,group=1,color="red"), size=1, ) +
  geom_line(data=table_weekday_monday_GA_mean, mapping=aes(x=as.POSIXct(table_omit$Time, format = "%H"), y=prediction_polynomial_weekday_sunday,group=1,color="blue"), size=1) +
  scale_color_discrete(name = "Weekends", labels = c("Saturday","Sunday"))




### Part 2

table_weekday_monday_morning <- table_weekday_monday[strptime(table_weekday_monday$Time, format = "%H:%M:%S") >= 
                                                            strptime("07:00:00", format = "%H:%M:%S") & 
                                                            strptime(table_weekday_monday$Time, format = "%H:%M:%S") <=
                                                            strptime("15:00:00", format = "%H:%M:%S"),]
table_weekday_monday_morning_train <- subset(table, as.Date(table$Date) >= as.Date("2006-12-16") & as.Date(table$Date) <= as.Date("2007-12-31") )


table_weekend_saturday_morning <- table_weekend_saturday[strptime(table_weekend_saturday$Time, format = "%H:%M:%S") >= 
                                                            strptime("07:00:00", format = "%H:%M:%S") & 
                                                            strptime(table_weekend_saturday$Time, format = "%H:%M:%S") <=
                                                            strptime("15:00:00", format = "%H:%M:%S"),]
table_weekend_saturday_morning_train <- subset(table, as.Date(table$Date) >= as.Date("2006-12-16") & as.Date(table$Date) <= as.Date("2007-12-31") )

## Univariant HMM

# Weekday
mod4monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 4)
fm4monday <- fit(mod4monday)
mod5monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 5)
fm5monday <- fit(mod5monday)
mod6monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 6)
fm6monday <- fit(mod6monday)
mod7monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 7)
fm7monday <- fit(mod7monday)
mod8monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 8)
fm8monday <- fit(mod8monday)
mod9monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 9)
fm9monday <- fit(mod9monday)
mod10monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 10)
fm10monday <- fit(mod10monday)
mod11monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 11)
fm11monday <- fit(mod11monday)
mod12monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 12)
fm12monday <- fit(mod12monday)
mod13monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 13)
fm13monday <- fit(mod13monday)
mod14monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 14)
fm14monday <- fit(mod14monday)
mod15monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 15)
fm15monday <- fit(mod15monday)
mod16monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 16)
fm16monday <- fit(mod16monday)
mod17monday <- depmix(response = Global_active_power ~ 1, data = table_weekday_monday_morning_train, nstates = 17)
fm17monday <- fit(mod17monday)


summary(fm4monday)
print(fm4monday)
summary(fm5monday)
print(fm5monday)
summary(fm6monday)
print(fm6monday)
summary(fm7monday)
print(fm7monday)
summary(fm8monday)
print(fm8monday)
summary(fm9monday)
print(fm9monday)
summary(fm10monday)
print(fm10monday)
summary(fm11monday)
print(fm11monday)
summary(fm12monday)
print(fm12monday)
summary(fm13monday)
print(fm13monday)
summary(fm14monday)
print(fm14monday)
summary(fm15monday)
print(fm15monday)
summary(fm16monday)
print(fm16monday)
summary(fm17monday)
print(fm17monday)

## Weekend
mod4saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 4)
fm4saturday <- fit(mod4saturday)
mod5saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 5)
fm5saturday <- fit(mod5saturday)
mod6saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 6)
fm6saturday <- fit(mod6saturday)
mod7saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 7)
fm7saturday <- fit(mod7saturday)
mod8saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 8)
fm8saturday <- fit(mod8saturday)
mod9saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 9)
fm9saturday <- fit(mod9saturday)
mod10saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 10)
fm10saturday <- fit(mod10saturday)
mod11saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 11)
fm11saturday <- fit(mod11saturday)
mod12saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 12)
fm12saturday <- fit(mod12saturday)
mod13saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 13)
fm13saturday <- fit(mod13saturday)
mod14saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 14)
fm14saturday <- fit(mod14saturday)
mod15saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 15)
fm15saturday <- fit(mod15saturday)
mod16saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 16)
fm16saturday <- fit(mod16saturday)
mod17saturday <- depmix(response = Global_active_power ~ 1, data = table_weekend_saturday_morning_train, nstates = 17)
fm17saturday <- fit(mod17saturday)

summary(fm4saturday)
print(fm4saturday)
summary(fm5saturday)
print(fm5saturday)
summary(fm6saturday)
print(fm6saturday)
summary(fm7saturday)
print(fm7saturday)
summary(fm8saturday)
print(fm8saturday)
summary(fm9saturday)
print(fm9saturday)
summary(fm10saturday)
print(fm10saturday)
summary(fm11saturday)
print(fm11saturday)
summary(fm12saturday)
print(fm12saturday)
summary(fm13saturday)
print(fm13saturday)
summary(fm14saturday)
print(fm14saturday)
summary(fm15saturday)
print(fm15saturday)
summary(fm16saturday)
print(fm16saturday)
summary(fm17saturday)
print(fm17saturday)

## Multivariant HMM


# Weekday

n_times_monday_morning_train <- nrow(table_weekend_monday_morning_train)
n_times_saturday_morning_train <- nrow(table_weekend_saturday_morning_train)

weekday_monday_model_train_4 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                       data = table_weekday_monday_morning_train, 
                                       nstates = 4, 
                                       ntimes=n_times_monday_morning_train, 
                                       family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_4 <- fit(weekday_monday_model_train_4,em=em.control(maxit = 2000))

weekday_monday_model_train_5 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                       data = table_weekday_monday_morning_train, 
                                       nstates = 5, 
                                       ntimes=n_times_monday_morning_train, 
                                       family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_5 <- fit(weekday_monday_model_train_5,em=em.control(maxit = 2000))

weekday_monday_model_train_6 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                       data = table_weekday_monday_morning_train, 
                                       nstates = 6, 
                                       ntimes=n_times_monday_morning_train, 
                                       family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_6 <- fit(weekday_monday_model_train_6,em=em.control(maxit = 2000))

weekday_monday_model_train_7 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                       data = table_weekday_monday_morning_train, 
                                       nstates = 7, 
                                       ntimes=n_times_monday_morning_train, 
                                       family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_7 <- fit(weekday_monday_model_train_7,em=em.control(maxit = 2000))

weekday_monday_model_train_8 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                       data = table_weekday_monday_morning_train, 
                                       nstates = 8, 
                                       ntimes=n_times_monday_morning_train, 
                                       family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_8 <- fit(weekday_monday_model_train_8,em=em.control(maxit = 2000))

weekday_monday_model_train_9 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                       data = table_weekday_monday_morning_train, 
                                       nstates = 9, 
                                       ntimes=n_times_monday_morning_train, 
                                       family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_9 <- fit(weekday_monday_model_train_9,em=em.control(maxit = 2000))

weekday_monday_model_train_10 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                        data = table_weekday_monday_morning_train, 
                                        nstates = 10, 
                                        ntimes=n_times_monday_morning_train, 
                                        family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_10 <- fit(weekday_monday_model_train_10,em=em.control(maxit = 2000))

weekday_monday_model_train_11 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                        data = table_weekday_monday_morning_train, 
                                        nstates = 11, 
                                        ntimes=n_times_monday_morning_train, 
                                        family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_11 <- fit(weekday_monday_model_train_11,em=em.control(maxit = 2000))

weekday_monday_model_train_12 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                        data = table_weekday_monday_morning_train, 
                                        nstates = 12, 
                                        ntimes=n_times_monday_morning_train, 
                                        family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_12 <- fit(weekday_monday_model_train_12,em=em.control(maxit = 2000))

weekday_monday_model_train_13 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                        data = table_weekday_monday_morning_train, 
                                        nstates = 13, 
                                        ntimes=n_times_monday_morning_train, 
                                        family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_13 <- fit(weekday_monday_model_train_13,em=em.control(maxit = 2000))

weekday_monday_model_train_14 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                        data = table_weekday_monday_morning_train, 
                                        nstates = 14, 
                                        ntimes=n_times_monday_morning_train, 
                                        family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_14 <- fit(weekday_monday_model_train_14,em=em.control(maxit = 2000))

weekday_monday_model_train_15 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                        data = table_weekday_monday_morning_train, 
                                        nstates = 15, 
                                        ntimes=n_times_monday_morning_train, 
                                        family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_15 <- fit(weekday_monday_model_train_15,em=em.control(maxit = 2000))

weekday_monday_model_train_16 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                        data = table_weekday_monday_morning_train, 
                                        nstates = 16, 
                                        ntimes=n_times_monday_morning_train, 
                                        family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_16 <- fit(weekday_monday_model_train_16,em=em.control(maxit = 2000))

weekday_monday_model_train_17 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                        data = table_weekday_monday_morning_train, 
                                        nstates = 17, 
                                        ntimes=n_times_monday_morning_train, 
                                        family = list(gaussian(), gaussian(), gaussian()))
weekday_monday_model_train_fit_17 <- fit(weekday_monday_model_train_17,em=em.control(maxit = 2000))

summary(weekday_monday_model_train_fit_4)
print(weekday_monday_model_train_fit_4)
summary(weekday_monday_model_train_fit_5)
print(weekday_monday_model_train_fit_5)
summary(weekday_monday_model_train_fit_6)
print(weekday_monday_model_train_fit_6)
summary(weekday_monday_model_train_fit_7)
print(weekday_monday_model_train_fit_7)
summary(weekday_monday_model_train_fit_8)
print(weekday_monday_model_train_fit_8)
summary(weekday_monday_model_train_fit_9)
print(weekday_monday_model_train_fit_9)
summary(weekday_monday_model_train_fit_10)
print(weekday_monday_model_train_fit_10)
summary(weekday_monday_model_train_fit_11)
print(weekday_monday_model_train_fit_11)
summary(weekday_monday_model_train_fit_12)
print(weekday_monday_model_train_fit_12)
summary(weekday_monday_model_train_fit_13)
print(weekday_monday_model_train_fit_13)
summary(weekday_monday_model_train_fit_14)
print(weekday_monday_model_train_fit_14)
summary(weekday_monday_model_train_fit_15)
print(weekday_monday_model_train_fit_15)

## Weekend

weekend_saturday_model_train_4 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                         data = table_weekend_saturday_morning_train, 
                                         nstates = 4, 
                                         ntimes=n_times_saturday_morning_train, 
                                         family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_4 <- fit(weekend_saturday_model_train_4,em=em.control(maxit = 2000))

weekend_saturday_model_train_5 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                         data = table_weekend_saturday_morning_train, 
                                         nstates = 5, 
                                         ntimes=n_times_saturday_morning_train, 
                                         family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_5 <- fit(weekend_saturday_model_train_5,em=em.control(maxit = 2000))

weekend_saturday_model_train_6 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                         data = table_weekend_saturday_morning_train, 
                                         nstates = 6, 
                                         ntimes=n_times_saturday_morning_train, 
                                         family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_6 <- fit(weekend_saturday_model_train_6,em=em.control(maxit = 2000))

weekend_saturday_model_train_7 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                         data = table_weekend_saturday_morning_train, 
                                         nstates = 7, 
                                         ntimes=n_times_saturday_morning_train, 
                                         family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_7 <- fit(weekend_saturday_model_train_7,em=em.control(maxit = 2000))

weekend_saturday_model_train_8 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                         data = table_weekend_saturday_morning_train, 
                                         nstates = 8, 
                                         ntimes=n_times_saturday_morning_train, 
                                         family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_8 <- fit(weekend_saturday_model_train_8,em=em.control(maxit = 2000))

weekend_saturday_model_train_9 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                         data = table_weekend_saturday_morning_train, 
                                         nstates = 9, 
                                         ntimes=n_times_saturday_morning_train, 
                                         family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_9 <- fit(weekend_saturday_model_train_9,em=em.control(maxit = 2000))

weekend_saturday_model_train_10 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                          data = table_weekend_saturday_morning_train, 
                                          nstates = 10, 
                                          ntimes=n_times_saturday_morning_train, 
                                          family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_10 <- fit(weekend_saturday_model_train_10,em=em.control(maxit = 2000))
weekend_saturday_model_train_11 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                          data = table_weekend_saturday_morning_train, 
                                          nstates = 11, 
                                          ntimes=n_times_saturday_morning_train, 
                                          family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_11 <- fit(weekend_saturday_model_train_11,em=em.control(maxit = 2000))
weekend_saturday_model_train_12 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                          data = table_weekend_saturday_morning_train, 
                                          nstates = 12, 
                                          ntimes=n_times_saturday_morning_train, 
                                          family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_12 <- fit(weekend_saturday_model_train_12,em=em.control(maxit = 2000))

weekend_saturday_model_train_13 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                          data = table_weekend_saturday_morning_train, 
                                          nstates = 13, 
                                          ntimes=n_times_saturday_morning_train, 
                                          family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_13 <- fit(weekend_saturday_model_train_13,em=em.control(maxit = 2000))

weekend_saturday_model_train_14 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                          data = table_weekend_saturday_morning_train, 
                                          nstates = 14, 
                                          ntimes=n_times_saturday_morning_train, 
                                          family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_14 <- fit(weekend_saturday_model_train_14,em=em.control(maxit = 2000))

weekend_saturday_model_train_15 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                          data = table_weekend_saturday_morning_train, 
                                          nstates = 15, 
                                          ntimes=n_times_saturday_morning_train, 
                                          family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_15 <- fit(weekend_saturday_model_train_15,em=em.control(maxit = 2000))

weekend_saturday_model_train_16 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                          data = table_weekend_saturday_morning_train, 
                                          nstates = 16, 
                                          ntimes=n_times_saturday_morning_train, 
                                          family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_16 <- fit(weekend_saturday_model_train_16,em=em.control(maxit = 2000))

weekend_saturday_model_train_17 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                          data = table_weekend_saturday_morning_train, 
                                          nstates = 17, 
                                          ntimes=n_times_saturday_morning_train, 
                                          family = list(gaussian(), gaussian(), gaussian()))
weekend_saturday_model_train_fit_17 <- fit(weekend_saturday_model_train_17,em=em.control(maxit = 2000))

summary(weekend_saturday_model_train_fit_4)
print(weekend_saturday_model_train_fit_4)
summary(weekend_saturday_model_train_fit_5)
print(weekend_saturday_model_train_fit_5)
summary(weekend_saturday_model_train_fit_6)
print(weekend_saturday_model_train_fit_6)
summary(weekend_saturday_model_train_fit_7)
print(weekend_saturday_model_train_fit_7)
summary(weekend_saturday_model_train_fit_8)
print(weekend_saturday_model_train_fit_8)
summary(weekend_saturday_model_train_fit_9)
print(weekend_saturday_model_train_fit_9)
summary(weekend_saturday_model_train_fit_10)
print(weekend_saturday_model_train_fit_10)
summary(weekend_saturday_model_train_fit_11)
print(weekend_saturday_model_train_fit_11)
summary(weekend_saturday_model_train_fit_12)
print(weekend_saturday_model_train_fit_12)
summary(weekend_saturday_model_train_fit_13)
print(weekend_saturday_model_train_fit_13)
summary(weekend_saturday_model_train_fit_14)
print(weekend_saturday_model_train_fit_14)
summary(weekend_saturday_model_train_fit_15)
print(weekend_saturday_model_train_fit_15)

#plot univariant monday
BIC_VALUE_MONDAY <- c(28782,20384,18721,14273,21390,13047,11551,8827,4183,5122,-455,8350,6052,8605)
BIC_VALUE_SATURDAY <- c(48729,40240,37220,35272,33224,29769,30973,27413,25418,24458,24055,24299,22747,20749)

LOG_LIK_MONDAY <- c(-14267,-10009,-9108,-6803,-10270,-5997,-5136,-3650,-1194,-1518,1420,-2810,-1484,-2572)
LOG_LIK_SATURDAY <- c(-24241,-19937,-18357,-17303,-16188,-14358,-14848,-12945,-11813,-11188,-10831,-10787,-9834,-8647)

plot(BIC_VALUE_SATURDAY, type ="o", col="blue")
plot(LOG_LIK_SATURDAY,type="o",col="red")

# PCA

table_omit_pca <- prcomp(table_omit[,c(3,7,9,8,6,5,4)], center = TRUE,scale. = TRUE)

summary(table_omit_pca)


summary(table_pca)

screeplot(table_pca, type = "l", npcs = 6, main = "Screeplot of the 6 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"), col=c("red"), lty=5, cex=0.6) 
cumpro <- cumsum(table_pca$sdev^2 / sum(table_pca$sdev^2))

plot(cumpro[0:6], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 3, col="blue", lty=5)
abline(h = 0.7339, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)





# Part 4.

#tableAnomaly1 <- read.table("~/Desktop/cmpt318groupproject/Test_Data_with_Injected_Anomalies/test1.txt",header = TRUE, sep = ",")
#tableAnomaly1$Date <- as.POSIXlt(tableAnomaly1$Date, format = "%d/%m/%Y")

#tableAnomaly1 <- tableAnomaly1[which(weekdays(as.Date(tableAnomaly1$Date, format = "%m/%d/%Y"))
 #                         %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday','Saturday','Sunday')),]


#table_weekday_monday_morning_Anomaly <- tableAnomaly1[strptime(tableAnomaly1$Time, format = "%H:%M:%S") >= 
  #                                                     strptime("07:00:00", format = "%H:%M:%S") & 
   #                                                    strptime(tableAnomaly1$Time, format = "%H:%M:%S") <=
    #                                                   strptime("10:00:00", format = "%H:%M:%S"),]
#table_weekday_monday_morning_train_Anomaly <- subset(table_weekday_monday_morning_Anomaly, as.Date(table_weekday_monday_morning_Anomaly$Date) >= as.Date("21-2-2010") & as.Date(table_weekday_monday_morning_Anomaly$Date) <= as.Date("27-2-2010") )


#movingAverages = movavg(table_weekday_monday_morning_train_Anomaly$Global_active_power, n = 60 )

#mA = data.frame(movingAverages)

table_anomaly <- read.table("~/Desktop/cmpt318groupproject/Test_Data_with_Injected_Anomalies/test1.txt", header = TRUE, sep = ",")
table_anomaly$Date <- as.POSIXlt(table_anomaly$Date, format = "%d/%m/%Y")
table_omit_anomaly <- na.omit(table_anomaly, cols=c("Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3"))


table_week_anomaly <- table_omit_anomaly[which(weekdays(as.Date(table_anomaly$Date, format = "%m/%d/%Y"))
                                               %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday','Saturday','Sunday')),]
table_week_anomaly['day'] <- weekdays(as.Date(table_week_anomaly$Date, format = "%m/%d/%Y"))



table_weekend_anomaly <- table_omit_anomaly[which(weekdays(as.Date(table_anomaly$Date, format = "%m/%d/%Y"))
                                               %in% c('Saturday','Sunday')),]
table_weekend_anomaly['day'] <- weekdays(as.Date(table_week_anomaly$Date, format = "%m/%d/%Y"))



table_day_time <- table_week_anomaly[strptime(table_week_anomaly$Time, format = "%H:%M:%S") >= 
                                       strptime("07:00:00", format = "%H:%M:%S") & 
                                       strptime(table_week_anomaly$Time, format = "%H:%M:%S") <=
                                       strptime("10:00:00", format = "%H:%M:%S"),]
tableSubset <- subset(table_day_time, as.Date(table_day_time$Date) >= as.Date("2010-02-21") & as.Date(table_day_time$Date) <= as.Date("2010-02-27") )


table_day_time_weekend <- table_week_anomaly[strptime(table_weekend_anomaly$Time, format = "%H:%M:%S") >= 
                                       strptime("07:00:00", format = "%H:%M:%S") & 
                                       strptime(table_weekend_anomaly$Time, format = "%H:%M:%S") <=
                                       strptime("10:00:00", format = "%H:%M:%S"),]
tableSubset_weekend <- subset(table_day_time_weekend, as.Date(table_day_time_weekend$Date) >= as.Date("2010-02-21") & as.Date(table_day_time_weekend$Date) <= as.Date("2010-02-27") )


movingAverages = movavg(tableSubset$Global_active_power, n = 50, "s")

movingAverages_weekend = movavg(tableSubset_weekend$Global_active_power, n = 50, "s")


# Plotting Averages
plot(movingAverages)

# Plotting the Difference - Change datasets 1 - 5.
plot(tableSubset$Global_active_power - movingAverages)






# Getting Test Data 1 - 5.
table_anomaly1 <- read.table("~/Desktop/cmpt318groupproject/Test_Data_with_Injected_Anomalies/test3.txt", header = TRUE, sep = ",")

###########################################
# Test Data 1.
table_anomaly1$Date <- as.POSIXlt(table_anomaly1$Date, format = "%d/%m/%Y")
table_omit_anomaly1 <- na.omit(table_anomaly1, cols=c("Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3"))


table_week_anomaly1 <- table_omit_anomaly1[which(weekdays(as.Date(table_anomaly1$Date, format = "%m/%d/%Y"))
                                               %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday','Saturday','Sunday')),]
table_week_anomaly1['day'] <- weekdays(as.Date(table_week_anomaly1$Date, format = "%m/%d/%Y"))


table_weekend_anomaly1 <- table_omit_anomaly1[which(weekdays(as.Date(table_anomaly1$Date, format = "%m/%d/%Y"))
                                                  %in% c('Saturday','Sunday')),]
table_weekend_anomaly1['day'] <- weekdays(as.Date(table_weekend_anomaly1$Date, format = "%m/%d/%Y"))


# Extract a year of data (31/12/2009 to 31/12/2010)


table_day_time_weekday1 <- table_week_anomaly1[strptime(table_week_anomaly1$Time, format = "%H:%M:%S") >= 
                                                 strptime("07:00:00", format = "%H:%M:%S") & 
                                                 strptime(table_week_anomaly1$Time, format = "%H:%M:%S") <=
                                                 strptime("10:00:00", format = "%H:%M:%S"),]
tableSubset_weekday1 <- subset(table_day_time_weekday1, as.Date(table_day_time_weekday1$Date) >= as.Date("2009-12-31") & as.Date(table_day_time_weekday1$Date) <= as.Date("2010-12-31") )



table_day_time_weekend1 <- table_weekend_anomaly1[strptime(table_weekend_anomaly1$Time, format = "%H:%M:%S") >= 
                                                    strptime("07:00:00", format = "%H:%M:%S") & 
                                                    strptime(table_weekend_anomaly1$Time, format = "%H:%M:%S") <=
                                                    strptime("10:00:00", format = "%H:%M:%S"),]
tableSubset_weekend1 <- subset(table_day_time_weekend1, as.Date(table_day_time_weekend1$Date) >= as.Date("2009-12-31") & as.Date(table_day_time_weekend1$Date) <= as.Date("2010-12-31") )


n_times_weekday_morning_train <- nrow(tableSubset_weekday1)
n_times_weekend_morning_train <- nrow(tableSubset_weekend1)

# 15 State model - Weekday Univariate
mod15weekday <- depmix(response = Global_active_power ~ 1, data = tableSubset_weekday1, nstates = 15)
fm15weekday <- fit(mod15weekday)

#  17 State model - Weekend Univariate.
mod17weekend <- depmix(response = Global_active_power ~ 1, data = tableSubset_weekend1, nstates = 17)
fm17weekend <- fit(mod17weekend)


# 18 State model - Weekday Multivariate
weekday_model_train_18 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                          data = tableSubset_weekday1, 
                                          nstates = 18, 
                                          ntimes=n_times_weekday_morning_train, 
                                          family = list(gaussian(), gaussian(), gaussian()))
weekday_model_train_fit_18 <- fit(weekday_model_train_18,em=em.control(maxit = 2000))


# 19 State model - Weekend Multivariate.
weekend_saturday_model_train_19 <- depmix(response = list(Global_active_power~1, Global_intensity~1 ,Voltage~1), 
                                          data = tableSubset_weekend1, 
                                          nstates = 19, 
                                          ntimes=n_times_weekend_morning_train, 
                                          family = list(gaussian(), gaussian(), gaussian()))
weekend_model_train_fit_19 <- fit(weekend_saturday_model_train_19,em=em.control(maxit = 2000))


# Weekday Univariate
summary(fm15weekday)
print(fm15weekday)

# Weekday Multivariate
summary(weekday_model_train_fit_18)
print(weekday_model_train_fit_18)

# Weekend Univariate
summary(fm17weekend)
print(fm17weekend)

# Weekend Multivariate
summary(weekend_model_train_fit_19)
print(weekend_model_train_fit_19)


