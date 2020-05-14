#PART I
library(psych)
library(modeest)
library(ggplot2)
library(dplyr)

table <- read.table("C:/Users/Chester/Documents/School/CMPT340/cmpt318-assignment1/Data_Assignment_1.txt", header = TRUE, sep = ",")


table$Date <- as.POSIXlt(table$Date, format = "%d/%m/%Y")

tableSubset <- subset(table, as.Date(table$Date) >= as.Date("2007-03-18") & as.Date(table$Date) <= as.Date("2007-03-24") )

ggplot(data=tableSubset, mapping=aes(x=as.POSIXct(tableSubset$Time, format = "%H"), y=Global_active_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(tableSubset$Time, format = "%H"), 1)))   #To determine the day hour and nighthour

#Creating a Data Frame with Mean, Med and SD values.

GAPmean = mean(tableSubset$Global_active_power, na.rm = TRUE)
GRPmean = mean(tableSubset$Global_reactive_power, na.rm = TRUE)
VOLmean = mean(tableSubset$Voltage, na.rm = TRUE)

GAPGEOmean = geometric.mean(tableSubset$Global_active_power, na.rm = TRUE)
GRPGEOmean = geometric.mean(tableSubset$Global_reactive_power, na.rm = TRUE)
VOLGEOmean = geometric.mean(tableSubset$Voltage, na.rm = TRUE)

GAPmed = median(tableSubset$Global_active_power, na.rm = TRUE)
GRPmed = median(tableSubset$Global_reactive_power, na.rm = TRUE)
VOLmed = median(tableSubset$Voltage, na.rm = TRUE)

GAPMode = mfv(tableSubset$Global_active_power)
GRPMode = mfv(tableSubset$Global_reactive_power)
VOLMode = mfv(tableSubset$Voltage)

GAPsd = sd(tableSubset$Global_active_power, na.rm = TRUE)
GRPsd = sd(tableSubset$Global_reactive_power, na.rm = TRUE)
VOLsd = sd(tableSubset$Voltage, na.rm = TRUE)

GAP = c(GAPmean,GAPGEOmean, GAPmed, GAPMode, GAPsd)
GRP = c(GRPmean,GRPGEOmean, GRPmed, GRPMode, GRPsd)
VOL = c(VOLmean,VOLGEOmean, VOLmed, VOLMode, VOLsd)

df = data.frame(GAP, GRP, VOL)


table_weekday <- tableSubset[which(weekdays(as.Date(tableSubset$Date, format = "%m/%d/%Y"))
                                    %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]

table_weekend <- tableSubset[which(weekdays(as.Date(tableSubset$Date, format = "%m/%d/%Y"))
                                    %in% c('Saturday', 'Sunday')),]
                                    
table_weekday_day <- table_weekday[strptime(table_weekday$Time, format = "%H:%M:%S") >= strptime("08:00:00", format = "%H:%M:%S") & strptime(table_weekday$Time, format = "%H:%M:%S") < strptime("19:00:00", format = "%H:%M:%S"),]
table_weekend_day <- table_weekend[strptime(table_weekend$Time, format = "%H:%M:%S") >= strptime("08:00:00", format = "%H:%M:%S") & strptime(table_weekend$Time, format = "%H:%M:%S") < strptime("19:00:00", format = "%H:%M:%S"),]

`%notin%` <- Negate(`%in%`)
table_weekend_night <-table_weekend[table_weekend$Time %notin% table_weekend_day$Time,]
table_weekday_night <-table_weekday[table_weekday$Time %notin% table_weekday_day$Time,]

#MAX and MIN for GAP
weekend_night_max_GAP <- max(table_weekend_night$Global_active_power, na.rm=TRUE)
weekend_night_min_GAP <- min(table_weekend_night$Global_active_power, na.rm=TRUE)
weekend_day_max_GAP <- max(table_weekend_day$Global_active_power, na.rm=TRUE)
weekend_day_min_GAP <- min(table_weekend_day$Global_active_power, na.rm=TRUE)

weekday_night_max_GAP <- max(table_weekday_night$Global_active_power, na.rm=TRUE)
weekday_night_min_GAP <- min(table_weekday_night$Global_active_power, na.rm=TRUE)
weekday_day_max_GAP <- max(table_weekday_day$Global_active_power, na.rm=TRUE)
weekday_day_min_GAP <- min(table_weekday_day$Global_active_power, na.rm=TRUE)

#MAX and MIN for GRP
weekend_night_max_GRP <- max(table_weekend_night$Global_reactive_power, na.rm=TRUE)
weekend_night_min_GRP <- min(table_weekend_night$Global_reactive_power, na.rm=TRUE)
weekend_day_max_GRP <- max(table_weekend_day$Global_reactive_power, na.rm=TRUE)
weekend_day_min_GRP <- min(table_weekend_day$Global_reactive_power, na.rm=TRUE)

weekday_night_max_GRP <- max(table_weekday_night$Global_reactive_power, na.rm=TRUE)
weekday_night_min_GRP <- min(table_weekday_night$Global_reactive_power, na.rm=TRUE)
weekday_day_max_GRP <- max(table_weekday_day$Global_reactive_power, na.rm=TRUE)
weekday_day_min_GRP <- min(table_weekday_day$Global_reactive_power, na.rm=TRUE)


#PART II

# Computing a Disjoint Correlation Matrix
correlations <- cor(tableSubset[3:9], method = "pearson")
disjoint_correlations <- correlations

# Plotting data to identify the trends.
ggplot(data = table_weekend, mapping = aes(x = as.POSIXct(table_weekend$Time, format = "%H") ,y = table_weekend$Global_active_power )) +
  geom_point(color = "blue")

# Plotting A Color-Coded Correlation Matrix
corr <- round(cor(disjoint_correlations), 4)
ggcorrplot(corr)



#Part III

table_weekday_day_GI_mean = table_weekday_day %>%
  group_by(Time) %>%
  summarise(GI_mean=(mean(Global_intensity)))

table_weekday_night_GI_mean = table_weekday_night %>%
  group_by(Time) %>%
  summarise(GI_mean=(mean(Global_intensity)))

table_weekend_day_GI_mean = table_weekend_day %>%
  group_by(Time) %>%
  summarise(GI_mean=(mean(Global_intensity)))

table_weekend_night_GI_mean = table_weekend_night %>%
  group_by(Time) %>%
  summarise(GI_mean=(mean(Global_intensity)))


#Fitting Linear model
GI_fit_linear_weekday_day <- lm(formula=GI_mean ~ as.POSIXct(table_weekday_day_GI_mean$Time,format="%H:%M:%S"), data=table_weekday_day_GI_mean)
GI_fit_linear_weekday_night <- lm(formula = GI_mean ~ as.POSIXct(table_weekday_night_GI_mean$Time, format = "%H:%M:%S"), data = table_weekday_night_GI_mean)
GI_fit_linear_weekend_day <- lm(formula = GI_mean ~ as.POSIXct(table_weekend_day_GI_mean$Time, format = "%H:%M:%S"), data = table_weekend_day_GI_mean)
GI_fit_linear_weekend_night <- lm(formula = GI_mean ~ as.POSIXct(table_weekend_night_GI_mean$Time, format = "%H:%M:%S"), data = table_weekend_night_GI_mean)


# RSS
RSS_linear_weekday_day = sum(GI_fit_linear_weekday_day$residuals^2)
RSS_linear_weekday_night = sum(GI_fit_linear_weekday_night$residuals^2)
RSS_linear_weekend_day = sum(GI_fit_linear_weekend_day$residuals^2)
RSS_linear_weekend_night = sum(GI_fit_linear_weekend_night$residuals^2)


#Linear prediction
prediction_line_weekday_day <- predict(GI_fit_linear_weekday_day,data=table_weekday_day_GI_mean$Time)
table_weekday_day_GI_mean$prediction_line <- prediction_line_weekday_day

prediction_line_weekday_night <- predict(GI_fit_linear_weekday_night,data=table_weekday_night_GI_mean$Time)
table_weekday_night_GI_mean$prediction_line <- prediction_line_weekday_night

prediction_line_weekend_day <- predict(GI_fit_linear_weekend_day,data=table_weekend_day_GI_mean$Time)
table_weekend_day_GI_mean$prediction_line <- prediction_line_weekend_day

prediction_line_weekend_night <- predict(GI_fit_linear_weekend_night,data=table_weekend_night_GI_mean$Time)
table_weekend_night_GI_mean$prediction_line <- prediction_line_weekend_night



#Plotting linear regression lines.

ggplot(data=table_weekday_day_GI_mean,mapping=aes(x=Time, y=GI_mean,group=1)) +
  geom_line(data=table_weekday_day_GI_mean, mapping=aes(x=Time, y=prediction_line_weekday_day,group=1), size=1, color="red")+
  geom_line(data=table_weekday_night_GI_mean, mapping=aes(x=Time, y=prediction_line_weekday_night,group=1), size=1, color="yellow")+
  geom_line(data=table_weekend_day_GI_mean, mapping=aes(x=Time, y=prediction_line_weekend_day,group=1), size=1, color="green")+
  geom_line(data=table_weekend_night_GI_mean, mapping=aes(x=Time, y=prediction_line_weekend_night,group=1), size=1, color="blue")
  



#Fitting polynomial models.
GI_fit_polynomial_weekday_day <- lm(formula=GI_mean~poly(as.numeric(table_weekday_day_GI_mean$Time),2,raw=TRUE),data=table_weekday_day_GI_mean)
GI_fit_polynomial_weekday_night <- lm(formula=GI_mean~poly(as.numeric(table_weekday_night_GI_mean$Time),2,raw=TRUE),data=table_weekday_night_GI_mean)
GI_fit_polynomial_weekend_day <- lm(formula=GI_mean~poly(as.numeric(table_weekend_day_GI_mean$Time),2,raw=TRUE),data=table_weekend_day_GI_mean)
GI_fit_polynomial_weekend_night <- lm(formula=GI_mean~poly(as.numeric(table_weekend_night_GI_mean$Time),2,raw=TRUE),data=table_weekend_night_GI_mean)


# Polynomial Prediction.
prediction_polynomial_weekday_day <- predict(GI_fit_polynomial_weekday_day, data=table_weekday_day_GI_mean$Time)
table_weekday_day_GI_mean$prediction_polynomial <- prediction_polynomial_weekday_day

prediction_polynomial_weekday_night <- predict(GI_fit_polynomial_weekday_night, data=table_weekday_night_GI_mean$Time)
table_weekday_night_GI_mean$prediction_polynomial <- prediction_polynomial_weekday_night

prediction_polynomial_weekend_day <- predict(GI_fit_polynomial_weekend_day, data=table_weekend_day_GI_mean$Time)
table_weekend_day_GI_mean$prediction_polynomial <- prediction_polynomial_weekend_day

prediction_polynomial_weekend_night <- predict(GI_fit_polynomial_weekend_night, data=table_weekend_night_GI_mean$Time)
table_weekend_night_GI_mean$prediction_polynomial <- prediction_polynomial_weekend_night


# Plot polynomial lines on the data.
ggplot(data=table_weekday_day_GI_mean, mapping=aes(x=Time, y=GI_mean)) +
  geom_line(data=table_weekday_day_GI_mean, mapping=aes(x=Time, y=prediction_polynomial_weekday_day,group=1), size=1, color="red") +
  geom_line(data=table_weekday_night_GI_mean, mapping=aes(x=Time, y=prediction_polynomial_weekday_night,group=1), size=1, color="yellow") +
  geom_line(data=table_weekend_day_GI_mean, mapping=aes(x=Time, y=prediction_polynomial_weekend_day,group=1), size=1, color="green") + 
  geom_line(data=table_weekend_night_GI_mean, mapping=aes(x=Time, y=prediction_polynomial_weekend_night,group=1), size=1, color="blue")






