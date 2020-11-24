# Load Libraries
library(psych)
library(modeest)
library(ggplot2)
library(dplyr)

# Load the Dataset
table <- read.table("./Data_Part_1.txt", header = TRUE, sep = ",")

# Format the dates as a POSIX object
table$Date <- as.POSIXlt(table$Date, format = "%d/%m/%Y")

# Select a Single sample week for analysis
tableSubset <- subset(table, as.Date(table$Date) >= as.Date("2007-03-18") & as.Date(table$Date) <= as.Date("2007-03-24") )

# Plot GlobalActivePower for a Single week as a boxplot.
ggplot(data=tableSubset, mapping=aes(x=as.POSIXct(tableSubset$Time, format = "%H"), y=Global_active_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(tableSubset$Time, format = "%H"), 1)))   #To determine the day hour and nighthour


# Descriptive Statistics Calculation
# Arithmetic Means
GAPmean = mean(tableSubset$Global_active_power, na.rm = TRUE)
GRPmean = mean(tableSubset$Global_reactive_power, na.rm = TRUE)
VOLmean = mean(tableSubset$Voltage, na.rm = TRUE)

# Geometric Means
GAPGEOmean = geometric.mean(tableSubset$Global_active_power, na.rm = TRUE)
GRPGEOmean = geometric.mean(tableSubset$Global_reactive_power, na.rm = TRUE)
VOLGEOmean = geometric.mean(tableSubset$Voltage, na.rm = TRUE)

# Medians
GAPmed = median(tableSubset$Global_active_power, na.rm = TRUE)
GRPmed = median(tableSubset$Global_reactive_power, na.rm = TRUE)
VOLmed = median(tableSubset$Voltage, na.rm = TRUE)

# Modes
GAPMode = mfv(tableSubset$Global_active_power)
GRPMode = mfv(tableSubset$Global_reactive_power)
VOLMode = mfv(tableSubset$Voltage)

# Standard Deviations
GAPsd = sd(tableSubset$Global_active_power, na.rm = TRUE)
GRPsd = sd(tableSubset$Global_reactive_power, na.rm = TRUE)
VOLsd = sd(tableSubset$Voltage, na.rm = TRUE)

# Stack it into columns
GAP = c(GAPmean,GAPGEOmean, GAPmed, GAPMode, GAPsd)
GRP = c(GRPmean,GRPGEOmean, GRPmed, GRPMode, GRPsd)
VOL = c(VOLmean,VOLGEOmean, VOLmed, VOLMode, VOLsd)

# Save as a dataframe
df = data.frame(GAP, GRP, VOL)


# Separate Weekends and Weekdays of a chosen sample week
table_weekday <- tableSubset[which(weekdays(as.Date(tableSubset$Date, format = "%m/%d/%Y"))
                                    %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
table_weekend <- tableSubset[which(weekdays(as.Date(tableSubset$Date, format = "%m/%d/%Y"))
                                    %in% c('Saturday', 'Sunday')),]
                                    
# Select Daytime Hours: 8am to 7pm
table_weekday_day <- table_weekday[strptime(table_weekday$Time, format = "%H:%M:%S") >= strptime("08:00:00", format = "%H:%M:%S") & strptime(table_weekday$Time, format = "%H:%M:%S") < strptime("19:00:00", format = "%H:%M:%S"),]
table_weekend_day <- table_weekend[strptime(table_weekend$Time, format = "%H:%M:%S") >= strptime("08:00:00", format = "%H:%M:%S") & strptime(table_weekend$Time, format = "%H:%M:%S") < strptime("19:00:00", format = "%H:%M:%S"),]

# Select Nighttime Hours - Defined as the opposite: 8pm to 7am
`%notin%` <- Negate(`%in%`)
table_weekend_night <-table_weekend[table_weekend$Time %notin% table_weekend_day$Time,]
table_weekday_night <-table_weekday[table_weekday$Time %notin% table_weekday_day$Time,]

# Minimum and Maximum Values of GlobalActivePower feature (Weekends & Weekdays)
weekend_night_max_GAP <- max(table_weekend_night$Global_active_power, na.rm=TRUE)
weekend_night_min_GAP <- min(table_weekend_night$Global_active_power, na.rm=TRUE)
weekend_day_max_GAP <- max(table_weekend_day$Global_active_power, na.rm=TRUE)
weekend_day_min_GAP <- min(table_weekend_day$Global_active_power, na.rm=TRUE)

weekday_night_max_GAP <- max(table_weekday_night$Global_active_power, na.rm=TRUE)
weekday_night_min_GAP <- min(table_weekday_night$Global_active_power, na.rm=TRUE)
weekday_day_max_GAP <- max(table_weekday_day$Global_active_power, na.rm=TRUE)
weekday_day_min_GAP <- min(table_weekday_day$Global_active_power, na.rm=TRUE)

# Minimum and Maximum Values of GlobalReactivePower feature (Weekends & Weekdays)
weekend_night_max_GRP <- max(table_weekend_night$Global_reactive_power, na.rm=TRUE)
weekend_night_min_GRP <- min(table_weekend_night$Global_reactive_power, na.rm=TRUE)
weekend_day_max_GRP <- max(table_weekend_day$Global_reactive_power, na.rm=TRUE)
weekend_day_min_GRP <- min(table_weekend_day$Global_reactive_power, na.rm=TRUE)

weekday_night_max_GRP <- max(table_weekday_night$Global_reactive_power, na.rm=TRUE)
weekday_night_min_GRP <- min(table_weekday_night$Global_reactive_power, na.rm=TRUE)
weekday_day_max_GRP <- max(table_weekday_day$Global_reactive_power, na.rm=TRUE)
weekday_day_min_GRP <- min(table_weekday_day$Global_reactive_power, na.rm=TRUE)


# PART II - Correlation Study

# Computing Disjoint Correlation Matrix
correlations <- cor(tableSubset[3:9], method = "pearson")
disjoint_correlations <- correlations

# Plotting data to see the trends.
ggplot(data = table_weekend, mapping = aes(x = as.POSIXct(table_weekend$Time, format = "%H") ,y = table_weekend$Global_active_power )) +
  geom_point(color = "blue")

# Plotting a Color-Coded Correlation Matrix
corr <- round(cor(disjoint_correlations), 4)
ggcorrplot(corr)



# Part III - Regression Analysis (Global Intensity)

# Compute Global Intensity Means
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


# Fitting Linear Models for Weekday/Weekend - Daytime/Nighttime Combinations
GI_fit_linear_weekday_day <- lm(formula=GI_mean ~ as.POSIXct(table_weekday_day_GI_mean$Time,format="%H:%M:%S"), data=table_weekday_day_GI_mean)
GI_fit_linear_weekday_night <- lm(formula = GI_mean ~ as.POSIXct(table_weekday_night_GI_mean$Time, format = "%H:%M:%S"), data = table_weekday_night_GI_mean)
GI_fit_linear_weekend_day <- lm(formula = GI_mean ~ as.POSIXct(table_weekend_day_GI_mean$Time, format = "%H:%M:%S"), data = table_weekend_day_GI_mean)
GI_fit_linear_weekend_night <- lm(formula = GI_mean ~ as.POSIXct(table_weekend_night_GI_mean$Time, format = "%H:%M:%S"), data = table_weekend_night_GI_mean)


# Residual Sum of Squares (RSS)
RSS_linear_weekday_day = sum(GI_fit_linear_weekday_day$residuals^2)
RSS_linear_weekday_night = sum(GI_fit_linear_weekday_night$residuals^2)
RSS_linear_weekend_day = sum(GI_fit_linear_weekend_day$residuals^2)
RSS_linear_weekend_night = sum(GI_fit_linear_weekend_night$residuals^2)


# Linear predictions for Weekday/Weekend - Daytime/Nighttime Combinations
prediction_line_weekday_day <- predict(GI_fit_linear_weekday_day,data=table_weekday_day_GI_mean$Time)
table_weekday_day_GI_mean$prediction_line <- prediction_line_weekday_day

prediction_line_weekday_night <- predict(GI_fit_linear_weekday_night,data=table_weekday_night_GI_mean$Time)
table_weekday_night_GI_mean$prediction_line <- prediction_line_weekday_night

prediction_line_weekend_day <- predict(GI_fit_linear_weekend_day,data=table_weekend_day_GI_mean$Time)
table_weekend_day_GI_mean$prediction_line <- prediction_line_weekend_day

prediction_line_weekend_night <- predict(GI_fit_linear_weekend_night,data=table_weekend_night_GI_mean$Time)
table_weekend_night_GI_mean$prediction_line <- prediction_line_weekend_night


# Plotting results of linear regression
ggplot(data=table_weekday_day_GI_mean,mapping=aes(x=Time, y=GI_mean,group=1)) +
  geom_line(data=table_weekday_day_GI_mean, mapping=aes(x=Time, y=prediction_line_weekday_day,group=1), size=1, color="red")+
  geom_line(data=table_weekday_night_GI_mean, mapping=aes(x=Time, y=prediction_line_weekday_night,group=1), size=1, color="yellow")+
  geom_line(data=table_weekend_day_GI_mean, mapping=aes(x=Time, y=prediction_line_weekend_day,group=1), size=1, color="green")+
  geom_line(data=table_weekend_night_GI_mean, mapping=aes(x=Time, y=prediction_line_weekend_night,group=1), size=1, color="blue")
  


# Fitting Polynomial Models of Degree 2 for Weekday/Weekend - Daytime/Nighttime Combinations
GI_fit_polynomial_weekday_day <- lm(formula=GI_mean~poly(as.numeric(table_weekday_day_GI_mean$Time),2,raw=TRUE),data=table_weekday_day_GI_mean)
GI_fit_polynomial_weekday_night <- lm(formula=GI_mean~poly(as.numeric(table_weekday_night_GI_mean$Time),2,raw=TRUE),data=table_weekday_night_GI_mean)
GI_fit_polynomial_weekend_day <- lm(formula=GI_mean~poly(as.numeric(table_weekend_day_GI_mean$Time),2,raw=TRUE),data=table_weekend_day_GI_mean)
GI_fit_polynomial_weekend_night <- lm(formula=GI_mean~poly(as.numeric(table_weekend_night_GI_mean$Time),2,raw=TRUE),data=table_weekend_night_GI_mean)

# Polynomial Predictions for Weekday/Weekend - Daytime/Nighttime Combinations
prediction_polynomial_weekday_day <- predict(GI_fit_polynomial_weekday_day, data=table_weekday_day_GI_mean$Time)
table_weekday_day_GI_mean$prediction_polynomial <- prediction_polynomial_weekday_day

prediction_polynomial_weekday_night <- predict(GI_fit_polynomial_weekday_night, data=table_weekday_night_GI_mean$Time)
table_weekday_night_GI_mean$prediction_polynomial <- prediction_polynomial_weekday_night

prediction_polynomial_weekend_day <- predict(GI_fit_polynomial_weekend_day, data=table_weekend_day_GI_mean$Time)
table_weekend_day_GI_mean$prediction_polynomial <- prediction_polynomial_weekend_day

prediction_polynomial_weekend_night <- predict(GI_fit_polynomial_weekend_night, data=table_weekend_night_GI_mean$Time)
table_weekend_night_GI_mean$prediction_polynomial <- prediction_polynomial_weekend_night


# Plot results of polynomial regression
ggplot(data=table_weekday_day_GI_mean, mapping=aes(x=Time, y=GI_mean)) +
  geom_line(data=table_weekday_day_GI_mean, mapping=aes(x=Time, y=prediction_polynomial_weekday_day,group=1), size=1, color="red") +
  geom_line(data=table_weekday_night_GI_mean, mapping=aes(x=Time, y=prediction_polynomial_weekday_night,group=1), size=1, color="yellow") +
  geom_line(data=table_weekend_day_GI_mean, mapping=aes(x=Time, y=prediction_polynomial_weekend_day,group=1), size=1, color="green") + 
  geom_line(data=table_weekend_night_GI_mean, mapping=aes(x=Time, y=prediction_polynomial_weekend_night,group=1), size=1, color="blue")




