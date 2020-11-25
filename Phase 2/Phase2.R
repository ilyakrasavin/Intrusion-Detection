# Load Libraries
library(depmixS4)
library(ggplot2)

# Load the Dataset
table <- read.table("./Data_Part_2.txt", header = TRUE, sep = ",")

# Format Dates as Posix objects
table$Date <- as.POSIXlt(table$Date, format = "%d/%m/%Y")

# Select Weekdays - Daytime
table_weekday <- table[which(weekdays(as.Date(table$Date, format = "%m/%d/%Y"))
                             %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')),]
table_weekday['day'] <- weekdays(as.Date(table_weekday$Date, format = "%m/%d/%Y"))

table_weekday_day <- table_weekday[strptime(table_weekday$Time, format = "%H:%M:%S") >= 
                                   strptime("12:00:00", format = "%H:%M:%S") & 
                                   strptime(table_weekday$Time, format = "%H:%M:%S") <
                                   strptime("18:00:00", format = "%H:%M:%S"),]

# Plotting data.

# Global_active_power
# Scatterplot
ggplot(data=table_weekday_day, mapping=aes(x=as.POSIXct(table_weekday_day$Time, format = "%H"), y=Global_active_power)) +
  geom_point(color = "blue")
# Boxplot
ggplot(data=table_weekday_day, mapping=aes(x=as.POSIXct(table_weekday_day$Time, format = "%H"), y=Global_active_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_day$Time, format = "%H"), 1)))

# Global_reactive_power
# Scatterplot
ggplot(data=table_weekday_day, mapping=aes(x=as.POSIXct(table_weekday_day$Time, format = "%H"), y=Global_reactive_power)) +
  geom_point(color = "blue")
# Boxplot
ggplot(data=table_weekday_day, mapping=aes(x=as.POSIXct(table_weekday_day$Time, format = "%H"), y=Global_reactive_power)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_day$Time, format = "%H"), 1)))


# Global_intensity
# Scatterplot
ggplot(data=table_weekday_day, mapping=aes(x=as.POSIXct(table_weekday_day$Time, format = "%H"), y=Global_intensity)) +
  geom_point(color = "blue")
# Boxplot
ggplot(data=table_weekday_day, mapping=aes(x=as.POSIXct(table_weekday_day$Time, format = "%H"), y=Global_intensity)) +
  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_day$Time, format = "%H"), 1)))
#########################################################


# Part 2 - Univariate HMMs using depmix4
# Number of states 4 - 20

mod4 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 4)
fm4 <- fit(mod4)

summary(fm4)
print(fm4)
###############################################
mod5 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 5)
fm5 <- fit(mod5)

summary(fm5)
print(fm5)
###############################################
mod6 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 6)
fm6 <- fit(mod6)

summary(fm6)
print(fm6)
###############################################
mod7 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 7)
fm7 <- fit(mod7)

summary(fm7)
print(fm7)
###############################################
mod8 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 8)
fm8 <- fit(mod8)

summary(fm8)
print(fm8)
###############################################
mod9 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 9)
fm9 <- fit(mod9)

summary(fm9)
print(fm9)
###############################################
mod10 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 10)
fm10 <- fit(mod10)

summary(fm10)
print(fm10)
###############################################
mod11 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 11)
fm11 <- fit(mod11)

summary(fm11)
print(fm11)
###############################################
mod12 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 12)
fm12 <- fit(mod12)

summary(fm12)
print(fm12)
###############################################
mod13 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 13)
fm13 <- fit(mod13)

summary(fm13)
print(fm13)
###############################################
mod14 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 14)
fm14 <- fit(mod14)

summary(fm14)
print(fm14)
###############################################
mod15 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 15)
fm15 <- fit(mod15)

summary(fm15)
print(fm15)
###############################################
mod16 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 16)
fm16 <- fit(mod16)

summary(fm16)
print(fm16)
###############################################
mod17 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 17)
fm17 <- fit(mod17)

summary(fm17)
print(fm17)
###############################################
mod18 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 18)
fm18 <- fit(mod18)

summary(fm18)
print(fm18)
###############################################
mod19 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 19)
fm19 <- fit(mod19)

summary(fm19)
print(fm19)
###############################################
mod20 <- depmix(response = Global_active_power ~ 1, data = table_weekday_day, nstates = 20)
fm20 <- fit(mod20)

summary(fm20)
print(fm20)


# Plot Bayesian Information Criterion for chosen number of states
plot(1:17,c(BIC(fm4),BIC(fm5),BIC(fm6),BIC(fm7),BIC(fm8),BIC(fm9),BIC(fm10),BIC(fm11), BIC(fm12), BIC(fm13), BIC(fm14),BIC(fm15), BIC(fm16),BIC(fm17),BIC(fm18),BIC(fm19),BIC(fm20)),ty="b")



# Part 3 - Fixed Sliding Window (60 minutes) Observations
###################################################################################

# Computing a plotting Moving Averages for a week

movingAverages = movavg(table_weekday_day$Global_active_power,n = 60 )

mA = data.frame(movingAverages)

ggplot(data = mA,aes(x = 'Observations', y = 'movingAverages' )) +
  geom_line(aes(movingAverages))

# Compute Moving Averages over the week. Time window chosen - 60 minutes = 1 hour.
movingAverages = movavg(table_weekday_day$Global_active_power,n = 60 )

# Plotting Averages
plot(movingAverages)

# Plotting difference between Actual data and Moving Averages.
plot(table_weekday_day$Global_active_power - movingAverages)

