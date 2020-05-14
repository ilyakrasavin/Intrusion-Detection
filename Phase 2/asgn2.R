library("depmixS4")
#data("speed")
set.seed(1)




table <- read.table("~/Desktop/CMPT318/Assignments/GA1/cmpt318-assignment1/Phase2/Data_Assignmet2.txt", header = TRUE, sep = ",")
#table <- read.table("C:/Users/Chester/Documents/School/CMPT340/cmpt318-assignment1/Phase2/Data_Assignmet2.txt", header = TRUE, sep = ",")

#head(table)
#?table

table$Date <- as.POSIXlt(table$Date, format = "%d/%m/%Y")

table_weekday <- table[which(weekdays(as.Date(table$Date, format = "%m/%d/%Y"))
                             %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')),]
table_weekday['day'] <- weekdays(as.Date(table_weekday$Date, format = "%m/%d/%Y"))

table_weekday_day <- table_weekday[strptime(table_weekday$Time, format = "%H:%M:%S") >= 
                                   strptime("12:00:00", format = "%H:%M:%S") & 
                                   strptime(table_weekday$Time, format = "%H:%M:%S") <
                                   strptime("18:00:00", format = "%H:%M:%S"),]

# SHOULD WE USE WEEKDAYS ONLY?
# tableSubset <- subset(table, as.Date(table$Date) >= as.Date("2007-03-18") & as.Date(table$Date) <= as.Date("2007-03-24") )

#tableSubset <- subset(table, as.Date(table$Date) >= as.Date("2007-03-18") & as.Date(table$Date) <= as.Date("2007-03-22") )


###############################################



# Plotting the data to determine theto be used in the analysis.
#Global_active_power
#ggplot(data=tableSubset, mapping=aes(x=as.POSIXct(tableSubset$Time, format = "%H"), y=Global_active_power)) +
#  geom_boxplot(aes(group=cut_width(as.POSIXct(tableSubset$Time, format = "%H"), 1)))   #To determine the day hour and nighthour

#Global_active_power
#ggplot(data=table_weekday_day, mapping=aes(x=as.POSIXct(table_weekday_day$Time, format = "%H"), y=Global_active_power)) +
#  geom_point(color = "blue")
#ggplot(data=table_weekday_day, mapping=aes(x=as.POSIXct(table_weekday_day$Time, format = "%H"), y=Global_active_power)) +
#  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_day$Time, format = "%H"), 1)))

#Global_reactive_power
#ggplot(data=table_weekday_day, mapping=aes(x=as.POSIXct(table_weekday_day$Time, format = "%H"), y=Global_reactive_power)) +
#  geom_point(color = "blue")
#ggplot(data=table_weekday_day, mapping=aes(x=as.POSIXct(table_weekday_day$Time, format = "%H"), y=Global_reactive_power)) +
#  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_day$Time, format = "%H"), 1)))


#Global_intensity
#ggplot(data=table_weekday_day, mapping=aes(x=as.POSIXct(table_weekday_day$Time, format = "%H"), y=Global_intensity)) +
#  geom_point(color = "blue")
#ggplot(data=table_weekday_day, mapping=aes(x=as.POSIXct(table_weekday_day$Time, format = "%H"), y=Global_intensity)) +
#  geom_boxplot(aes(group=cut_width(as.POSIXct(table_weekday_day$Time, format = "%H"), 1)))
########################################################################################################################



mod4 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 4)
fm4 <- fit(mod4)

summary(fm4)
print(fm4)


###############################################


mod5 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 5)
fm5 <- fit(mod5)

summary(fm5)
print(fm5)

###############################################

mod6 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 6)
fm6 <- fit(mod6)

summary(fm6)
print(fm6)

###############################################


mod7 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 7)
fm7 <- fit(mod7)

summary(fm7)
print(fm7)

###############################################

mod8 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 8)
fm8 <- fit(mod8)

summary(fm8)
print(fm8)

###############################################


mod9 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 9)
fm9 <- fit(mod9)

summary(fm9)
print(fm9)

###############################################

mod10 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 10)
fm10 <- fit(mod10)

summary(fm10)
print(fm10)


###############################################


mod11 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 11)
fm11 <- fit(mod11)

summary(fm11)
print(fm11)

<<<<<<< HEAD

=======
###############################################
>>>>>>> 682b1484940525c095fbb197bc58b1cc9ed42b3a


mod12 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 12)
fm12 <- fit(mod12)

summary(fm12)
print(fm12)


<<<<<<< HEAD
=======
###############################################


>>>>>>> 682b1484940525c095fbb197bc58b1cc9ed42b3a
mod13 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 13)
fm13 <- fit(mod13)

summary(fm13)
print(fm13)


<<<<<<< HEAD
=======
###############################################


>>>>>>> 682b1484940525c095fbb197bc58b1cc9ed42b3a
mod14 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 14)
fm14 <- fit(mod14)

summary(fm14)
print(fm14)



<<<<<<< HEAD
=======
###############################################


>>>>>>> 682b1484940525c095fbb197bc58b1cc9ed42b3a
mod15 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 15)
fm15 <- fit(mod15)

summary(fm15)
print(fm15)


<<<<<<< HEAD
=======
###############################################

>>>>>>> 682b1484940525c095fbb197bc58b1cc9ed42b3a

mod16 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 16)
fm16 <- fit(mod16)

summary(fm16)
print(fm16)

<<<<<<< HEAD
=======
###############################################

>>>>>>> 682b1484940525c095fbb197bc58b1cc9ed42b3a


mod17 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 17)
fm17 <- fit(mod17)

summary(fm17)
print(fm17)


<<<<<<< HEAD
=======
###############################################

>>>>>>> 682b1484940525c095fbb197bc58b1cc9ed42b3a

mod18 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 18)
fm18 <- fit(mod18)

summary(fm18)
print(fm18)


<<<<<<< HEAD
=======
###############################################

>>>>>>> 682b1484940525c095fbb197bc58b1cc9ed42b3a

mod19 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 19)
fm19 <- fit(mod19)

summary(fm19)
print(fm19)


<<<<<<< HEAD
=======
###############################################

>>>>>>> 682b1484940525c095fbb197bc58b1cc9ed42b3a

mod20 <- depmix(response = Global_active_power ~ 1, data = tableSubset, nstates = 20)
fm20 <- fit(mod20)

summary(fm20)
print(fm20)

<<<<<<< HEAD

=======
###############################################

# Ploting BIC values for number of states used on data.
>>>>>>> 682b1484940525c095fbb197bc58b1cc9ed42b3a

plot(1:17,c(BIC(fm4),BIC(fm5),BIC(fm6),BIC(fm7),BIC(fm8),BIC(fm9),BIC(fm10),BIC(fm11), BIC(fm12), BIC(fm13), BIC(fm14),BIC(fm15), BIC(fm16),BIC(fm17),BIC(fm18),BIC(fm19),BIC(fm20)),ty="b")



<<<<<<< HEAD

###################################################################################


# Fixed Window Observations
=======
# PART 3.
###################################################################################


# Extracting the data for the chosen week (2007-12-10 to 2007-12-15)
>>>>>>> 682b1484940525c095fbb197bc58b1cc9ed42b3a

table$Date <- as.POSIXlt(table$Date, format = "%d/%m/%Y")

table_week <- table[which(weekdays(as.Date(table$Date, format = "%m/%d/%Y"))
                             %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday','Saturday','Sunday')),]
table_weekday['day'] <- weekdays(as.Date(table_weekday$Date, format = "%m/%d/%Y"))

table_weekday_day <- table_weekday[strptime(table_weekday$Time, format = "%H:%M:%S") >= 
                                    strptime("00:00:00", format = "%H:%M:%S") & 
                                     strptime(table_weekday$Time, format = "%H:%M:%S") <=
                                     strptime("23:59:59", format = "%H:%M:%S"),]

tableSubset <- subset(table_weekday_day, as.Date(table_weekday_day$Date) >= as.Date("2007-12-10") & as.Date(table_weekday_day$Date) <= as.Date("2007-12-15") )

<<<<<<< HEAD

movingAverages = movavg(tableSubset$Global_active_power,n = 60 )

mA = data.frame(movingAverages)

ggplot(data = mA,aes(x = 'Observations', y = 'movingAverages' )) +
  geom_line(aes(movingAverages))



=======
# Compute Moving Averages over the week. Time window chosen - 60 minutes = 1 hour.
movingAverages = movavg(tableSubset$Global_active_power,n = 60 )

# Plotting Averages
plot(movingAverages)

# Plotting difference between Actual data and computed Moving Averages.
plot(tableSubset$Global_active_power - movingAverages)
>>>>>>> 682b1484940525c095fbb197bc58b1cc9ed42b3a






