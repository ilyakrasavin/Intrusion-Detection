getwd()
setwd("C:/Users/Chester/Documents/cmpt318-assignment1")
table <- read.table("Data_Assignment_1.txt", header=T, sep = ",")

date <- table$Date
table$Date <- as.POSIXlt(date, format = "%d/%m/%Y")

table_subset <- subset(table, as.Date(table$Date) >= as.Date("2007-03-18") &
                          as.Date(table$Date) <= as.Date("2007-03-24"))


#weekdays
table_weekday <- table_subset[which(weekdays(as.Date(table_subset$Date, format = "%m/%d/%Y"))
         %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]

table_weekend <- table_subset[which(weekdays(as.Date(table_subset$Date, format = "%m/%d/%Y"))
                           %in% c('Saturday','Sunday')), ]
