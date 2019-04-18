library(jsonlite)

#Setting Working directory for files
setwd("~/Desktop/IOT")

#Reading files and cleansing
#JSON File with forecast
forecast <- fromJSON("forecast.json")
forecast <- forecast$list
forecast <- cbind(forecast,do.call("rbind", forecast$weather))

#Lighting information and removal of duplicates
light_z1 <- read.csv(file="babd_light_z1.csv", header=TRUE, sep="\t")
light_z2 <- read.csv(file="babd_light_z2.csv", header=TRUE, sep="\t")
light_z3 <- read.csv(file="babd_light_z3.csv", header=TRUE, sep="\t")
light_z3A <- read.csv(file="babd_light_z3A.csv", header=TRUE, sep="\t")
light_z1 <- light_z1[!duplicated(light_z1$timestamp),]
light_z2 <- light_z2[!duplicated(light_z2$timestamp),]
light_z3 <- light_z3[!duplicated(light_z3$timestamp),]
light_z3A <- light_z3A[!duplicated(light_z3A$timestamp),]

#Movement information and removal of duplicates
movement_z1 <- read.csv(file="babd_movement_z1.csv", header=TRUE, sep="\t")
movement_z2 <- read.csv(file="babd_movement_z2.csv", header=TRUE, sep="\t")
movement_z3 <- read.csv(file="babd_movement_z3.csv", header=TRUE, sep="\t")
movement_z3A <- read.csv(file="babd_movement_z3A.csv", header=TRUE, sep="\t")
movement_z1 <- movement_z1[!duplicated(movement_z1$timestamp),]
movement_z2 <- movement_z2[!duplicated(movement_z2$timestamp),]
movement_z3 <- movement_z3[!duplicated(movement_z3$timestamp),]
movement_z3A <- movement_z3A[!duplicated(movement_z3A$timestamp),]

#Select only columns of interest from each dataset and meger into one dataframe since
light_z1 <- light_z1[, c("timestamp","zone", "value")]
light_z2 <- light_z2[, c("timestamp","zone", "value")]
light_z3 <- light_z3[, c("timestamp","zone", "value")]
light_z3A <- light_z3A[, c("timestamp","zone", "value")]
light <- rbind(light_z1, light_z2, light_z3, light_z3A)

movement_z1 <- movement_z1[, c("timestamp","zone", "n_passages")]
movement_z2 <- movement_z2[, c("timestamp","zone", "n_passages")]
movement_z3 <- movement_z3[, c("timestamp","zone", "n_passages")]
movement_z3A <- movement_z3A[, c("timestamp","zone", "n_passages")]
movement <- rbind(movement_z1, movement_z2, movement_z3, movement_z3A)

forecast <- forecast[, c("dt_txt","clouds")]
names(forecast)[1] <- "timestamp"
names(forecast)[2] <- "clouds"

#Obtain Hours and date to group values from light and movement
light$Date <- as.Date(light$timestamp)
light$Time <- format(as.POSIXct(light$timestamp) ,format = "%H:%M:%S")
light$Hour <- format(as.POSIXct(light$Time, format = "%H:%M:%S"),"%H")
light$Day <- weekdays(light$Date)

movement$Date <- as.Date(movement$timestamp)
movement$Time <- format(as.POSIXct(movement$timestamp) ,format = "%H:%M:%S")
movement$Hour <- format(as.POSIXct(movement$Time, format = "%H:%M:%S"),"%H")
movement$Day <- weekdays(movement$Date)

forecast$Date <- as.Date(forecast$timestamp)
forecast$Time <- format(as.POSIXct(forecast$timestamp) ,format = "%H:%M:%S")
forecast$Hour <- format(as.POSIXct(forecast$Time, format = "%H:%M:%S"),"%H")
forecast$Day <- weekdays(forecast$Date)

#Aggregate light information by hour using the mean
agg_light = aggregate(light$value,
                by = list(light$Hour),
                FUN = mean)

barplot(agg_light$x, main="Light", xlab="Hour",  
        ylab="Total", names.arg=agg_light$Group.1, 
        border="blue")

#Aggregate movement information by hour using the mean
agg_movement = aggregate(movement$n_passages,
                      by = list(movement$Hour),
                      FUN = mean)

barplot(agg_movement$x, main="Light", xlab="Hour",  
        ylab="Total", names.arg=agg_movement$Group.1, 
        border="blue")

#Create dataframe to be used in function containing forecasted_lux, hour and day
#Convert hour to numeric
light$Hour <- as.numeric(light$Hour)
forecast$Hour <- as.numeric(forecast$Hour)
forecast$clouds <- as.numeric(forecast$clouds$all)
forecast$Hour[forecast$Hour == 6 ] <- 7

#Filter data to include weekdays and work hours
light <- subset(light, light$Hour >= 7 & light$Hour <= 20 & (light$Day != "Saturday" & light$Day != "Sunday"), select = c(value,Hour,Day))
forecast <- subset(forecast, forecast$Hour >= 7 & forecast$Hour <= 20, select = c(clouds,Hour,Day))

#Fill values to complete hours and weekdays
Hour <- c(8,10,11,13,14,16,17,19,20)
clouds <- c(0,8,8,44,44,68,68,48,48)
Day <- c("Monday","Monday","Monday","Monday","Monday","Monday","Monday","Monday","Monday")
forecast_complete <- data.frame(clouds,Hour,Day)
clouds <- c(56,68,68,76,76,76,76,80,80)
Day <- c("Tuesday","Tuesday","Tuesday","Tuesday","Tuesday","Tuesday","Tuesday","Tuesday","Tuesday")
forecast_complete <- rbind(forecast_complete,data.frame(clouds, Hour, Day))
clouds <- c(36,68,68,76,76,92,92,100,100)
Day <- c("Wednesday","Wednesday","Wednesday","Wednesday","Wednesday","Wednesday","Wednesday","Wednesday","Wednesday")
forecast_complete <- rbind(forecast_complete,data.frame(clouds, Hour, Day))
clouds <- c(92,92,92,92,92,92,92,92,92)
Day <- c("Thursday","Thursday","Thursday","Thursday","Thursday","Thursday","Thursday","Thursday","Thursday")
forecast_complete <- rbind(forecast_complete,data.frame(clouds, Hour, Day))
clouds <- c(12,56,56,80,80,56,56,80,80)
Day <- c("Friday","Friday","Friday","Friday","Friday","Friday","Friday","Friday","Friday")
forecast_complete <- rbind(forecast_complete,data.frame(clouds, Hour, Day))
forecast_final <- rbind(forecast_complete,forecast)

agg_light = aggregate(light$value,by = list(light$Hour,light$Day),FUN = mean)
names(agg_light)[names(agg_light) == "Group.1"] <- "Hour"
names(agg_light)[names(agg_light) == "Group.2"] <- "Day"
names(agg_light)[names(agg_light) == "x"] <- "light"

#Merge forecast and Light distribution
Lux <- merge(forecast_final,agg_light, by=c("Hour","Day"))
#Calculate Forecasted lux
Lux$Forecasted <- (1 - (Lux$clouds*0.01) * 0.40) * Lux$light
#Select relevant variables and change days and hours for loop function
Forecasted_Lux <- Lux[, c("Forecasted","Hour", "Day")]
Forecasted_Lux$Day <- as.character(Forecasted_Lux$Day)
Forecasted_Lux$Day[Forecasted_Lux$Day == "Monday" ] <- "1"
Forecasted_Lux$Day[Forecasted_Lux$Day == "Tuesday" ] <- "2"
Forecasted_Lux$Day[Forecasted_Lux$Day == "Wednesday" ] <- "3"
Forecasted_Lux$Day[Forecasted_Lux$Day == "Thursday" ] <- "4"
Forecasted_Lux$Day[Forecasted_Lux$Day == "Friday" ] <- "5"
Forecasted_Lux$Day <- as.numeric(Forecasted_Lux$Day)
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 7] <- 1
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 8] <- 2
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 9] <- 3
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 10] <- 4
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 11] <- 5
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 12] <- 6
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 13] <- 7
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 14] <- 8
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 15] <- 9
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 16] <- 10
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 17] <- 11
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 18] <- 12
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 19] <- 13
Forecasted_Lux$Hour[Forecasted_Lux$Hour == 20] <- 14
