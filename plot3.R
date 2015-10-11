plot3 = function(){
  
  power = read.table("household_power_consumption.txt", header = T, sep = ";")
  actTime = paste(power$Date, power$Time, sep = " ")
  power$actTime = actTime
  actTime = strptime(actTime, format = "%d/%m/%Y %H:%M:%S")
  power$actTime = actTime
  powerSub = power[Date == '01/02/2007' || Date == '02/02/2007',]
  modDate = as.Date(power$Date, format = "%d/%m/%Y")
  power$Date = modDate
  powerSub = power[power$Date == as.Date("2007-02-01", format = "%Y-%m-%d") || power$Date == as.Date("2007-02-02", format = "%Y-%m-%d"), ]
  d1 = as.Date("2007-02-01", format = "%Y-%m-%d")
  d2 = as.Date("2007-02-02", format = "%Y-%m-%d")
  powerSub = power[power$Date == d1, ]
  powerSub2 = power[power$Date == d2,]
  powerSubF = rbind(powerSub, powerSub2)
  sub1 = as.numeric(levels(powerSubF$Sub_metering_1))[powerSubF$Sub_metering_1]
  sub2 = as.numeric(levels(powerSubF$Sub_metering_2))[powerSubF$Sub_metering_2]
  
  
  # return(powerSubF)
  powerSubF$Sub_metering_1 = sub1
  powerSubF$Sub_metering_2 = sub2
  #powerSubF$Sub_metering_3
  plot(powerSubF$actTime, powerSubF$Global_active_power, "n", ylab = "Energy sub metering", xlab = "",  ylim = c(0,40))
  # lines(powerSubF$actTime, powerSubF$Sub_metering_3, col = "blue")
  lines(powerSubF$actTime, powerSubF$Sub_metering_1, col = "black")
  lines(powerSubF$actTime, powerSubF$Sub_metering_2, col = "red")
  lines(powerSubF$actTime, powerSubF$Sub_metering_3, col = "blue")
  dev.copy(png, "plot3.png", width = 480, height = 480)
  dev.off()
}