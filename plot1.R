plot1 = function(){
  
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
  GB = as.numeric(levels(powerSubF$Global_active_power))[powerSubF$Global_active_power]
  powerSubF$Global_active_power = GB
  hist(powerSubF$Global_active_power, col = "red", xlab = "Global Active Power(kilowatts)", ylab = "Frequency", main  = "Global Active Power")
  dev.copy(png, "plot1.png", width = 480, height = 480)
  dev.off()
  }