require(dplyr)
require(tidyr)
# load in location data
load("/Users/tomoei/Dropbox/DMV project/data/location.Rdata")
location <- group_by(location, Year, CrashTypeName)
table(location$CrashTypeName)
crashType = c("Fatal Crash","Injury Crash","Property Damage Crash")

# plot number of crashes by year (three types in on graph)
by_year <- summarise(location, n=n())
by_year <- na.omit(by_year)
for (i in c(1:3)){
  par(new = TRUE)
  plot(by_year$Year[by_year$CrashTypeName == crashType[i]], by_year$n[by_year$CrashTypeName == crashType[i]],
       ylim = c(600,82000), ylab = "Number of Crashes", xlab = "Year",
       main = "Number of crash by year",
       pch = '*', type = "o", col = i)
}

par(mfrow=c(1,3))
for (i in c(1:3)){
  plot(by_year$Year[by_year$CrashTypeName == crashType[i]], by_year$n[by_year$CrashTypeName == crashType[i]],
       ylab = "Number of Crashes", xlab = "Year",
       main = "Number of crash by year",
       pch = '*', type = "o", col = i)
}

# plot by crash type and year
par(mfrow=c(1,3))
for (i in c(1:3)){
  plot(by_year$Year[by_year$CrashTypeName == crashType[i]], by_year$n[by_year$CrashTypeName == crashType[i]],
       xlab = "Year", ylab = "Number of crashes", main = crashType[i], col = i,
       pch = '*', type = "o")
}

# plot by month - fatal crash
location$CrashDate <- as.character(location$CrashDate)
location$month <- lapply(location$CrashDate, function(x) return(strsplit(x,"/")[[1]][1]))
location$month <- as.numeric(location$month)
location <- group_by(location, Year, month, CrashTypeName)
temp <- summarise(location, sum = n())
temp <- na.omit(temp)

par(mar = rep(2, 4))
par(mfrow=c(2,3))

for (i in c(2015:2010)){
  name  = paste(i, " Fatal Crash")
  plot(temp$month[temp$CrashTypeName == "Fatal Crash" & temp$Year == i], 
       temp$sum[temp$CrashTypeName == "Fatal Crash" & temp$Year == i],
       xlab = "Month", ylab = "Number of crashes", main = name,
       ylim = c(0,90), col = i-2005,
       pch = '*', type = 'o')
}

# plot by month - injury crash
par(mar = rep(2, 4))
par(mfrow=c(2,3))
for (i in c(2015:2010)){
  name  = paste(i, " Injury Crash")
  plot(temp$month[temp$CrashTypeName == "Injury Crash" & temp$Year == i], 
       temp$sum[temp$CrashTypeName == "Injury Crash" & temp$Year == i],
       xlab = "Month", ylab = "Number of crashes", main = name,col = i-2005,
       pch = '*', type = 'o')
}

# plot by month - property damage crash
par(mar = rep(2, 4))
par(mfrow=c(2,3))
for (i in c(2015:2010)){
  name  = paste(i, " Property Damage Crash")
  plot(temp$month[temp$CrashTypeName == "Property Damage Crash" & temp$Year == i], 
       temp$sum[temp$CrashTypeName == "Property Damage Crash" & temp$Year == i],
       xlab = "Month", ylab = "Number of crashes", main = name, col = i+5,
       pch = '*', type = 'o')
}


###### plot by location ######
# total crash of the year 
par(mar = rep(2, 4))
par(mfrow=c(2,3))
location <- location[location$GPSLatitude > 36 & location$GPSLatitude <= 40 & location$GPSLongitude >= -84 & location$GPSLongitude <= -75,]
for (i in c(2015:2010)){
  name  = paste(i, " Total Crash")
  plot(location$GPSLongitude[location$Year == i], location$GPSLatitude[location$Year == i],
       pch = '.', main = name,xlim = c(-83.5,-75), ylim = c(36.5,39.5),
       xlab = "longitude", ylab = "longitude")
}

# fatal crashes of the year
par(mar = rep(2, 4))
par(mfrow=c(2,3))
fatals <- na.omit(location[location$CrashTypeName == "Fatal Crash", c("GPSLatitude","GPSLongitude","Year")])
for (i in c(2015:2010)){
  name  = paste(i, " Fatal Crash")
  plot(fatals$GPSLongitude[fatals$Year == i], fatals$GPSLatitude[fatals$Year == i],
       pch = '.', main = name,
       xlab = "longitude", ylab = "longitude")
}

# Injury crashes of the year
par(mar = rep(2, 4))
par(mfrow=c(2,3))
fatals <- na.omit(location[location$CrashTypeName == "Injury Crash", c("GPSLatitude","GPSLongitude","Year")])
for (i in c(2015:2010)){
  name  = paste(i, " Injury Crash")
  plot(fatals$GPSLongitude[fatals$Year == i], fatals$GPSLatitude[fatals$Year == i],
       pch = '.', main = name,
       xlab = "longitude", ylab = "longitude")
}

# Property Damage crashes of the year
par(mar = rep(2, 4))
par(mfrow=c(2,3))
fatals <- location[location$CrashTypeName == "Property Damage Crash", c("GPSLatitude","GPSLongitude","Year")]
for (i in c(2015:2010)){
  name  = paste(i, " Property Damage Crash")
  plot(fatals$GPSLongitude[fatals$Year == i], fatals$GPSLatitude[fatals$Year == i],
       pch = '.', main = name,
       xlab = "longitude", ylab = "longitude")
}










