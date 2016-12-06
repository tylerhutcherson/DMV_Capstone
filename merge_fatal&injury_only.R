setwd("/Users/tomoei/Dropbox/DMV project/data2016/")
# read txt files
location <- read.table("1-crash-crashlocation.txt",sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
vehicle <- read.table("2-crashvehicle.txt",sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
driver <- read.table("3-driverdata.txt",sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
passenger <- read.table("4-vehiclepassenger.txt",sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
conviction <- read.table("5-conviction data.txt",sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
indicator <- read.table("6-crash indicator file 2010-2015.txt",sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
names(indicator)[1] <- "CrashId"
names(passenger)[1] <- "CrashId"
names(driver)[c(3,4)] <- c("CrashId","VehicleId")

# merge data frames
driver.vehicle <- merge(vehicle, driver, all.x = TRUE, all.y = TRUE, by=c("VehicleDriverId", "CrashId", "VehicleId"))
location.add <- merge(location, driver.vehicle, all.x = TRUE, all.y = TRUE, by=c("CrashId"))
indicator.add <- merge(location.add, indicator, all.x = TRUE, all.y = TRUE, by=c("CrashId","CrashDate"))
final <- merge(passenger, indicator.add, all.x = TRUE, all.y = TRUE, by=c("CrashId","VehicleId"))




