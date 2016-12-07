setwd("/Users/tomoei/Dropbox/DMV project/")
load("data/location.RData")
load("data/drivers.RData")
load("data/licenses.RData")
load("data/commercial.RData")
load("data/vehicles.RData")
load("data/propertyDamage.RData")
load("data/indicator.RData")
load("data/pedestrian.RData")
load("data/passengers.RData")


location$CrashId <- as.numeric(as.character(location$CrashId))
location$CrashDate <- as.Date(location$CrashDate, "%m/%d/%Y")
# delete obs with NA in every colunmn
# valid crash Id number -> 7 digits
location <- location[rowSums(!is.na(location)) != 0,]
length(unique(location$CrashId))

drivers$CrashId <- as.numeric(as.character(drivers$CrashId))
drivers$CrashDate <- as.Date(drivers$CrashDate, "%m/%d/%Y")
# valid crash Id number -> 7 digits
drivers <- drivers[drivers$CrashId >= 1000000,]
length(unique(drivers$VehicleDriverId))

names(license)[3] <- "VehicleDriverId"
license$CrashId <- as.numeric(as.character(license$CrashId))
license$CrashDate <- as.Date(license$CrashDate, "%m/%d/%Y")

propertyDamage$CrashId <- as.numeric(as.character(propertyDamage$CrashId))
propertyDamage$CrashDate <- as.Date(propertyDamage$CrashDate, "%m/%d/%Y")
cost <- sapply(propertyDamage$DamagedPropertyRepairCost[!is.na(propertyDamage$DamagedPropertyRepairCost)], function(x) as.numeric(substring(x, 2)))
sum(cost)

indicator$CrashId <- as.numeric(as.character(indicator$CrashId))
indicator$CrashDate <- as.Date(indicator$CrashDate, "%m/%d/%Y")

commercial$CrashId <- as.numeric(as.character(commercial$CrashId))
commercial$CrashDate <- as.Date(commercial$CrashDate, "%m/%d/%Y")

vehicles$CrashId <- as.numeric(as.character(vehicles$CrashId))
vehicles$CrashDate <- as.Date(vehicles$CrashDate, "%m/%d/%Y")

passengers$CrashId <- as.numeric(as.character(passengers$CrashId))
passengers$CrashDate <- as.Date(passengers$CrashDate, "%m/%d/%Y")

pedestrians$CrashId <- as.numeric(as.character(pedestrians$CrashId))
pedestrians$CrashDate <- as.Date(pedestrians$CrashDate, "%m/%d/%Y")
pedestrians <- pedestrians[pedestrians$CrashId >= 1000000,]

### 1. merge driver and license ###
driver.license <- merge(drivers, license, all.x = TRUE, by=c("CrashId","VehicleDriverId", "CrashDate"))
### 2. merge vehicle and commercial ###
vehicle.comm <- merge(vehicles, commercial, all.x = TRUE, by=c("CrashId","VehicleId", "CrashDate","VehicleCommercialID"))
### 3. merge location and property damage ###
location.pd <- merge(location, propertyDamage, all.x = TRUE, by=c("CrashId", "CrashDate"))
### 4. merge 3 and indicator ###
merge <- merge(location.pd, indicator, all.x = TRUE, by=c("CrashId", "CrashDate","Year","CrashTypeName"))
### 5. merge 4&2 ###
merge <- merge(merge, vehicle.comm, all.x = TRUE, by=c("CrashId", "CrashDate"))
### 6. merge 5&1 ###
final <- merge(merge, driver.license, all = TRUE, by = c("CrashId", "CrashDate", "VehicleId","VehicleDriverId"))
str(final)
rm(list=setdiff(ls(), c("final","passengers","pedestrians")))
