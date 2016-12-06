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
location <- location[rowSums(!is.na(location)) != 0,]

drivers$CrashId <- as.numeric(as.character(drivers$CrashId))
drivers$CrashDate <- as.Date(drivers$CrashDate, "%m/%d/%Y")
drivers <- drivers[drivers$CrashId >= 1000000,]

names(license)[3] <- "VehicleDriverId"
license$CrashId <- as.numeric(as.character(license$CrashId))
license$CrashDate <- as.Date(license$CrashDate, "%m/%d/%Y")

propertyDamage$CrashId <- as.numeric(as.character(propertyDamage$CrashId))
propertyDamage$CrashDate <- as.Date(propertyDamage$CrashDate, "%m/%d/%Y")

indicator$CrashId <- as.numeric(as.character(indicator$CrashId))
indicator$CrashDate <- as.Date(indicator$CrashDate, "%m/%d/%Y")

commercial$CrashId <- as.numeric(as.character(commercial$CrashId))
commercial$CrashDate <- as.Date(commercial$CrashDate, "%m/%d/%Y")

vehicles$CrashId <- as.numeric(as.character(vehicles$CrashId))
vehicles$CrashDate <- as.Date(vehicles$CrashDate, "%m/%d/%Y")

passengers$CrashId <- as.numeric(as.character(passengers$CrashId))
passengers$CrashDate <- as.Date(passengers$CrashDate, "%m/%d/%Y")

### !!!!! ####
pedestrians$CrashId <- as.numeric(as.character(pedestrians$CrashId))
pedestrians$CrashDate <- as.Date(pedestrians$CrashDate, "%m/%d/%Y")

driver.license <- merge(drivers, license, all.x = TRUE, by=c("CrashId","VehicleDriverId", "CrashDate"))
vehicle.driver <- merge(vehicles, driver.license, all.x = TRUE, by=c("CrashId","VehicleId", "CrashDate","VehicleDriverId"))
location.vehicles <- merge(location, vehicle.driver, all = TRUE, by = c("CrashId", "CrashDate"))
location.commercial <- merge(location.vehicles, commercial, all.x = TRUE, by=c("CrashId", "CrashDate","VehicleId","VehicleCommercialID"))
addPD <- merge(location.commercial, propertyDamage, all.x = TRUE, by=c("CrashId","CrashDate"))
