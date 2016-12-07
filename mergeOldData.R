#setwd("/Users/tomoei/Dropbox/DMV project/")
#load("data/location.RData")
#load("data/drivers.RData")
#load("data/licenses.RData")
#load("data/commercial.RData")
#load("data/vehicles.RData")
#load("data/propertyDamage.RData")
#load("data/indicator.RData")
#load("data/pedestrian.RData")
#load("data/passengers.RData")
library(readr)
library(purrr)
library(dplyr)

## Read in CSV files from root directory
location <- read_csv("location.csv") %>% 
  select(-c(1))
drivers <- read_csv("drivers.csv") %>% 
  select(-c(1))
licenses <- read_csv("licenses.csv") %>% 
  select(-c(1))
commercial <- read_csv("commercial.csv") %>% 
  select(-c(1))
vehicles <- read_csv("vehicles.csv") %>% 
  select(-c(1))
propertyDamage <- read_csv("propertyDamage.csv") %>% 
  select(-c(1))
pedestrians <- read_csv("pedestrians.csv") %>% 
  select(-c(1))
passengers <- read_csv("passengers.csv") %>% 
  select(-c(1))

## Location
location$CrashId <- as.numeric(as.character(location$CrashId))
location$CrashDate <- as.Date(location$CrashDate, "%m/%d/%Y")
# delete obs with NA in every colunmn
# valid crash Id number -> 7 digits
location <- location[rowSums(!is.na(location)) != 0,]
length(unique(location$CrashId)) #728274 unique crashes total

## Drivers
drivers$CrashId <- as.numeric(as.character(drivers$CrashId))
drivers$CrashDate <- as.Date(drivers$CrashDate, "%m/%d/%Y")
# valid crash Id number -> 7 digits
drivers <- drivers[drivers$CrashId >= 1000000,]
length(unique(drivers$VehicleDriverId)) #1331396 unique drivers total

## Licenses
names(licenses)[3] <- "VehicleDriverId"
licenses$CrashId <- as.numeric(as.character(licenses$CrashId))
licenses$CrashDate <- as.Date(licenses$CrashDate, "%m/%d/%Y")
length(unique(licenses$VehicleDriverId)) #134334 unique license information total, does not match above number...

## Property Damage
propertyDamage$CrashId <- as.numeric(as.character(propertyDamage$CrashId))
propertyDamage$CrashDate <- as.Date(propertyDamage$CrashDate, "%m/%d/%Y")
propertyDamage$DamagedPropertyRepairCost <- propertyDamage$DamagedPropertyRepairCost %>% map(function(x){
  substring(x, 2) %>% 
    as.numeric() %>% 
    unlist()
})
#sum(unlist(propertyDamage$DamagedPropertyRepairCost), na.rm=TRUE)  < $$312,857,876 in damage

#indicator$CrashId <- as.numeric(as.character(indicator$CrashId))
#indicator$CrashDate <- as.Date(indicator$CrashDate, "%m/%d/%Y")

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
driver.license <- merge(drivers, licenses, all.x = TRUE, by=c("CrashId","VehicleDriverId", "CrashDate"))
#increased observation count because some drivers have two licenses

### 2. merge vehicle and commercial ###
vehicle.comm <- merge(vehicles, commercial, all.x = TRUE, by=c("CrashId","VehicleId", "CrashDate","VehicleCommercialID"))

### 3. merge location and property damage ###
location.pd <- merge(location, propertyDamage, all.x = TRUE, by=c("CrashId", "CrashDate"))

### 4. merge 3 and indicator ###
#merge <- merge(location.pd, indicator, all.x = TRUE, by=c("CrashId", "CrashDate","Year","CrashTypeName"))

### 5. merge 4&2 ###
merge <- merge(location.pd, vehicle.comm, all.x = TRUE, by=c("CrashId", "CrashDate"))
# gained almost 2x observations due to multiple vehicles per crash

### 6. merge 5&1 ###
final <- merge(merge, driver.license, all = TRUE, by = c("CrashId", "CrashDate", "VehicleId","VehicleDriverId"))
#  gained ~8000 more observations due to multiple drivers and licenses

#str(final)
#rm(list=setdiff(ls(), c("final","passengers","pedestrians")))
write.csv(final,"final_crash.csv")
