########################
#### DataCleaning.R ####
########################

### Raw data notes
# 1. crash location - 10-12 has four extra columns
# 2. vehicle - can merge
# 3. driver - can merge
# 4. passenger - 13-14 has one extra column
# 5. pedestrain - can merge
# 6. license - can merge
# 7. property damage - can merge
# 8. vehicle commercial - 10-12 and 13-14 have one extra column
# 9. uva-vt indicator - 11-14 has 4 extra columns
####### all can merge by CrashId #######

########################################
############ 1. Location ###############
########################################
# 2015 location data: latitude and longitude only have two decimal places
require(plyr)
location15 <- read.table('raw_dmv_data/1 - uva-crash-crashlocation 2015.txt',sep="\t",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
location14 <- read.table('raw_dmv_data/1-uva-crash-crashlocation-2014.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
location13 <- read.table('raw_dmv_data/1-uva-crash-crashlocation-2013.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
location10.12 <- read.table('raw_dmv_data/1 - uva-crash-crashlocation 2010-2012.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")

# 10-12 has 4 more columns #
location10.12 <- location10.12[,-c(11,12,16,20)] #delete extra columns
colnames(location10.12)[c(14,17,34,36)] <- c("RouteOrStreet","MileMarker","CrashTypeName","County.City.Town")
colnames(location15)[c(11,12,14,35,36,37)] <- c("GPSLatitude","GPSLongitude","RouteOrStreet","Jurisdiction","County.City.Town","County")
colnames(location13)[34] <- "CrashTypeName"
colnames(location14)[34] <- "CrashTypeName"
location13$CrashTypeName <- as.factor(location13$CrashTypeName)

# rename crashTypeName
levels(location13$CrashTypeName) <- c("Fatal Crash","Injury Crash","Property Damage Crash")
levels(location14$CrashTypeName) <- c("Fatal Crash","Injury Crash","Property Damage Crash")

# combine location
location <- rbind(location15, location14, location13, location10.12)
location <- location[,-c(15)]
detach("package:plyr", unload=TRUE) 
#sum(is.na(location))/(sum(is.na(location))+sum(!is.na(location))) #4.2% values missing!

########################################
############## 2. Vehicle ##############
########################################
# 2010 no vehicle year #
vehicle15 <- read.table('raw_dmv_data/2 - uva-vehicle 2015.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
vehicle13.14 <- read.table('raw_dmv_data/2-uva-vehicle 2013-2014.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
vehicle11.12 <- read.table('raw_dmv_data/2 - uva-vehicle 2011-2012.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
vehicle10 <- read.table('raw_dmv_data/2 - uva-vehicle 2010.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")

vehicle10$VehicleYear <- NA  #add NA for missing year
vehicle10 <- vehicle10[,c(1:4,30,5:29)] #reorder
vehicles <- rbind(vehicle15, vehicle13.14, vehicle11.12, vehicle10)

vehicle$VehicleMake <- toupper(vehicle$VehicleMake)
## clean up vehicle make -- NOT going to use
test <- c()
for (i in 1:nrow(vehicle)){
  if (grepl("^T[0O]+[Y]", vehicle$VehicleMake[i]) == TRUE){
    #test <- c(test, vehicle$VehicleMake[i])
    vehicle$VehicleMake[i] <- "TOYOTA"
  } else if (grepl("^C(H)?RY", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "CHRYSLER"
  } else if (grepl("^V+((OLK)|W)+", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "VW"
  } else if (grepl("^CH[E]?V", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "CHEVY"
  } else if (grepl("^LINC", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "LINOLN"
  } else if (grepl("(BENZ)|(MERC)", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "MERCEDES BENZ"
  } else if (grepl("^LEX", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "LEXUS"
  } else if (grepl("^NIS", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "NISSAN"
  } else if (grepl("^H(Y|U)+(N|(ND)|(DAI))?", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "HYUNDAI"
  } else if (grepl("^DOD", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "DODGE"
  } else if (grepl("^(VO)+(V)+", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "VOLVO"
  } else if (grepl("^BUI", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "BUICK"
  } else if (grepl("^MAZ", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "MAZDA"
  } else if (grepl("^MIT", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "MITSUBISHI"
  } else if (grepl("^(HD)|(HON)", vehicle$VehicleMake[i]) == TRUE){
    vehicle$VehicleMake[i] <- "HONDA"
  }
}

########################################
############### 3. Driver ##############
########################################
driver15 <- read.table('raw_dmv_data/3 - uva-driver 2015.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
driver13.14 <- read.table('raw_dmv_data/3-uva-driver 2013-2014.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
driver10.12 <- read.table('raw_dmv_data/3 -uva-driver 2010-2012.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
names(driver15)[7] <- "CommercialDriverLicenseID"
drivers <- rbind(driver15, driver13.14, driver10.12)


########################################
############# 4. Passenger #############
########################################
passenger15 <- read.table('raw_dmv_data/4 - uva-passenger 2015.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
passenger13.14 <- read.table('raw_dmv_data/4-uva-passenger 2013-2014.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
passenger10.12 <- read.table('raw_dmv_data/4 - uva-passsenger 2010 - 2012.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
passenger13.14 <- passenger13.14[,-c(5)]
passengers < - rbind(passenger15, passenger13.14, passenger10.12)

########################################
############# 5. Pedestrian ############
########################################
pedestrian15 <- read.table('raw_dmv_data/5 - uva-pedestrain2015.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
pedestrian13.14 <- read.table('raw_dmv_data/5-uva-pedestrian 2013-2014.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
pedestrian10.12 <- read.table('raw_dmv_data/5 - uva-pedestrian 2010 - 2012.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")

names(pedestrian10.12)[3] <- "CrashId"
pedestrian13.14 <- pedestrian13.14[,names(pedestrian15)]
pedestrian10.12 <- pedestrian10.12[,names(pedestrian15)]

########################################
############### 6. License #############
########################################
license15 <- read.table('raw_dmv_data/6 - uva-licenseclass 2015.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
license13.14 <- read.table('raw_dmv_data/6-uva-licenseclass 2013-2014.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
license10.12 <- read.table('raw_dmv_data/6 - uva-licenseclass 2010-2012.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
names(license15)[3] = "VehicleDriverID"

########################################
########### 7. PropertyDamage ##########
########################################
damage15 <- read.table('raw_dmv_data/7 - uva-propertydamage 2015.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
damage13.14 <- read.table('raw_dmv_data/7-uva-damageproperty 2013-2014.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
damage10.12 <- read.table('raw_dmv_data/7 - uva - PropertyDamage 2010-2012.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")

########################################
############# 8. Commercial ############
########################################
commercial15 <- read.table('raw_dmv_data/8 - uva-vehiclecommercial 2015.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
commercial13.14 <- read.table('raw_dmv_data/8-uva-vehiclecommercial 2013-2014.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
commercial10.12 <- read.table('raw_dmv_data/8 - uva-vehiclecommercial 2010-2012.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
commercial10.12 <- commercial10.12[,-c(9)]
commercial13.14 <- commercial13.14[,-c(9)]

########################################
############## 9. Indicator ############
########################################
indicator15 <- read.table('raw_dmv_data/9 - uva-vt-indicator 2015.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
# 11-14 has CrashDateTime LrgTruck OlderDriverInv MonthName
indicator11.14 <- read.table('raw_dmv_data/9-uva-vt-indicator-crashid 2011-2014.txt',sep="~",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")
indicator10 <- read.table('raw_dmv_data/9-uvavt--indicator 2010 11-08-13.txt',sep="\t",fill=TRUE,header=T,na.strings = c('',' ',NA),,quote="")

########################################
############### Merge data #############
########################################
driver.vehicle <- merge(vehicles, drivers, all.x = TRUE, by=c("CrashId","VehicleId","VehicleDriverId","CrashDate"))
str(driver.vehicle$CrashDate)
