library(ks)
library(RColorBrewer)
library(zoo)
library(rgdal)
library(maptools)
library(lubridate)
library(rgdal)
library(ggplot2)
setwd("/Users/tomoei/Downloads/tl_2013_51_cousub/")
load("/Users/tomoei/Desktop/location.RData")

library(dplyr)
finalbyid <- group_by(final[, c("Year","CrashId","GPSLatitude","GPSLongitude", "CrashTypeName")], CrashId)
finalbyid <- unique(finalbyid)
finalbyid <- finalbyid[!is.na(finalbyid$GPSLatitude) & finalbyid$GPSLatitude > 30 & finalbyid$GPSLongitude < -75,]

library(rworldmap)
library(ggmap)
loc2014 <- finalbyid[finalbyid$Year == 2014 & !is.na(finalbyid$CrashTypeName),]
map <- get_map(location = "Virginia", maptype = "roadmap", zoom = 6)
ggmap(map) +
  geom_polygon(data=subset(map_data("state"), region %in% regions), aes(x=long, y=lat, group=group), col="black", fill=NA) + 
  geom_point(data = loc2014, aes(x = GPSLongitude, y = GPSLatitude), alpha = .5, size = .05, col = "red") 

fatal <- loc2014[loc2014$CrashTypeName == "Fatal Crash" & !is.na(loc2014$CrashTypeName), ]
map <- get_map(location = "Virginia", maptype = "roadmap", zoom = 6)
ggmap(map) +
  geom_polygon(data=subset(map_data("state"), region %in% regions), aes(x=long, y=lat, group=group), col="black", fill=NA) + 
  geom_point(data = fatal, aes(x = GPSLongitude, y = GPSLatitude), alpha = .5, size = 1, col = "red") 

injury <- loc2014[loc2014$CrashTypeName == "Injury Crash" & !is.na(loc2014$CrashTypeName), ]
map <- get_map(location = "Virginia", maptype = "roadmap", zoom = 6)
ggmap(map) +
  geom_polygon(data=subset(map_data("state"), region %in% regions), aes(x=long, y=lat, group=group), col="black", fill=NA) + 
  geom_point(data = injury, aes(x = GPSLongitude, y = GPSLatitude), alpha = .5, size = .05, col = "red") 

pd <- loc2014[loc2014$CrashTypeName == "Property Damage Crash" & !is.na(loc2014$CrashTypeName),]
map <- get_map(location = "Virginia", maptype = "roadmap", zoom = 6)
ggmap(map) +
  geom_polygon(data=subset(map_data("state"), region %in% regions), aes(x=long, y=lat, group=group), col="black", fill=NA) + 
  geom_point(data = pd, aes(x = GPSLongitude, y = GPSLatitude), alpha = .5, size = .05, col = "red") 

