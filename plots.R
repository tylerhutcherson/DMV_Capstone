require(dplyr)
require(tidyr)
require(ggplot2)
require(purrr)
require(scales)

## load in location data
#load("/Users/tomoei/Dropbox/DMV project/data/location.Rdata")
location <- read.csv("location.csv") %>% 
  group_by(Year, CrashTypeName)
table(location$CrashTypeName)
crashType = c("Fatal Crash","Injury Crash","Property Damage Crash", NA)
#sum(is.na(location$CrashTypeName))   121865 unknown crash types (NA)

## load in property damage data
rates <- c(1.11,1.07,1.05,1.04,1.02,1.02)
propertyDamage <- read.csv("propertyDamage.csv") %>% 
  mutate(CrashDate = as.Date(CrashDate,"%m/%d/%Y")) %>% 
  mutate(Year = CrashDate %>% format('%Y')) %>%
  mutate(DamagedPropertyRepairCost = DamagedPropertyRepairCost %>% map(function(x){
    substring(x, 2) %>% 
      as.numeric() 
  }) %>% unlist()) 

#adjust for inflation
for (i in (1:nrow(propertyDamage))){
  if (is.na(propertyDamage$Year[i])){
    next()
  }
  if (propertyDamage$Year[i] == "2010"){
    propertyDamage$DamagedPropertyRepairCost[i] = propertyDamage$DamagedPropertyRepairCost[i]*rates[1]
  }
  if (propertyDamage$Year[i] == "2011"){
    propertyDamage$DamagedPropertyRepairCost[i] = propertyDamage$DamagedPropertyRepairCost[i]*rates[2]
  }
  if (propertyDamage$Year[i] == "2012"){
    propertyDamage$DamagedPropertyRepairCost[i] = propertyDamage$DamagedPropertyRepairCost[i]*rates[3]
  }
  if (propertyDamage$Year[i] == "2013"){
    propertyDamage$DamagedPropertyRepairCost[i] = propertyDamage$DamagedPropertyRepairCost[i]*rates[4]
  }
  if (propertyDamage$Year[i] == "2014"){
    propertyDamage$DamagedPropertyRepairCost[i] = propertyDamage$DamagedPropertyRepairCost[i]*rates[5]
  }
  if (propertyDamage$Year[i] == "2015"){
    propertyDamage$DamagedPropertyRepairCost[i] = propertyDamage$DamagedPropertyRepairCost[i]*rates[6]
  }
}

#write back out to save the updated version of the data
write.csv(propertyDamage,"propertyDamage.csv")

# group by Year
cost_by_year <- group_by(propertyDamage,Year) %>% 
  summarise(cost=sum(as.numeric(DamagedPropertyRepairCost),na.rm=TRUE), 
            max_cost=max(as.numeric(DamagedPropertyRepairCost),na.rm=TRUE))

#############################################################################
#### Scatter plot for Property Damage Cost per year
theme <- theme(                              
  axis.title.x = element_text(face="bold", color="black", size=14, family ="serif"),
  axis.title.y = element_text(face="bold", color="black", size=14, family ="serif"),
  plot.title = element_text(face="bold", color = "black", size=16, family ="serif")
)

damage_cost <- ggplot(data=cost_by_year, mapping=aes(x=Year, y=cost)) +
  geom_point(aes(size=max_cost)) +
  scale_size_continuous(name ="Max Damage Cost Per Year", range=c(0.5,15), breaks=c(2500000,5000000,7500000), labels=c("$2,500,000","$5,000,000","$7,500,000")) +
  theme_bw() + theme +
  labs(title="Property Damage Cost Per Year", x="Year", y="Total Property Damage Cost") +
  scale_y_continuous(limits = c(0,100000000), breaks=c(0,25000000,50000000,75000000,100000000), labels=c("$0","$25,000,000","$50,000,000","$75,000,000","$100,000,000"))
  
 
#### Line plot for total crashes per year
crashes <- read.csv("location.csv") %>% 
  group_by(Year) %>% 
  summarise(total_crashes = length(unique(CrashId))) %>% 
  merge(cost_by_year, by = "Year")
  
crash_count <- ggplot(data=crashes, mapping=aes(x=total_crashes,y=cost)) +
  geom_point(colour="black") +
  xlim(c(100000,140000))
  
#by_year <- summarise(location, n=n())
#by_year <- na.omit(by_year)
# plot number of crashes by year (three types in on graph)
#for (i in c(1:3)){
#  par(new = TRUE)
#  plot(by_year$Year[by_year$CrashTypeName == crashType[i]], by_year$n[by_year$CrashTypeName == crashType[i]],
#       ylim = c(600,82000), ylab = "Number of Crashes", xlab = "Year",
#       main = "Number of crash by year",
#       pch = '*', type = "o", col = i)
#}

#par(mfrow=c(1,3))
#for (i in c(1:3)){
#  plot(by_year$Year[by_year$CrashTypeName == crashType[i]], by_year$n[by_year$CrashTypeName == crashType[i]],
#       ylab = "Number of Crashes", xlab = "Year",
#       main = "Number of crash by year",
#       pch = '*', type = "o", col = i)
#}

# plot by crash type and year (each one individual but on same row)
#par(mfrow=c(1,3))
#for (i in c(1:3)){
#  plot(by_year$Year[by_year$CrashTypeName == crashType[i]], by_year$n[by_year$CrashTypeName == crashType[i]],
#       xlab = "Year", ylab = "Number of crashes", main = crashType[i], col = i,
#       pch = '*', type = "o")
#}

# plot by month - fatal crash
location$CrashDate <- as.character(location$CrashDate)
location$month <- lapply(location$CrashDate, function(x) return(strsplit(x,"/")[[1]][1]))
location$month <- as.numeric(location$month)
#location2 <- group_by(location, Year, month, CrashTypeName)
#temp <- summarise(location2, sum = n())
#temp <- na.omit(temp)

#par(mar = rep(2, 4))
#par(mfrow=c(2,3))

#for (i in c(2010:2015)){
#  name  = paste(i, " Fatal Crash")
#  plot(temp$month[temp$CrashTypeName == "Fatal Crash" & temp$Year == i], 
#       temp$sum[temp$CrashTypeName == "Fatal Crash" & temp$Year == i],
#       xlab = "Month", ylab = "Number of crashes", main = name,
#       ylim = c(0,90), col = i-2005,
#       pch = '*', type = 'o')
#}

# plot by month - injury crash
#par(mar = rep(2, 4))
#par(mfrow=c(2,3))
#for (i in c(2015:2010)){
#  name  = paste(i, " Injury Crash")
#  plot(temp$month[temp$CrashTypeName == "Injury Crash" & temp$Year == i], 
#       temp$sum[temp$CrashTypeName == "Injury Crash" & temp$Year == i],
#       xlab = "Month", ylab = "Number of crashes", main = name,col = i-2005,
#       pch = '*', type = 'o')
#}

# plot by month - property damage crash
#par(mar = rep(2, 4))
#par(mfrow=c(2,3))
#for (i in c(2015:2010)){
#  name  = paste(i, " Property Damage Crash")
#  plot(temp$month[temp$CrashTypeName == "Property Damage Crash" & temp$Year == i], 
#       temp$sum[temp$CrashTypeName == "Property Damage Crash" & temp$Year == i],
#       xlab = "Month", ylab = "Number of crashes", main = name, col = i+5,
#       pch = '*', type = 'o')
#}







######### plot by location #########
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










