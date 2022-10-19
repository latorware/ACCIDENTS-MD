
#PREPROCESSING SCRIPT

#CHECKS IF IT IS NECESSARY TO INSTALL ANY PACKAGE
list.of.packages <- c("rstudioapi","remotes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("rstudioapi")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dd <- read.csv("Accidents5000Reduit.csv", header=T); #Reading the .csv file that has our dataset with the selected variables and individuals


#Changing names of features
names(dd)[names(dd) == "zona"] <- "Zone"
names(dd)[names(dd) == "dat"] <- "Date"
names(dd)[names(dd) == "nomCom"] <- "Region"
names(dd)[names(dd) == "nomDem"] <- "Prov"
names(dd)[names(dd) == "F_MORTS"] <- "nMortal"
names(dd)[names(dd) == "F_FERITS_GREUS"] <- "nGraveInj"
names(dd)[names(dd) == "F_FERITS_LLEUS"] <- "nMinorInj"
names(dd)[names(dd) == "F_UNITATS_IMPLICADES"] <- "nInvolv"
names(dd)[names(dd) == "F_VIANANTS_IMPLICADES"] <- "nPedest"
names(dd)[names(dd) == "F_BICICLETES_IMPLICADES"] <- "nBikes"
names(dd)[names(dd) == "VEHICLES_MOTOR"] <- "nMotor"
names(dd)[names(dd) == "C_VELOCITAT_VIA"] <- "Vel"
names(dd)[names(dd) == "D_ACC_AMB_FUGA"] <- "Escaped"
names(dd)[names(dd) == "D_CLIMATOLOGIA"] <- "Weather"
names(dd)[names(dd) == "D_INFLUIT_CIRCULACIO"] <- "TrafficInf"
names(dd)[names(dd) == "D_INFLUIT_ESTAT_CLIMA"] <- "WeatherInf"
names(dd)[names(dd) == "D_INFLUIT_LLUMINOSITAT"] <- "LightInf"
names(dd)[names(dd) == "D_INFLUIT_VISIBILITAT"] <- "VisionInf"
names(dd)[names(dd) == "D_INTER_SECCIO"] <- "Intersect"
names(dd)[names(dd) == "D_SUPERFICIE"] <- "Surface"
names(dd)[names(dd) == "grupDiaLab"] <- "DayGroup"
names(dd)[names(dd) == "grupHor"] <- "HourGroup"
names(dd)[names(dd) == "tipAcc"] <- "AccType"

attach(dd)


#declaring the qualitative variables as factors
sapply(dd, class)
Zone     <- factor(Zone)
Region     <- factor(Region)
Prov     <- factor(Prov)
Vel     <- factor(Vel)
Escaped     <- factor(Escaped)
Weather     <- factor(Weather)
TrafficInf     <- factor(TrafficInf)
WeatherInf     <- factor(WeatherInf)
LightInf     <- factor(LightInf)
VisionInf     <- factor(VisionInf)
Intersect     <- factor(Intersect)
Surface     <- factor(Surface)
DayGroup     <- factor(DayGroup)
HourGroup     <- factor(HourGroup)
AccType     <- factor(AccType)

#processing the levels of all qualitative variables (changing its names, ordering, etc.)

summary(Zone)
#pie(table(Zone))
#barplot(table(Zone))
levels(Zone) <- c("Road", "Urban")
summary(Zone)



summary(Region)
levels(Region)[22] <- "Moianes"
dd[,3]<-Region
table(dd$Region)
tochange<- PREPROCESSINGjoin <- read.csv("PREPROCESSINGjoin.csv", sep=";")  #because of the number of regions, the name change is done by using the tochange function
dd$Region<-tochange$Short[match(dd$Region,tochange$Long)] 
Region     <- factor(dd$Region)
#pie(table(Region))
#barplot(table(Region))
summary(Region)

summary(Prov)
#pie(table(Prov))
#barplot(table(Prov))
summary(Prov)

summary(Vel)
#pie(table(Vel))
#barplot(table(Vel))
summary(Vel)

summary(Escaped)
#pie(table(Escaped))
#barplot(table(Escaped))
levels(Escaped) <- c("No", "NA", "Yes")
#Ordering levels: 
Escaped <- factor(Escaped, ordered=TRUE, levels=c('Yes', 'No', 'NA'))
summary(Escaped)

summary(Weather)
#pie(table(Weather))
#barplot(table(Weather))
levels(Weather) <- c("Good", "Snow", "WeakRain", "StrongRain", "NA")
#Ordering levels:
Weather <- factor(Weather, ordered=TRUE, levels=c('Good', 'WeakRain', 'StrongRain', 'Snow', 'NA'))
summary(Weather)

summary(TrafficInf)
#pie(table(TrafficInf))
#barplot(table(TrafficInf))
levels(TrafficInf) <- c("No", "NA", "Yes")
#Ordering levels: 
TrafficInf <- factor(TrafficInf, ordered=TRUE, levels=c('Yes', 'No', 'NA'))
summary(TrafficInf)

summary(WeatherInf)
#pie(table(WeatherInf))
#barplot(table(WeatherInf))
levels(WeatherInf) <- c("No", "NA", "Yes")
#Ordering levels: 
WeatherInf <- factor(WeatherInf, ordered=TRUE, levels=c('Yes', 'No', 'NA'))
summary(WeatherInf)

summary(LightInf)
#pie(table(LightInf))
#barplot(table(LightInf))
levels(LightInf) <- c("No", "NA", "Yes")
#Ordering levels: 
LightInf <- factor(LightInf, ordered=TRUE, levels=c('Yes', 'No', 'NA'))
summary(LightInf)

summary(VisionInf)
#pie(table(VisionInf))
#barplot(table(VisionInf))
levels(VisionInf) <- c("No", "NA", "Yes")
#Ordering levels: 
VisionInf <- factor(VisionInf, ordered=TRUE, levels=c('Yes', 'No', 'NA'))
summary(VisionInf)

summary(Intersect)
#pie(table(Intersect))
#barplot(table(Intersect))
levels(Intersect) <- c("Arriving", "Inside", "InSection")
#Ordering levels: 
Intersect <- factor(Intersect, ordered=TRUE, levels=c('InSection', 'Arriving', 'Inside'))
summary(Intersect)

summary(DayGroup)
#pie(table(DayGroup))
#barplot(table(DayGroup))
levels(DayGroup) <- c("Weekend", "Weekday")
#Ordering levels: 
DayGroup <- factor(DayGroup, ordered=TRUE, levels=c('Weekday', 'Weekend'))
summary(DayGroup)

summary(Surface)
#pie(table(Surface))
#barplot(table(Surface))
levels(Surface) <- c("Icy", "Flooded", "Wet", "Snowy", "Slippery", "Dry&Clean", "NA")
#Ordering levels: 
Surface <- factor(Surface, ordered=TRUE, levels=c("Dry&Clean", "Slippery", "Wet", "Flooded", "Icy", "Snowy", "NA"))
summary(Surface)

summary(HourGroup)
#pie(table(HourGroup))
#barplot(table(HourGroup))
levels(HourGroup) <- c("Morning", "Night", "Afternoon")
#Ordering levels: 
HourGroup <- factor(HourGroup, ordered=TRUE, levels=c('Morning', 'Afternoon', 'Night'))
summary(HourGroup)

summary(AccType)
#pie(table(AccType))
#barplot(table(AccType))
levels(AccType) <- c("Other", "RunOver", "Rollover", "HitObstacle", "HitVehicle/s", "NARoadExit")
#Ordering levels: 
AccType <- factor(AccType, ordered=TRUE, levels=c('RunOver', 'Rollover', 'HitObstacle', 'HitVehicle/s', 'NARoadExit', 'Other'))
summary(AccType)


#declaing dates
Date     <- as.Date(Date, format =  "%d/%m/%Y")
Date

#creating new variables from the date variabl. Declaring them as factors and ordered.

#year
Year<-format(Date, "%Y")
Year
Year <- factor(Year)
Year <- factor(Year, ordered=TRUE)
summary(Year)


#month
Month<-format(Date,"%m")
Month
Month<- factor(Month)
levels(Month) <- c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.")
Month <- factor(Month, ordered=TRUE, levels=c('Jan.', 'Feb.', 'March', 'April', 'May', 'June', 'July', 'Aug.', 'Sept.', 'Oct.', 'Nov.', 'Dec.'))
summary(Month)


#applying changes to the table dd

dd[,1]<-Zone
dd[,3]<-Region
dd[,4]<-Prov
dd[,12]<-Vel
dd[,13]<-Escaped
dd[,14]<-Weather
dd[,15]<-TrafficInf
dd[,16]<-WeatherInf
dd[,17]<-LightInf
dd[,18]<-VisionInf
dd[,19]<-Intersect
dd[,20]<-Surface
dd[,21]<-DayGroup
dd[,22]<-HourGroup
dd[,23]<-AccType
dd[,2]<-Date
dd$Month<-Month
dd$Year<-Year


sapply(dd, class)
sapply(dd, levels)


#saving the resulting rds from the previous steps in order to execute the descriptive analysis with raw variables (before treating missings and outliers).
#we are saving the file as an rds instead of a csv because we don't want to declare factors again.
saveRDS(dd, file= "before_missings.rds")

#MISSING TREATMENT

# Observing which variables have missing values or outliers that have to be treated, and treating them.
table(dd$Zone)
table(dd$Date)
table(dd$Region)
table(dd$Prov)
table(dd$nMortal)
table(dd$nGraveInj)
table(dd$nMinorInj)
table(dd$nInv)
table(dd$nPedest)
table(dd$nBikes)
table(dd$nMotor)


table(dd$Vel)
#table vel has missing values that are found in three different forms : "NA", "999" and "0". Switching them for "UnkVel"

dd$Vel <- factor(dd$Vel, ordered=TRUE, levels=c(levels(dd$Vel), 'UnkVel')) #Adds 'UnkVel' level
dd$Vel[dd$Vel==999 |dd$Vel==0 | is.na(dd$Vel)]  <- 'UnkVel' #Moves vel's where it's value is 999 or 0 or NA to 'UnkVel'
dd$Vel <- factor(dd$Vel, ordered=TRUE, levels=levels(droplevels(dd$Vel))) #Removes 10 and 999 factors since there aren't used
table(dd$Vel)


table(dd$Escaped)
#table Escaped has missing values that are found as "NA". Switching them for "UnkEsc"

dd$Escaped <- factor(dd$Escaped, ordered=TRUE, levels=c(levels(dd$Escaped), 'UnkEsc')) #Adds 'UnkEsc' level
dd$Escaped[dd$Escaped == "NA"] <- 'UnkEsc'#Moves vel's where it's value is NA to 'UnkEsc'
dd$Escaped <- factor(dd$Escaped, ordered=TRUE, levels=levels(droplevels(dd$Escaped))) #Removes NA factor
table(dd$Escaped)


#Next missing treatments are the same as Escaped column

table(dd$Weather)
#table Weather has missing values that are found as "NA". Switching them for "UnkWth"

dd$Weather <- factor(dd$Weather, ordered=TRUE, levels=c(levels(dd$Weather), 'UnkWth')) 
dd$Weather[dd$Weather == "NA"] <- 'UnkWth'
dd$Weather <- factor(dd$Weather, ordered=TRUE, levels=levels(droplevels(dd$Weather))) 
table(dd$Weather)


table(dd$TrafficInf)
#table TrafficInf has missing values that are found as "NA". Switching them for "UnkTrafInf"

dd$TrafficInf <- factor(dd$TrafficInf, ordered=TRUE, levels=c(levels(dd$TrafficInf), 'UnkTrafInf')) 
dd$TrafficInf[dd$TrafficInf == "NA"] <- 'UnkTrafInf'
dd$TrafficInf <- factor(dd$TrafficInf, ordered=TRUE, levels=levels(droplevels(dd$TrafficInf))) 
table(dd$TrafficInf)


table(dd$WeatherInf)
#table WeatherInf has missing values that are found as "NA". Switching them for "UnkWthInf"

dd$WeatherInf <- factor(dd$WeatherInf, ordered=TRUE, levels=c(levels(dd$WeatherInf), 'UnkWthInf')) 
dd$WeatherInf[dd$WeatherInf == "NA"] <- 'UnkWthInf'
dd$WeatherInf <- factor(dd$WeatherInf, ordered=TRUE, levels=levels(droplevels(dd$WeatherInf))) 
table(dd$WeatherInf)


table(dd$LightInf)
#table LightInf has missing values that are found as "NA". Switching them for "UnkLightInf"

dd$LightInf <- factor(dd$LightInf, ordered=TRUE, levels=c(levels(dd$LightInf), 'UnkLightInf')) 
dd$LightInf[dd$LightInf == "NA"] <- 'UnkLightInf'
dd$LightInf <- factor(dd$LightInf, ordered=TRUE, levels=levels(droplevels(dd$LightInf))) 
table(dd$LightInf)


table(dd$VisionInf)
#table VisionInf has missing values that are found as "NA". Switching them for "UnkVisInf"

dd$VisionInf <- factor(dd$VisionInf, ordered=TRUE, levels=c(levels(dd$VisionInf), 'UnkVisInf')) 
dd$VisionInf[dd$VisionInf == "NA"] <- 'UnkVisInf'
dd$VisionInf <- factor(dd$VisionInf, ordered=TRUE, levels=levels(droplevels(dd$VisionInf))) 
table(dd$VisionInf)


table(dd$Intersect)


table(dd$Surface)
#table Surface has missing values that are found as "NA". Switching them for "UnkSrfc"

dd$Surface <- factor(dd$Surface, ordered=TRUE, levels=c(levels(dd$Surface), 'UnkSrfc')) 
dd$Surface[dd$Surface == "NA"] <- 'UnkSrfc'
dd$Surface <- factor(dd$Surface, ordered=TRUE, levels=levels(droplevels(dd$Surface))) 
table(dd$Surface)


table(dd$DayGroup)
table(dd$HourGroup)
table(dd$AccType)
table(dd$Month)
table(dd$Year)

#also, there are not outliers that have to be removed, as the ones found are missing codes or just extrem values that are important to our study.
#Saving the dataframe
saveRDS(dd, file= "Preprocessed.rds")
write.table(dd, file = "Preprocessed.csv", sep = ",", row.names = FALSE, col.names = TRUE)

#Manual analysis about the randomness about our variables with unknown values
VelNAs <- subset(dd, Vel == "UnknownVel")
EscapedNAs <- subset(dd, Escaped == 'UnknownEscaped')
WeatherNAs <- subset(dd, Weather == "UnknownWeather")
TrafficInfNAs <- subset(dd, TrafficInf == "UnknownTrafficInf")
WeatherInfNAs <- subset(dd, WeatherInf == "UnknownWeatherInf")
LightInfNAs <- subset(dd, LightInf == "UnknownLightInf")
VisionInfNAs <- subset(dd, VisionInf == "UnknownVisionInf")
SurfaceNAs <- subset(dd, Surface == "UnknownSurface")




