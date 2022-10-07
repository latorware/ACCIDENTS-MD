
#CHECKEJA LA INSTALACIO DELS PACKAGES NECESARIS
list.of.packages <- c("rstudioapi","remotes") #posar els packages que es facin servir
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("rstudioapi") #posar els packages que es facin servir

setwd(paste(dirname(rstudioapi::getActiveDocumentContext()$path),"/RScripts + csv", sep="")) #path al dicteroy del script

dd <- read.csv("Accidents5000Reduit.csv", header=T);


#CANVIAR NOMS VARIABLES
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


#declarar categoriques
#sapply(dd, class)
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


#processar levels categoriques (canviar noms, ordernar levels si necessari, etc)

summary(Zone)
#pie(table(Zone))
#barplot(table(Zone))
levels(Zone) <- c("Road", "Urban")
summary(Zone)



summary(Region)
levels(Region)[22] <- "Moianes"
dd[,3]<-Region
table(dd$Region)
tochange<- PREPROCESSINGjoin <- read.csv("PREPROCESSINGjoin.csv", sep=";")
dd$Region<-tochange$Short[match(dd$Region,tochange$Long)] 
Region     <- factor(dd$Region)
#dd[,3]<-Region
#pie(table(Region))
#barplot(table(Region), las =2)
summary(Region)

summary(Prov)
#pie(table(Prov))
#barplot(table(Prov))
#no cal canviar res
summary(Prov)

summary(Vel)
#pie(table(Vel))
#barplot(table(Vel))
#No cal canviar res (missings es tracten m?s endavant...). Levels ja ordenats
summary(Vel)

summary(Escaped)
#pie(table(Escaped))
#barplot(table(Escaped))
levels(Escaped) <- c("No", "NA", "Yes")
#Ordenar levels: 
Escaped <- factor(Escaped, ordered=TRUE, levels=c('Yes', 'No', 'NA'))
summary(Escaped)

summary(Weather)
#pie(table(Weather))
#barplot(table(Weather))
levels(Weather) <- c("Good", "Snow", "WeakRain", "StrongRain", "NA")
#ordenar levels:
Weather <- factor(Weather, ordered=TRUE, levels=c('Good', 'WeakRain', 'StrongRain', 'Snow', 'NA'))
summary(Weather)

summary(TrafficInf)
#pie(table(TrafficInf))
#barplot(table(TrafficInf))
levels(TrafficInf) <- c("No", "NA", "Yes")
#Ordenar levels: 
TrafficInf <- factor(TrafficInf, ordered=TRUE, levels=c('Yes', 'No', 'NA'))
summary(TrafficInf)

summary(WeatherInf)
#pie(table(WeatherInf))
#barplot(table(WeatherInf))
levels(WeatherInf) <- c("No", "NA", "Yes")
#Ordenar levels: 
WeatherInf <- factor(WeatherInf, ordered=TRUE, levels=c('Yes', 'No', 'NA'))
summary(WeatherInf)

summary(LightInf)
#pie(table(LightInf))
#barplot(table(LightInf))
levels(LightInf) <- c("No", "NA", "Yes")
#Ordenar levels: 
LightInf <- factor(LightInf, ordered=TRUE, levels=c('Yes', 'No', 'NA'))
summary(LightInf)

summary(VisionInf)
#pie(table(VisionInf))
#barplot(table(VisionInf))
levels(VisionInf) <- c("No", "NA", "Yes")
#Ordenar levels: 
VisionInf <- factor(VisionInf, ordered=TRUE, levels=c('Yes', 'No', 'NA'))
summary(VisionInf)

summary(Intersect)
#pie(table(Intersect))
#barplot(table(Intersect))
levels(Intersect) <- c("Arriving", "Inside", "InSection")
#Ordenar levels: 
Intersect <- factor(Intersect, ordered=TRUE, levels=c('InSection', 'Arriving', 'Inside'))
summary(Intersect)

summary(DayGroup)
#pie(table(DayGroup))
#barplot(table(DayGroup))
levels(DayGroup) <- c("Weekend", "Weekday")
#Ordenar levels: 
DayGroup <- factor(DayGroup, ordered=TRUE, levels=c('Weekday', 'Weekend'))
summary(DayGroup)

summary(Surface)
#pie(table(Surface))
#barplot(table(Surface))
levels(Surface) <- c("Icy", "Flooded", "Wet", "Snowy", "Slippery", "Dry&Clean", "NA")
#Ordenar levels: 
Surface <- factor(Surface, ordered=TRUE, levels=c("Dry&Clean", "Slippery", "Wet", "Flooded", "Icy", "Snowy", "NA"))
summary(Surface)

summary(HourGroup)
#pie(table(HourGroup))
#barplot(table(HourGroup))
levels(HourGroup) <- c("Morning", "Night", "Afternoon")
#Ordenar levels: 
HourGroup <- factor(HourGroup, ordered=TRUE, levels=c('Morning', 'Afternoon', 'Night'))
summary(HourGroup)

summary(AccType)
#pie(table(AccType))
#barplot(table(AccType))
levels(AccType) <- c("Other", "RunOver", "Rollover", "HitObstacle", "HitVehicle/s", "NARoadExit")
#Ordenar levels: 
AccType <- factor(AccType, ordered=TRUE, levels=c('RunOver', 'Rollover', 'HitObstacle', 'HitVehicle/s', 'NARoadExit', 'Other'))
summary(AccType)


#declarar dates
Date     <- as.Date(Date, format =  "%d/%m/%Y")


#aplicar canvis a dd
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

sapply(dd, class)
sapply(dd, levels)


#write.csv(dd,"before_missings.rds", row.names = FALSE)
saveRDS(dd, file= "before_missings.rds")

#Missing treatment

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
dd$Vel <- factor(dd$Vel, ordered=TRUE, levels=c(levels(dd$Vel), 'UnknownVel')) #Adds 'UnknownVel' level
dd$Vel[dd$Vel==999 |dd$Vel==0 | is.na(dd$Vel)]  <- 'UnknownVel' #Moves vel's where it's value is 999 or 0 or NA to 'UnknownVel'
dd$Vel <- factor(dd$Vel, ordered=TRUE, levels=levels(droplevels(dd$Vel))) #Removes 10 and 999 factors since there aren't used


table(dd$Escaped)
dd$Escaped <- factor(dd$Escaped, ordered=TRUE, levels=c(levels(dd$Escaped), 'UnknownEscaped')) #Adds 'UnknownEscaped' level
dd$Escaped[dd$Escaped == "NA"] <- 'UnknownEscaped'#Moves vel's where it's value is NA to 'UnknownEscaped'
dd$Escaped <- factor(dd$Escaped, ordered=TRUE, levels=levels(droplevels(dd$Escaped))) #Removes NA factor

#Next missing treatments are the same as Escaped column

table(dd$Weather)
dd$Weather <- factor(dd$Weather, ordered=TRUE, levels=c(levels(dd$Weather), 'UnknownWeather')) 
dd$Weather[dd$Weather == "NA"] <- 'UnknownWeather'
dd$Weather <- factor(dd$Weather, ordered=TRUE, levels=levels(droplevels(dd$Weather))) 


table(dd$TrafficInf)
dd$TrafficInf <- factor(dd$TrafficInf, ordered=TRUE, levels=c(levels(dd$TrafficInf), 'UnknownTrafficInf')) 
dd$TrafficInf[dd$TrafficInf == "NA"] <- 'UnknownTrafficInf'
dd$TrafficInf <- factor(dd$TrafficInf, ordered=TRUE, levels=levels(droplevels(dd$TrafficInf))) 


table(dd$WeatherInf)
dd$WeatherInf <- factor(dd$WeatherInf, ordered=TRUE, levels=c(levels(dd$WeatherInf), 'UnknownWeatherInf')) 
dd$WeatherInf[dd$WeatherInf == "NA"] <- 'UnknownWeatherInf'
dd$WeatherInf <- factor(dd$WeatherInf, ordered=TRUE, levels=levels(droplevels(dd$WeatherInf))) 


table(dd$LightInf)
dd$LightInf <- factor(dd$LightInf, ordered=TRUE, levels=c(levels(dd$LightInf), 'UnknownLightInf')) 
dd$LightInf[dd$LightInf == "NA"] <- 'UnknownLightInf'
dd$LightInf <- factor(dd$LightInf, ordered=TRUE, levels=levels(droplevels(dd$LightInf))) 


table(dd$VisionInf)
dd$VisionInf <- factor(dd$VisionInf, ordered=TRUE, levels=c(levels(dd$VisionInf), 'UnknownVisionInf')) 
dd$VisionInf[dd$VisionInf == "NA"] <- 'UnknownVisionInf'
dd$VisionInf <- factor(dd$VisionInf, ordered=TRUE, levels=levels(droplevels(dd$VisionInf))) 


table(dd$Intersect)


table(dd$Surface)
dd$Surface <- factor(dd$Surface, ordered=TRUE, levels=c(levels(dd$Surface), 'UnknownSurface')) 
dd$Surface[dd$Surface == "NA"] <- 'UnknownSurface'
dd$Surface <- factor(dd$Surface, ordered=TRUE, levels=levels(droplevels(dd$Surface))) 


table(dd$DayGroup)
table(dd$HourGroup)
table(dd$AccType)


#Save of the dataframe

saveRDS(dd, file= "Preprocessed.rds")
dd <- readRDS(file = "Preprocessed.rds")
write.table(dd, file = "Preprocesed.csv", sep = ",", row.names = FALSE, col.names = TRUE)

#Manual analysis about the randomness about our variables with unknown values
VelNAs <- subset(dd, Vel == "UnknownVel")
EscapedNAs <- subset(dd, Escaped == 'UnknownEscaped')
WeatherNAs <- subset(dd, Weather == "UnknownWeather")
TrafficInfNAs <- subset(dd, TrafficInf == "UnknownTrafficInf")
WeatherInfNAs <- subset(dd, WeatherInf == "UnknownWeatherInf")
LightInfNAs <- subset(dd, LightInf == "UnknownLightInf")
VisionInfNAs <- subset(dd, VisionInf == "UnknownVisionInf")
SurfaceNAs <- subset(dd, Surface == "UnknownSurface")



#remotes::install_github("njtierney/naniar") #https://search.r-project.org/CRAN/refmans/naniar/html/mcar_test.html
#library(naniar)
#mcar_test(dd) 
#1a columna: Chi-squared statistic for Little's test, 2a columna:Degrees of freedom used for chi-squared statistic
#3a columna: P-value for the chi-squared statistic y 4a columna: Number of missing data patterns in the data
