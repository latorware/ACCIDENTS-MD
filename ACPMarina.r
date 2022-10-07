
#PREREQUISITES: 
#factors are properly labelled and reading data makes R to directly recognize them
#Numerical variables do not contain missing values anymore. They have been imputed in preprocessing step

#  READING OUR DATASET - THE VERSION THAT PASSES THE REQUISITES ABOVE
setwd("C://Users//marina.alapont//Downloads//ACCIDENTS-MD-PCA_Marina//ACCIDENTS-MD-PCA_Marina")
dd <- read.csv("Preprocesed.csv",header=T);  #Hauriem de llegir d'un rds per mantenir els factors?

#declarar factors?
Zone     <- factor(dd[,1])
Region     <- factor(dd[,3])
Prov     <- factor(dd[,4])
Vel     <- factor(dd[,12])
Escaped     <- factor(dd[,13])
Weather     <- factor(dd[,14])
TrafficInf     <- factor(dd[,15])
WeatherInf     <- factor(dd[,16])
LightInf     <- factor(dd[,17])
VisionInf     <- factor(dd[,18])
Intersect     <- factor(dd[,19])
Surface     <- factor(dd[,20])
DayGroup     <- factor(dd[,21])
HourGroup     <- factor(dd[,22])
AccType     <- factor(dd[,23])

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

#alternatively 
#objects()
#attributes(dd)

# VISUALISATION OF DATA
## PRINCIPAL COMPONENT ANALYSIS OF CONTINUOUS VARIABLES
### CREATION OF THE DATA FRAME OF CONTINUOUS VARIABLES

attach(dd)
names(dd)

#is R understanding well my factor variables?
sapply(dd,class)

#set a list of numerical variables (with no missing values)
numeriques <- which(sapply(dd,is.numeric))
numeriques

#CREATING A TABLE WITH ALL THE INDIVIDUALS BUT ONLY THE NUMERICAL VARIABLES IN ORDER TO DO THE PCA
dcon <- dd[, numeriques]
sapply(dcon, class)

############################CREATION OF THE PCAs###############################

# PRINCIPAL COMPONENT ANALYSIS OF dcon

pc1 <- prcomp(dcon, scale=TRUE)
class(pc1)
attributes(pc1)

print(pc1)

# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?
pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
barplot(pinerEix)

#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])
percInerAccum<-100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2]
percInerAccum

# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)
nd = 4 #With 4 dimensions we have the 77% of the inertia, it is enough 

print(pc1)
attributes(pc1)
pc1$rotation

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS
View(pc1$x)
dim(pc1$x)
dim(dcon)
dcon[2000,]
pc1$x[2000,]

Psi = pc1$x[,1:nd]
dim(Psi)
Psi[2000,]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES
iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq)) # we will need this vector afterwards for the graphics


############################PLOTS###############################


# PLOT OF INDIVIDUALS
#select your axis
eje1<-1
eje2<-2

{plot(Psi[,eje1],Psi[,eje2])
text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")}


#PROJECTION OF NUMERICAL VARIABLES

Phi = cor(dcon,Psi)
View(Phi)

#select your axis
X<-Phi[,eje1]
Y<-Phi[,eje2]

{plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)}


#zooms
{plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)}



# PROJECTION OF ILLUSTRATIVE qualitative variables on individuals' map

#zona
varcat=dd[,1]
plot(Psi[,1],Psi[,2],col =varcat)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(varcat),pch=1,col=c(1,2), cex=0.6)


# Overproject THE CDG OF  LEVELS OF varcat
k<-1 #zona

varcat<-dd[,k]
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean) 
text(fdic1,fdic2,labels=levels(varcat),col="yellow", cex=0.7)

#Now we project both cdgs of levels of a selected qualitative variable without
#representing the individual anymore

plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

text(fdic1,fdic2,labels=levels(varcat),col="blue", cex=0.7)


###############
#repetir per les variables que creiem oportu

varcat=dd[,13]
plot(Psi[,1],Psi[,2],col = varcat)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(varcat),pch=1,col=c(1,2), cex=0.6)

varcat=dd[,15]
plot(Psi[,1],Psi[,2],col = varcat)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(varcat),pch=1,col=c(1,2), cex=0.6)

varcat=dd[,16]
plot(Psi[,1],Psi[,2],col = varcat)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(varcat),pch=1,col=c(1,2), cex=0.6)

varcat=dd[,17]
plot(Psi[,1],Psi[,2],col = varcat)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(varcat),pch=1,col=c(1,2), cex=0.6)

#############################


#PROJECCTION OF QUALITATIVE VARIABLES CENTROIDS

plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")


#nominal qualitative variables

dcat<-c(1:4,12:23)
#TO DO -- DIVIDIR EN GRAPHS DIFERENTS
#divide categoricals in several graphs if joint representation saturates 

#build a palette with as much colors as qualitative variables 

colors<-rainbow(length(dcat))

c<-1
for(k in dcat){
  seguentColor<-colors[c]
fdic1 = tapply(Psi[,eje1],dd[,k],mean)
fdic2 = tapply(Psi[,eje2],dd[,k],mean) 

text(fdic1,fdic2,labels=levels(dd[,k]),col=seguentColor, cex=0.6)
c<-c+1
}
legend("bottomleft",names(dd)[dcat],pch=1,col=colors, cex=0.6)

#determine zoom level
#use the scale factor or not depending on the position of centroids
# ES UN FACTOR D'ESCALA PER DIBUIXAR LES FLETXES MES VISIBLES EN EL GRAFIC
#fm = round(max(abs(Psi[,1]))) 
# fm=20

#scale the projected variables
# X<-fm*U[,eje1]
# Y<-fm*U[,eje2]


#PROJECTION OF THE NUMERICAL VARIABLES + CENTROIDS OF CATEGORICAL REPRESENTED 

#represent numerical variables in background
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1), ylim=c(-3,1))
plot(X,Y,type="none",xlim=c(-2,1), ylim=c(-2,2))
 axis(side=1, pos= 0, labels = F, col="cyan")
 axis(side=3, pos= 0, labels = F, col="cyan")
 axis(side=2, pos= 0, labels = F, col="cyan")
 axis(side=4, pos= 0, labels = F, col="cyan")

#add projections of numerical variables in background
 arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
 text(X,Y,labels=etiq,col="gray", cex=0.7)

#add centroids
 c<-1
 for(k in dcat){
  seguentColor<-colors[c]
   
  fdic1 = tapply(Psi[,eje1],dd[,k],mean)
  fdic2 = tapply(Psi[,eje2],dd[,k],mean) 
 
 #points(fdic1,fdic2,pch=16,col=seguentColor, abels=levels(dd[,k]))
 text(fdic1,fdic2,labels=levels(dd[,k]),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(dd)[dcat],pch=1,col=colors, cex=0.6)


############################################### AIXO NO SE QUE ES. CREC QUE NO TENIM CAP ORDINAL

#add ordinal qualitative variables. Ensure ordering is the correct
dordi<-c(8)


levels(dd[,dordi[1]])
#reorder modalities: when required
dd[,dordi[1]] <- factor(dd[,dordi[1]], ordered=TRUE,  levels= c("WorkingTypeUnknown","altres sit","temporal","fixe","autonom"))
levels(dd[,dordi[1]])

c<-1
col<-1
for(k in dordi){
  seguentColor<-colors[col]
  fdic1 = tapply(Psi[,eje1],dd[,k],mean)
  fdic2 = tapply(Psi[,eje2],dd[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
  #connect modalities of qualitative variables
  lines(fdic1,fdic2,pch=16,col=seguentColor)
 text(fdic1,fdic2,labels=levels(dd[,k]),col=seguentColor, cex=0.6)
  c<-c+1
  col<-col+1
}
legend("topleft",names(dd)[dordi],pch=1,col=colors[1:length(dordi)], cex=0.6)

##################################




