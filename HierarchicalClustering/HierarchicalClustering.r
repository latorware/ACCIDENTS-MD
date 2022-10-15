#CHECKEJA LA INSTALACIO DELS PACKAGES NECESARIS
list.of.packages <- c("rstudioapi","NbClust") #posar els packages que es facin servir
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("rstudioapi") #posar els packages que es facin servir
library(cluster)
library(NbClust)
#Retrieve the data saved AFTER the profiling practice...... this means data already cleaned

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #path al dicteroy del script

dd <- readRDS(file = "Preprocessed.rds")
names(dd)
dim(dd)
summary(dd)

attach(dd)

#set a list of numerical variables
names(dd)

#EMPEZAMOS AQUI EL HIERARCHICAL xdxdxd
ddNoData <- dd

ddNoData$Date=NULL

#dissimilarity matrix

actives<-c(1, 3:25) #We ignore date column because it causes problems
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2


clust <- NbClust(diss=dissimMatrix, distance=NULL, min.nc=2, max.nc=10, method = "ward.D2",index="cindex" , alphaBeale = 0.1)
clust$All.index #cindex, mclain minimo. silhouette y dunn maximo.
clust$Best.nc
clust <- NbClust(diss=dissimMatrix, distance=NULL, min.nc=2, max.nc=10, method = "ward.D2",index="mclain" , alphaBeale = 0.1)
clust$All.index #cindex, mclain minimo. silhouette y dunn maximo.
clust$Best.nc
clust <- NbClust(diss=dissimMatrix, distance=NULL, min.nc=2, max.nc=10, method = "ward.D2",index="silhouette" , alphaBeale = 0.1)
clust$All.index #cindex, mclain minimo. silhouette y dunn maximo.
clust$Best.nc
clust <- NbClust(diss=dissimMatrix, distance=NULL, min.nc=2, max.nc=10, method = "ward.D2",index="dunn" , alphaBeale = 0.1)
clust$All.index #cindex, mclain minimo. silhouette y dunn maximo.
clust$Best.nc

#k=2 es mejor, pero alomejor hay poco que estudiar. Si los dos centroides spn muy parecidos, no hay mucho que estudiar
#entonces seria mejor k=5 (en caso que no haya mucho que estudiar)
#tener en cuenta que dunn si hay muchos clusters le va mejor asi que a los grandes no mucho caso
h1 <- hclust(distMatrix,method="ward.D2")  # NOTICE THE COST
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot


plot(h1)
rect.hclust(h1,2)
c2 <- cutree(h1,2) #cogemos 2 por los KPI anteriores

#class sizes 
table(c2)
plot(c2)