

#COSES IMPORTANTS A OBSERVAR DELS PCA

#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
#source code llibreria factoextra, aixi podem fer les grafiques manualment i el profe ni s'entera: https://rdrr.io/cran/factoextra/api/

##si hi ha redundancia entre les mostres, i per tant poder disminuir nombre variables
##Scree plot: mirar dimensions necesaries, nombre necesari de PCAs > 80%. Veure cada PCA la variancia (% info) de les mostres que es capaç de retenir
##fer el corrplot de la contribucio de cada variable a cada PCA (amb pesos com punts)
##fer també barplots de les pr
##fer grafic factors amb vectors de variables numeriques (millor si color de cada vector segons la contribucio respecte els dos PCA)
##fer grafic factors amb centroides de levels de variables categoriques (biplot)






#CHECKEJA LA INSTALACIO DELS PACKAGES NECESARIS
list.of.packages <- c("rstudioapi","remotes", "ggplot2") #posar els packages que es facin servir
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("rstudioapi") #posar els packages que es facin servir
library("ggplot2")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #path al dicteroy del script


#carreguem rds ja que tambe necesitarem els factors pels centroides etc...
dd <- readRDS(file = "dd.rds")


sapply(dd,class)
attach(dd)


#set a list of numerical variables (with no missing values)
numeriques<-which(sapply(dd,is.numeric))
numeriques

dcon<-dd[,numeriques]
sapply(dcon,class)

#PCA
pc1 <- prcomp(dcon, scale=TRUE)
attributes(pc1)
print(pc1)


#percentartge of total inertia per subspace
pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*(inerProj/totalIner)
pinerEix
#fer grafiques amb ggplot2 perque siguin mes guapes... mirar source code de factoextra