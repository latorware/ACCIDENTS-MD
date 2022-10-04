

#COSES IMPORTANTS A OBSERVAR DELS PCA

#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

##si hi ha redundancia entre les mostres, i per tant poder disminuir nombre variables
##Scree plot: mirar dimensions necesaries, nombre necesari de PCAs > 80%. Veure cada PCA la variancia (% info) de les mostres que es capaç de retenir
##fer el corrplot de la contribucio de cada variable a cada PCA (amb pesos com punts)
##fer també barplots de les pr
##fer grafic factors amb vectors de variables numeriques (millor si color de cada vector segons la contribucio respecte els dos PCA)
##fer grafic factors amb centroides de levels de variables categoriques (biplot)






#CHECKEJA LA INSTALACIO DELS PACKAGES NECESARIS
list.of.packages <- c("rstudioapi","remotes") #posar els packages que es facin servir
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("rstudioapi") #posar els packages que es facin servir

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #path al dicteroy del script


#carreguem rds ja que tambe necesitarem els factors pels centroides etc...
dd <- readRDS(file = "dd.rds")