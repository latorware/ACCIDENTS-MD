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

#dissimilarity matrix

actives<-c(1, 3:25) #We ignore date column because it causes problems
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D2")  # NOTICE THE COST
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot


plot(h1)


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

rect.hclust(h1,2)
c2 <- cutree(h1,2) #cogemos 2 por los KPI anteriores

#class sizes 
table(c2)
plot(c2)
#saving the clusters information in a new variable
dd$Cluster <- c2


#PROFILING
#PROFILING PLOTS

#Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum,P){
  #freq dis of fac
  nk <- as.vector(table(P)); 
  n <- sum(nk); 
  #mitjanes x grups
  xk <- tapply(Xnum,P,mean);
  #valors test
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk))); 
  #p-values
  pxk <- pt(txk,n-1,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}



ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali);
  n <- sum(taula); 
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2], byrow=TRUE);      
  dpf <- pf - pjm; 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); 
  #i hi ha divisions iguals a 0 dona NA i no funciona
  zkj <- dpf
  zkj[dpf!=0]<-dpf[dpf!=0]/dvt[dpf!=0]; 
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}


#source("file")
#dades contain the dataset
ddWithoutDate<-c(1, 3:25) #We ignore date column because it causes problems
dades<-dd[,ddWithoutDate]
#dades<-dd[filtro,]
#dades<-df

K<-dim(dades)[2]
par(ask=TRUE)


#P must contain the class variable
#P<-dd[,3]
P<-c2
#P<-dd[,18]
nameP<-"classe"
#P<-df[,33]

nc<-length(levels(factor(P)))
nc
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dades)))
nameP<-"Class"
n<-dim(dades)[1]


for(k in 1:K){
  if (is.numeric(dades[,k])){ 
    print(paste("Anàlisi per classes de la Variable:", names(dades)[k]))
   
     jpeg(file = paste("Variable",k,"_1.jpeg",sep=""),width = 980, height = 640, units = "px", quality = 100)
    boxplot(dades[,k]~P, main=paste("Boxplot of", names(dades)[k], "vs", nameP ), horizontal=TRUE)
    dev.off()
    jpeg(file = paste("Variable",k,"_2.jpeg",sep=""),width = 980, height = 640, units = "px", quality = 100)
    barplot(tapply(dades[[k]], P, mean),main=paste("Means of", names(dades)[k], "by", nameP ))
    abline(h=mean(dades[[k]]))
    legend(0,mean(dades[[k]]),"global mean",bty="n")
    dev.off()
    
    print("Estadístics per groups:")
    for(s in levels(as.factor(P))) {print(summary(dades[P==s,k]))}
    o<-oneway.test(dades[,k]~P)
    print(paste("p-valueANOVA:", o$p.value))
    kw<-kruskal.test(dades[,k]~P)
    print(paste("p-value Kruskal-Wallis:", kw$p.value))
    pvalk[,k]<-ValorTestXnum(dades[,k], P)
    print("p-values ValorsTest: ")
    print(pvalk[,k])      
  }else{
    if(class(dades[,k])[1]=="Date"){
      print(summary(dades[,k]))
      print(sd(dades[,k]))
      #decide breaks: weeks, months, quarters...
      hist(dades[,k],breaks="weeks")
    }else{
      #qualitatives
      print(paste("Variable", names(dades)[k]))
      table<-table(P,dades[,k])
      #   print("Cross-table")
      #   print(table)
      rowperc<-prop.table(table,1)
      
      colperc<-prop.table(table,2)
      #  print("Distribucions condicionades a files")
      # print(rowperc)
      
      #ojo porque si la variable es true o false la identifica amb el tipus Logical i
      #aquest no te levels, por tanto, coertion preventiva
      
      dades[,k]<-as.factor(dades[,k])
      
      
      marg <- table(as.factor(P))/n
      print(append("Categories=",levels(as.factor(dades[,k]))))
      
      #from next plots, select one of them according to your practical case
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }

    
      #with legend
      jpeg(file = paste("Variable",k,"_1.jpeg",sep=""),width = 980, height = 640, units = "px", quality = 100)
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
      legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=1.5)
      dev.off()
      
      
      #condicionades a classes
      print(append("Categories=",levels(dades[,k])))
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }

      
      #with legend
      jpeg(file = paste("Variable",k,"_2.jpeg",sep=""),width = 980, height = 640, units = "px", quality = 100)
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
      legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=1.5)
      dev.off()
      
      
      #amb variable en eix d'abcisses
      marg <-table(dades[,k])/n
      print(append("Categories=",levels(dades[,k])))
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
      #x<-plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), xaxt="n")
      #text(x=x+.25, y=-1, adj=1, levels(CountryName), xpd=TRUE, srt=25, cex=0.7)
      paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c]) }
     
      #with legend
      jpeg(file = paste("Variable",k,"_3.jpeg",sep=""), width = 980, height = 640, units = "px", quality = 100)
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=1.5)
      dev.off()
      
      #condicionades a columna 
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
      paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c]) }
      
      #with legend
      jpeg(file = paste("Variable",k,"_4.jpeg",sep=""), width = 980, height = 640, units = "px", quality = 100)
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=1.5)
      dev.off()
      
      table<-table(dades[,k],P)
      print("Cross Table:")
      print(table)
      print("Distribucions condicionades a columnes:")
      print(colperc)
      
      #diagrames de barres apilades                                         
      
      paleta<-rainbow(length(levels(dades[,k])))
      barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
      
      jpeg(file = paste("Variable",k,"_5.jpeg",sep=""),width = 980, height = 640, units = "px", quality = 100)
      barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=1.5, col=paleta)
      dev.off()
      
      #diagrames de barres adosades
      barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta )
      
      jpeg(file = paste("Variable",k,"_6.jpeg",sep=""),width = 980, height = 640, units = "px", quality = 100)
      barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta)
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=1.5, col=paleta)
      dev.off()
      
      print("Test Chi quadrat: ")
      print(chisq.test(dades[,k], as.factor(P)))
      
      print("valorsTest:")
      print( ValorTestXquali(P,dades[,k]))
      #calcular els pvalues de les quali
    }
  }
}#endfor


#descriptors de les classes més significatius. Afegir info qualits
for (c in 1:length(levels(as.factor(P)))) {
  if(!is.na(levels(as.factor(P))[c])){
    print(paste("P.values per class:",levels(as.factor(P))[c]));
    print(sort(pvalk[c,]), digits=3) 
  }
}

#afegir la informacio de les modalitats de les qualitatives a la llista de pvalues i fer ordenacio global

#saving the dataframe in an external file
#write.table(dd, file = "credscoClean.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)


