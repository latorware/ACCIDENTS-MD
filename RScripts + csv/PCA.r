

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
list.of.packages <- c("rstudioapi","remotes", "ggplot2", "hrbrthemes", "showtext", "jsonlite", "curl", "sysfonts", "corrplot") #posar els packages que es facin servir
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("rstudioapi") #posar els packages que es facin servir
library("ggplot2")
library("hrbrthemes")
library("showtext")
library("corrplot")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #path al dicteroy del script


#carreguem rds ja que tambe necesitarem els factors pels centroides etc...
dd <- readRDS(file = "dd.rds")

#carreguem fonts necessaries pels themes a les ggplot
sysfonts::font_add_google("Roboto Condensed")
showtext_auto()



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

#scree plot
ggplot(mapping = aes(x=seq_along(pinerEix),y=pinerEix)) +
  geom_bar(stat="identity", fill=hcl.colors(length(pinerEix), palette = "Pastel 1"), color="steelblue") +
  geom_line(color = "black", linetype = "solid") +
  geom_point(shape=19, color="black", size=5) +
  geom_text(label = paste0(round(pinerEix,1), "%"), vjust=-1, hjust=-0.) +
  labs(title = "Scree plot", x = "Dimensions", y = "Percentage of explained variances") +
  scale_x_continuous(breaks=seq(1, length(pinerEix), 1), minor_breaks=seq(1, length(pinerEix), 1)) + 
  scale_y_continuous(breaks=seq(0, 100, 5), minor_breaks=seq(0, 100, 5), labels = function(x) paste0(x, "%")) + 
  theme_ipsum_rc(base_size=15, plot_title_size = 22, axis_title_size = 13)

#cumulative scree plot
AccumPinerEix <- cumsum(pinerEix)
ggplot(mapping = aes(x=seq_along(AccumPinerEix),y=AccumPinerEix, fill=AccumPinerEix)) +
  geom_bar(stat="identity", color="steelblue") +
  scale_fill_gradient2(midpoint=80, 
                        low ="red",mid = "yellow", high = "green") + 
  geom_line(color = "black", linetype = "solid") +
  geom_point(shape=19, color="black", size=5) +
  geom_text(label = paste0(round(AccumPinerEix,1), "%"), vjust=-1, hjust=-0.) +
  labs(title = "Cumulative Scree plot", x = "Dimensions", y = "Percentage of cumulated explained variances") +
  scale_x_continuous(breaks=seq(1, length(AccumPinerEix), 1), minor_breaks=seq(1, length(AccumPinerEix), 1)) + 
  scale_y_continuous(breaks=seq(0, 100, 5), minor_breaks=seq(0, 100, 5), labels = function(x) paste0(x, "%")) + 
  theme_ipsum_rc(base_size=15, plot_title_size = 22, axis_title_size = 13)

# SELECTION OF THE SINGIFICNT DIMENSIONS
nd = 5

#coordinates of observations on significant PCAs. Also the calculation of the correlation of the variables to the PCAs
CoordSignificant = pc1$x[,1:nd]
RotationSignificant = pc1$rotation[,1:nd]
CorSignificant = cor(dcon,CoordSignificant) #Correlation variables to every significant PCA

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES.
rownSeq = row.names(dcon)
etiq = names(dcon)


#Quality of the variables on the PCAs. (Useful not only to see which variables are well represented, but also which ones are not)

#- it is measured as the SQUARE COSINE, square of the correlation of the variables to the PCAs. The correlation between a variable and a principal component (PC) is used as the coordinates of the variable on the PC
#- A high cos2 indicates a good representation of the variable on the principal component. In this case the variable is positioned close to the circumference of the correlation circle.
#- A low cos2 indicates that the variable is not perfectly represented by the PCs. In this case the variable is close to the center of the circle.
#- For a given variable, the sum of the cos2 on all the principal components is equal to one.

SquareCorSignificant = CorSignificant^2
corrplot(SquareCorSignificant, is.corr=FALSE, title="aaa")


# PLOT OF INDIVIDUALS

#without labels on the observation circles

#with labels on the observation circles





#Correlation circle

#- Positively correlated variables are grouped together.
#- Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
#- The distance between variables and the origin measures the quality of the variables on the factor map. Variables that are away from the origin are well represented on the factor map.

