#PCA SCRIPT

#CHECKS IF IT IS NECESSARY TO INSTALL ANY PACKAGE
list.of.packages <- c("rstudioapi","remotes", "ggplot2", "hrbrthemes", "showtext", "jsonlite", "curl", "sysfonts", "corrplot", "ggforce", "ggpubr", "ggrepel", "factoextra", "colorspace", "svglite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("rstudioapi")
library("ggplot2")
library("hrbrthemes")
library("showtext")
library("corrplot")
library("ggforce")
library("ggpubr")
library("ggrepel")
library("factoextra")
library("colorspace")
library("svglite")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.create("../Plots")
dir.create("../Plots/PCAgraphs")

#Reading the preprocessed rds file that contains our dataset with factors declared and missings treated
dd <- readRDS(file = "Preprocessed.rds")

#obtaining necessary fonts for the ggplots themes
sysfonts::font_add_google("Roboto Condensed")
showtext_auto()



sapply(dd,class)
attach(dd)


#set a list of numerical variables (with no missing values)
numeriques<-which(sapply(dd,is.numeric))
numeriques
factors<-which(sapply(dd,is.factor))
factors

dcon<-dd[,numeriques]
sapply(dcon,class)

#PCA
pc1 <- prcomp(dcon, scale=TRUE)
attributes(pc1)
print(pc1)


#Percentatge of total inertia per subspace
pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*(inerProj/totalIner)
pinerEix

#scree plot
screePlot <- ggplot(mapping = aes(x=seq_along(pinerEix),y=pinerEix)) +
  geom_bar(stat="identity", fill=hcl.colors(length(pinerEix), palette = "Pastel 1"), color="steelblue") +
  geom_line(color = "black", linetype = "solid") +
  geom_point(shape=19, color="black", size=5) +
  geom_text(label = paste0(round(pinerEix,1), "%"), vjust=-1, hjust=-0.) +
  labs(title = "Scree plot", x = "Dimensions", y = "Percentage of explained variances") +
  scale_x_continuous(breaks=seq(1, length(pinerEix), 1), minor_breaks=seq(1, length(pinerEix), 1)) + 
  scale_y_continuous(breaks=seq(0, 100, 5), minor_breaks=seq(0, 100, 5), labels = function(x) paste0(x, "%")) + 
  theme_ipsum_rc(base_size=15, plot_title_size = 22, axis_title_size = 13)

#print(screePlot)
#saving the resulting scree plot in a new folder
dir.create("../Plots/PCAgraphs/screePlot")
ggsave("../Plots/PCAgraphs/screePlot/screePplot.jpg", plot=screePlot, width = 1650, height = 980, units = "px", dpi=120)


#cumulative scree plot
AccumPinerEix <- cumsum(pinerEix)
cumScreePlot <- ggplot(mapping = aes(x=seq_along(AccumPinerEix),y=AccumPinerEix, fill=AccumPinerEix)) +
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

#print(cumScreePlot)
#saving the resulting cummulative scree plot in a new folder
dir.create("../Plots/PCAgraphs/cumScreePlot")
ggsave("../Plots/PCAgraphs/cumScreePlot/cumScreePlot.jpg", plot=cumScreePlot, width = 1650, height = 980, units = "px", dpi=120)


# SELECTION OF THE SINGIFICNT DIMENSIONS
nd = 4  # 4 diensions are enough as they hold together a 77% of the inertia.

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
corrplot(SquareCorSignificant, is.corr=FALSE, title="Quality of the variables on the PCAs (square cosine of the correlation)",col =COL2('PR'), bg = '#F1F1F1', tl.col = 'black', mar=c(0,0,2,0))
dir.create("../Plots/PCAgraphs/qualityVariables")
savePlotAsImage('../Plots/PCAgraphs/qualityVariables/qualityVariables.jpg','jpeg',width = 1650,height = 980)



#CORRELATION CIRCLE for each combination of principal components

#- Positively correlated variables are grouped together.
#- Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
#- The distance between variables and the origin measures the quality of the variables on the factor map. Variables that are away from the origin are well represented on the factor map.

dir.create("../Plots/PCAgraphs/corrCircle")

for (x in seq(nd-1)) {  
  for (y in seq(x+1, nd)) {
    actual <- ggplot(mapping = aes(x=CorSignificant[, x],y=CorSignificant[, y], color=(SquareCorSignificant[,x]+SquareCorSignificant[,y]))) +
            geom_segment(aes(x=0, y=0, xend=CorSignificant[, x], yend=CorSignificant[, y])
                         , arrow=arrow(length=unit(0.3,"cm")), alpha=1, size=0.8)+
            geom_text_repel(label = rownames(CorSignificant)) +
            ggpubr::gradient_color(c("#00AFBB", "#E7B800", "#FC4E07")) + 
            geom_circle(aes(x0 = 0, y0 = 0, r = 1), color=rgb(0.5,0.5,0.5)) + 
            labs(title = "Correlation circle", subtitle= "and variables quality (calculated as the sum (for the two PCAs) of the square cosine of the correlation)" , x = paste0("PCA", x, " (", pinerEix[x], ' %)'), y = paste0("PCA", y, " (", pinerEix[y], ' %)'), color="% Overall quality of the variables\non the two PCAs (cos2)") +
            scale_x_continuous(breaks=seq(-100, 100, 0.1), minor_breaks=seq(-100, 100, 0.1)) + 
            scale_y_continuous(breaks=seq(-100, 100, 0.1), minor_breaks=seq(-100, 100, 0.1)) + 
            geom_hline(yintercept=0, linetype="dashed") +
            geom_vline(xintercept=0, linetype="dashed") +
            theme_ipsum_rc(base_size=15, plot_title_size = 22, axis_title_size = 13)
    #saving the resulting plots in a new folder
    ggsave(paste0('../Plots/PCAgraphs/corrCircle/corrCircleDim', x, "Dim", y, ".jpg"), plot=actual, width = 1650, height = 980, units = "px", dpi=120)
  }
  
}


# PLOTS OF INDIVIDUALS for each factorial map obtained

dir.create("../Plots/PCAgraphs/individuals")

color_palette1 = c("#4B5D67", "#322F3D", "#59405C", "#87556F")
colorIteration=1
for (x in seq(nd-1)) {  
  for (y in seq(x+1, nd)) {
    actual <- ggplot(mapping = aes(x=CoordSignificant[, x],y=CoordSignificant[, y])) +
      geom_point(size = 2.5, alpha = 4/10, color=(colorIteration%%(length(color_palette1)[1]))+1) + 
      labs(title = "Individuals plot", subtitle= "Point transparency according to the frequency on that exact point" , x = paste0("PCA", x, " (", pinerEix[x], ' %)'), y = paste0("PCA", y, " (", pinerEix[y], ' %)')) +
      scale_x_continuous(breaks=seq(-100, 100, 5), minor_breaks=seq(-100, 100, 5)) + 
      scale_y_continuous(breaks=seq(-100, 100, 5), minor_breaks=seq(-100, 100, 5)) + 
      geom_hline(yintercept=0, linetype="dashed") +
      geom_vline(xintercept=0, linetype="dashed") +
      theme_ipsum_rc(base_size=15, plot_title_size = 22, axis_title_size = 13)
    #saving the resulting plots in a folder
    ggsave(paste0('../Plots/PCAgraphs/individuals/individualsDim', x, "Dim", y, ".jpg"), plot=actual, width = 1650, height = 980, units = "px", dpi=120)
    
    
    colorIteration = colorIteration+1
  }
    
}


#SCALED CORRELATION CIRCLE, INDIVIDUALS, AND EACH CATEGORICAL VARIABLE REPRESENTED (ONE VARIABLE PER PLOT)
#ELLIPSES THAT GROUP LEVELS OF A VARIABLE ARE DONE WITH A NORMAL DISTRIBUTION

dir.create("../Plots/PCAgraphs/pcaVarCat")


for (x in seq(nd-1)) {  
  for (y in seq(x+1, nd)) {
    for(varNumber in factors) {
      actual <- fviz_pca_biplot(pc1, axes = c(x,y),
                            fill.ind = dd[,varNumber], palette = "jco", 
                            label = "var",
                            col.var = rgb(0.5,0.5,0.5), repel = TRUE,
                            legend.title = colnames(dd)[varNumber], geom.ind = "point",
                            pointshape = 21,
                            pointsize = 4, col.ind = "black", addEllipses = TRUE, ellipse.type = "norm", addlabel=TRUE, mean.point.size = 15) + 
              ggpubr::color_palette("jco") + scale_color_manual(values=qualitative_hcl(length(levels(dd[,varNumber]))[1], palette = "Dark3", c=120, h = c(-250, 0))) + scale_fill_manual(values=qualitative_hcl(length(levels(dd[,varNumber]))[1], palette = "Dark3", c=120, h = c(-250, 0))) + 
              scale_x_continuous(breaks=seq(-100, 100, 2), minor_breaks=seq(-100, 100, 2)) +
              scale_y_continuous(breaks=seq(-100, 100, 2), minor_breaks=seq(-100, 100, 2)) + 
              labs(title = paste0("Correlation circle, Individuals, and representation of the ",colnames(dd)[varNumber], " categorical variable"), subtitle= "Correlation vectors are scaled for clarity.\nConcentration ellipses (using multivariate normal distribution) are drawn. Mean points for the levels are also drawn." , x = paste0("PCA", x, " (", pinerEix[x], ' %)'), y = paste0("PCA", y, " (", pinerEix[y], ' %)')) +
              theme_ipsum_rc(base_size=15, plot_title_size = 22, axis_title_size = 13)
      #saving the resulting plots in a new folder
      ggsave(paste0('../Plots/PCAgraphs/pcaVarCat/pcaVarCatDim', x, "Dim", y, "_", colnames(dd)[varNumber], ".jpg"), plot=actual, width = 1650, height = 980, units = "px", dpi=120)
    }
  }
}


#SAME AS BEFORE, BUT ZOOMED IN

dir.create("../Plots/PCAgraphs/pcaVarCatZOOM")

for (x in seq(nd-1)) {  
  for (y in seq(x+1, nd)) {
    for(varNumber in factors) {
      actual <- fviz_pca_biplot(pc1, axes = c(x,y),
                            fill.ind = dd[,varNumber], palette = "jco", 
                            label = "var",
                            col.var = rgb(0.5,0.5,0.5), repel = TRUE,
                            legend.title = colnames(dd)[varNumber], geom.ind = "point",
                            pointshape = 21,
                            pointsize = 4, col.ind = "black", addEllipses = TRUE, ellipse.type = "norm", addlabel=TRUE, mean.point.size = 15) + 
              ggpubr::color_palette("jco") + scale_color_manual(values=qualitative_hcl(length(levels(dd[,varNumber]))[1], palette = "Dark3", c=120, h = c(0, 250))) + scale_fill_manual(values=qualitative_hcl(length(levels(dd[,varNumber]))[1], palette = "Dark3", c=120, h = c(0, 250))) + 
              scale_x_continuous(breaks=seq(-100, 100, 1), minor_breaks=seq(-100, 100, 1), limits = c(-5, 6)) +
              scale_y_continuous(breaks=seq(-100, 100, 1), minor_breaks=seq(-100, 100, 1), limits = c(-5, 6)) + 
              labs(title = paste0("Correlation circle, Individuals, and representation of the ",colnames(dd)[varNumber], " categorical variable (ZOOMED IN)"), subtitle= "Correlation vectors are scaled for clarity.\nConcentration ellipses (using multivariate normal distribution) are drawn. Mean points for the levels are also drawn." , x = paste0("PCA", x, " (", pinerEix[x], ' %)'), y = paste0("PCA", y, " (", pinerEix[y], ' %)')) +
              theme_ipsum_rc(base_size=15, plot_title_size = 22, axis_title_size = 13)
      #saving the resulting plots in a new folder
      ggsave(paste0('../Plots/PCAgraphs/pcaVarCatZOOM/pcaVarCatZOOMDim', x, "Dim", y, "_", colnames(dd)[varNumber], ".jpg"), plot=actual, width = 1650, height = 980, units = "px", dpi=120)
    }
  }
}


#CORRELATION CIRCLE, AND ALL THE LEVELS OF ALL THE VARIABLES IN ONE PLOT (all in one plot except region, that its in another plot for dimensions reasons)

dir.create("../Plots/PCAgraphs/pcaALLvarCatNoRegion")
dir.create("../Plots/PCAgraphs/pcaALLOnlyRegion")

for (x in seq(nd-1)) {  
  for (y in seq(x+1, nd)) {
    first = 1
    
    for (varNumber in factors) {
      fx <- tapply(CoordSignificant[,x],dd[,varNumber],mean)
      fy <- tapply(CoordSignificant[,y],dd[,varNumber],mean) 
      bb <- cbind(fx, fy, summary(dd[, varNumber]))
      
      if(first== 1) {
        first=0
        levelsMeansQuantityNoRegion = cbind(fx, fy, summary(dd[, varNumber]), colnames(dd)[varNumber])
      }
      else{
        if (colnames(dd)[varNumber] == "Region") {
          levelsMeansQuantityRegion <- cbind(fx, fy, summary(dd[, varNumber]))
        }
        else {
          levelsMeansQuantityNoRegion <- rbind(levelsMeansQuantityNoRegion, cbind(fx, fy, summary(dd[, varNumber]), colnames(dd)[varNumber])) 
        }
      }
    }
    
    levelsMeansQuantityNoRegion<-as.data.frame(levelsMeansQuantityNoRegion)
    levelsMeansQuantityNoRegion[, 1] <- sapply(levelsMeansQuantityNoRegion[, 1], as.numeric)
    levelsMeansQuantityNoRegion[, 2] <- sapply(levelsMeansQuantityNoRegion[, 2], as.numeric)
    levelsMeansQuantityNoRegion[, 3] <- sapply(levelsMeansQuantityNoRegion[, 3], as.numeric)
    levelsMeansQuantityNoRegion[, 4] <- sapply(levelsMeansQuantityNoRegion[, 4], as.factor)
    
    #we are not gonna diplay labels for all the ones that are in the center
    levelsMeansQuantityNoRegionForLabels <- subset(levelsMeansQuantityNoRegion, (sqrt( ((levelsMeansQuantityNoRegion[,1])^2) + ((levelsMeansQuantityNoRegion[,2])^2) ) > 0.2 ))
    levelsMeansQuantityRegionForLabels <- subset(levelsMeansQuantityRegion, (sqrt( ((levelsMeansQuantityRegion[,1])^2) + ((levelsMeansQuantityRegion[,2])^2) ) > 0.2 ))
    
    #all no region
    actual <- ggplot() +
      geom_point(mapping = aes(x=levelsMeansQuantityNoRegion[,1], y=levelsMeansQuantityNoRegion[,2], size=levelsMeansQuantityNoRegion[,3], color=levelsMeansQuantityNoRegion[,4])) + 
      geom_segment(aes(x=0, y=0, xend=CorSignificant[, x], yend=CorSignificant[, y])
                   , arrow=arrow(length=unit(0.3,"cm")), alpha=1, size=0.8)+
      geom_text_repel(mapping = aes(x=CorSignificant[, x],y=CorSignificant[, y]), label = rownames(CorSignificant)) +
      geom_label_repel(mapping = aes(x=levelsMeansQuantityNoRegionForLabels[,1], y=levelsMeansQuantityNoRegionForLabels[,2]) ,label = rownames(levelsMeansQuantityNoRegionForLabels),
                       box.padding   = 0.35, 
                       point.padding = 0.5,
                       max.overlaps = 50,
                       segment.color = 'grey50') +
      geom_circle(aes(x0 = 0, y0 = 0, r = 1), color=rgb(0.5,0.5,0.5)) + 
      labs(title = "Correlation circle, and representation of all modalitites of all cat. variables (except Region)", subtitle= "and modality quantities as point sizes" , x = paste0("PCA", x, " (", pinerEix[x], ' %)'), y = paste0("PCA", y, " (", pinerEix[y], ' %)'), color="Color according to\n cateogorical variable", size="Size according to quantity samples\non modalitites") +
      scale_x_continuous(breaks=seq(-100, 100, 0.1), minor_breaks=seq(-100, 100, 0.1)) + 
      scale_y_continuous(breaks=seq(-100, 100, 0.1), minor_breaks=seq(-100, 100, 0.1)) + 
      geom_hline(yintercept=0, linetype="dashed") +
      geom_vline(xintercept=0, linetype="dashed") +
      theme_ipsum_rc(base_size=15, plot_title_size = 22, axis_title_size = 13)
    #saving the resulting plot
    ggsave(paste0('../Plots/PCAgraphs/pcaALLvarCatNoRegion/pcaALLvarCatNoRegionDim', x, "Dim", y, ".jpg"), plot=actual, width = 1650, height = 980, units = "px", dpi=120)
    
    
    #only region
    actual2 <- ggplot() +
      geom_point(mapping = aes(x=levelsMeansQuantityRegion[,1], y=levelsMeansQuantityRegion[,2], size=levelsMeansQuantityRegion[,3], color=rownames(levelsMeansQuantityRegion))) + 
      geom_segment(aes(x=0, y=0, xend=CorSignificant[, x], yend=CorSignificant[, y])
                   , arrow=arrow(length=unit(0.3,"cm")), alpha=1, size=0.8)+
      geom_text_repel(mapping = aes(x=CorSignificant[, x],y=CorSignificant[, y]), label = rownames(CorSignificant)) +
      geom_label_repel(mapping = aes(x=levelsMeansQuantityRegionForLabels[,1], y=levelsMeansQuantityRegionForLabels[,2]) ,label = rownames(levelsMeansQuantityRegionForLabels),
                       box.padding   = 0.35, 
                       point.padding = 0,
                       max.overlaps = 50,
                       segment.color = 'grey50') +
      geom_circle(aes(x0 = 0, y0 = 0, r = 1), color=rgb(0.5,0.5,0.5)) + 
      labs(title = "Correlation circle, and representation of all modalitites of Region cat. variable", subtitle= "and modality quantities as point sizes" , x = paste0("PCA", x, " (", pinerEix[x], ' %)'), y = paste0("PCA", y, " (", pinerEix[y], ' %)'), size="Size according to quantity samples\non modalitites") +
      scale_x_continuous(breaks=seq(-100, 100, 0.1), minor_breaks=seq(-100, 100, 0.1)) + 
      scale_y_continuous(breaks=seq(-100, 100, 0.1), minor_breaks=seq(-100, 100, 0.1)) + 
      geom_hline(yintercept=0, linetype="dashed") +
      geom_vline(xintercept=0, linetype="dashed") +
      guides(color = FALSE)  + 
      theme_ipsum_rc(base_size=15, plot_title_size = 22, axis_title_size = 13)
    #saving the resulting plot
    ggsave(paste0('../Plots/PCAgraphs/pcaALLOnlyRegion/pcaALLOnlyRegionDim', x, "Dim", y, ".jpg"), plot=actual2, width = 1650, height = 980, units = "px", dpi=120)
  }
}

