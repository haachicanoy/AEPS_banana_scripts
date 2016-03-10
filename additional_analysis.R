# Estadisticas descriptivas - Informe final MADR
# H. Achicanoy
# CIAT, 2016

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Peso del racimo behavior by modeling scenario
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

sc1 <- read.csv('Z:/DATOS_PROCESADOS/_cosecha/_banasan/banasan_cosechas_suelo_foliar_clima_cycle_corrected.csv')
sc1 <- sc1[complete.cases(sc1),]; sc1 <- sc1[sc1$Peso_racimo>0,]; rownames(sc1) <- 1:nrow(sc1)
sc1 <- data.frame(scenario='Escenario 1', peso_racimo=sc1$Peso_racimo)

sc2 <- read.csv('Z:/DATOS_PROCESADOS/_cosecha/_cobana/cobana_cosechas_fertilizaciones_clima_cycle.csv')
sc2 <- sc2[complete.cases(sc2),]; sc2 <- sc2[sc2$Peso_racimo>0,]; rownames(sc2) <- 1:nrow(sc2)
sc2 <- data.frame(scenario='Escenario 2', peso_racimo=sc2$Peso_racimo)

sc3 <- read.csv('Z:/DATOS_PROCESADOS/_cosecha/all_clima_cycle.csv')
sc3 <- sc3[complete.cases(sc3),]; sc3 <- sc3[sc3$Peso_racimo>0,]; rownames(sc3) <- 1:nrow(sc3)
sc3 <- data.frame(scenario='Escenario 3', peso_racimo=sc3$Peso_racimo)

scenarios <- rbind(sc1, sc2, sc3)
rm(sc1, sc2, sc3)

library(ggplot2)

# ECDF by scenario
g <- ggplot(data=scenarios, aes(x=peso_racimo, colour=scenario)) + stat_ecdf(size=1.2)
g <- g + xlab('Peso del racimo (kg)') + ylab('Densidad acumulada')
g <- g + guides(colour=guide_legend(title=NULL))
g <- g + theme_bw()
g <- g + theme(axis.text  = element_text(size=18),
               axis.title = element_text(size=20, face="bold"),
               legend.text = element_text(size=18))
g
ggsave(filename='Z:/DOCUMENTOS/Informes/ecdf_scenario.png', plot=g, width=10, height=8, units='in')

library(cowplot)

# Histogram - scenario 1
g1 <- ggplot(data=scenarios[scenarios$scenario=='Escenario 1',], aes(x=peso_racimo, y=..density..)) + geom_histogram(colour='darkgray')
g1 <- g1 + xlab('Peso del racimo (kg)') + ylab('Densidad')
g1 <- g1 + theme_bw()
g1 <- g1 + theme(axis.text  = element_text(size=18),
                 axis.title = element_text(size=20, face="bold"))
g1

sc1 <- read.csv('Z:/DATOS_PROCESADOS/_cosecha/_banasan/banasan_cosechas_suelo_foliar_clima_cycle_corrected.csv')
sc1$fechaCosecha <- as.Date(sc1$fechaCosecha, '%m/%d/%Y')
g2 <- ggplot(data=sc1[sc1$Peso_racimo>0,], aes(x=fechaCosecha, y=Peso_racimo)) + geom_point()
g2 <- g2 + xlab('Fecha de cosecha') + ylab('Peso del racimo (kg)')
g2 <- g2 + theme_bw()
g2 <- g2 + theme(axis.text  = element_text(size=18),
                 axis.title = element_text(size=20, face="bold"))
g2

# ggsave(filename='Z:/DOCUMENTOS/Informes/histogram_scenario3.png', plot=g, width=8, height=8, units='in')

gcombine <- plot_grid(g1, g2) # labels=c("A", "B")
ggsave(filename='Z:/DOCUMENTOS/Informes/timeserie_scenario1.png', plot=gcombine, width=16, height=8, units='in')

# Histogram - scenario 2
g1 <- ggplot(data=scenarios[scenarios$scenario=='Escenario 2',], aes(x=peso_racimo, y=..density..)) + geom_histogram(colour='darkgray')
g1 <- g1 + xlab('Peso del racimo (kg)') + ylab('Densidad')
g1 <- g1 + theme_bw()
g1 <- g1 + theme(axis.text  = element_text(size=18),
                 axis.title = element_text(size=20, face="bold"))
g1

sc2 <- read.csv('Z:/DATOS_PROCESADOS/_cosecha/_cobana/cobana_cosechas_fertilizaciones_clima_cycle.csv')
sc2$fechaCosecha <- as.Date(sc2$fechaCosecha)
g2 <- ggplot(data=sc2[sc2$Peso_racimo>0,], aes(x=fechaCosecha, y=Peso_racimo, colour=)) + geom_point()
g2 <- g2 + xlab('Fecha de cosecha') + ylab('Peso del racimo (kg)')
g2 <- g2 + theme_bw()
g2 <- g2 + theme(axis.text  = element_text(size=18),
                 axis.title = element_text(size=20, face="bold"))
g2

# ggsave(filename='Z:/DOCUMENTOS/Informes/histogram_scenario3.png', plot=g, width=8, height=8, units='in')

gcombine <- plot_grid(g1, g2) # labels=c("A", "B")
ggsave(filename='Z:/DOCUMENTOS/Informes/timeserie_scenario2.png', plot=gcombine, width=16, height=8, units='in')

library(cowplot)

# Histogram - scenario 3
g1 <- ggplot(data=scenarios[scenarios$scenario=='Escenario 3',], aes(x=peso_racimo, y=..density..)) + geom_histogram(colour='darkgray')
g1 <- g1 + xlab('Peso del racimo (kg)') + ylab('Densidad')
g1 <- g1 + theme_bw()
g1 <- g1 + theme(axis.text  = element_text(size=18),
               axis.title = element_text(size=20, face="bold"))
g1

sc3 <- read.csv('Z:/DATOS_PROCESADOS/_cosecha/all_clima_cycle.csv')
sc3$fechaCosecha <- as.Date(sc3$fechaCosecha)
g2 <- ggplot(data=sc3[sc3$Peso_racimo>0,], aes(x=fechaCosecha, y=Peso_racimo)) + geom_point()
g2 <- g2 + xlab('Fecha de cosecha') + ylab('Peso del racimo (kg)')
g2 <- g2 + theme_bw()
g2 <- g2 + theme(axis.text  = element_text(size=18),
                 axis.title = element_text(size=20, face="bold"))
g2

# ggsave(filename='Z:/DOCUMENTOS/Informes/histogram_scenario3.png', plot=g, width=8, height=8, units='in')

gcombine <- plot_grid(g1, g2) # labels=c("A", "B")
ggsave(filename='Z:/DOCUMENTOS/Informes/timeserie_scenario3.png', plot=gcombine, width=16, height=8, units='in')

# Merma
merma_data <- read.csv('Z:/DATOS_PROCESADOS/_merma/all_merma_clima_cycle.csv')

# Histogram - scenario merma

library(cowplot)

g1 <- ggplot(data=merma_data[merma_data$Merma>0,], aes(x=Merma, y=..density..)) + geom_histogram(colour='darkgray')
g1 <- g1 + xlab('Merma') + ylab('Densidad')
g1 <- g1 + theme_bw()
g1 <- g1 + theme(axis.text  = element_text(size=18),
                 axis.title = element_text(size=20, face="bold"))
g1
#ggsave(filename='Z:/DOCUMENTOS/Informes/histogram_merma.png', plot=g1, width=8, height=8, units='in')

merma_data$fechaCosecha <- as.Date(merma_data$fechaCosecha)
g2 <- ggplot(data=merma_data[merma_data$Merma>0,], aes(x=fechaCosecha, y=Merma)) + geom_point()
g2 <- g2 + xlab('Fecha de cosecha') + ylab('Merma')
g2 <- g2 + theme_bw()
g2 <- g2 + theme(axis.text  = element_text(size=18),
                 axis.title = element_text(size=20, face="bold"))
g2
#ggsave(filename='Z:/DOCUMENTOS/Informes/timeserie_merma.png', plot=g2, width=8, height=8, units='in')

gcombine <- plot_grid(g1, g2) # labels=c("A", "B")
ggsave(filename='Z:/DOCUMENTOS/Informes/timeserie_merma.png', plot=gcombine, width=16, height=8, units='in')

# g <- ggplot(data=scenarios, aes(x=peso_racimo, colour=scenario)) + geom_density()
# g <- g + xlab('Peso del racimo (kg)') + ylab('Densidad acumulada')
# g <- g + guides(colour=guide_legend(title=NULL))
# g <- g + theme_bw()
# g <- g + theme(axis.text  = element_text(size=18),
#                axis.title = element_text(size=20, face="bold"),
#                legend.text = element_text(size=18))
# g

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Multivariate analysis - La Samaria data farm
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

library(FactoMineR)
library(factoextra)
library(ggplot2)

samaria <- read.csv('Z:/DATOS_PROCESADOS/_cosecha/_samaria/samaria_cosechas_finca.csv')
samaria <- samaria[complete.cases(samaria),]

# Week with two digits
for(i in 1:nrow(samaria))
{
  if(nchar(samaria$Week[i])==1){
    samaria$Week[i] <- paste('0',samaria$Week[i],sep='')
  }
}; rm(i)

# Creating ID variable
samaria$ID <- paste(samaria$Id_Lote, '-', samaria$Year, '-', samaria$Week, sep='')
rownames(samaria) <- samaria$ID
samaria <- samaria[,c('Year', 'Month', 'Week', 'Recobro', 'Racimos_cosechar', 'Merma', 'Peso_racimo', 'Cajas_total', 'Embolsados_lote', 'Ratio_total', 'Cajas_premio', 'Edad_cosecha')]
samaria$Week <- as.numeric(samaria$Week)

round(cor(samaria), 2)

samaria_pca <- PCA(samaria, scale.unit=TRUE, graph=FALSE)

# Variability explained by each factor
fviz_screeplot(samaria_pca, addlabels=TRUE) + theme_bw()

# Graph of variables
fviz_pca_var(samaria_pca, col.var="steelblue") + theme_bw()

# Control variable colors using their contributions
# Use gradient color
fviz_pca_var(samaria_pca, axes=c(1, 2), col.var="contrib") + scale_color_gradient2(low="white", mid="blue", high="red", midpoint=96) + theme_bw()

# Variable contributions on axis 1
fviz_contrib(samaria_pca, choice="var", axes=1)

# Variable contributions on axes 1 + 2
fviz_contrib(samaria_pca, choice="var", axes=1:2)

# Graph of individuals
fviz_pca_ind(samaria_pca, geom="text")

# Control automatically the color of individuals using the cos2
# cos2 = the quality of the individuals on the factor map
# Use points only
# Use gradient color
fviz_pca_ind(samaria_pca, col.ind="cos2", geom="point") +  scale_color_gradient2(low="blue", mid="white", high="red", midpoint=0.6) + theme_bw()

# Color by groups
p <- fviz_pca_ind(samaria_pca, geom="point", habillage=as.factor(samaria$Year), addEllipses=TRUE, ellipse.level=0.95) + theme_bw()
p <- p + scale_color_brewer(palette ="Set1")
p

# Biplot of individuals and variables
fviz_pca_biplot(samaria_pca, geom="point") + theme_bw()

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Multivariate analysis - La Samaria data plot
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

library(FactoMineR)
library(factoextra)
library(ggplot2)

samaria_lote <- read.csv('Z:/DATOS_PROCESADOS/_cosecha/_samaria/samaria_cosechas_lote.csv')
samaria_lote <- samaria_lote[,c("Id_Lote","Year","Week","Plantas_productivas","Plantas_lentas","Plantas_pobres","Resiembras","Espacios","Totales_productivas_hasta_resiembras","Totales_mas_espacios","Peso_racimo","Embolsados_lote")] # "Peso_racimo", "Racimos_cosechar"
samaria_lote <- samaria_lote[complete.cases(samaria_lote),]

# Week with two digits
for(i in 1:nrow(samaria_lote))
{
  if(nchar(samaria_lote$Week[i])==1){
    samaria_lote$Week[i] <- paste('0',samaria_lote$Week[i],sep='')
  }
}; rm(i)

# Creating ID variable
samaria_lote$ID <- paste(samaria_lote$Id_Lote, '-', samaria_lote$Year, '-', samaria_lote$Week, sep='')
rownames(samaria_lote) <- samaria_lote$ID
samaria_lote <- samaria_lote[,c('Week',"Plantas_productivas","Plantas_lentas","Plantas_pobres","Resiembras","Espacios","Totales_productivas_hasta_resiembras","Totales_mas_espacios","Peso_racimo","Embolsados_lote")]
samaria_lote$Week <- as.numeric(samaria_lote$Week)

round(cor(samaria_lote), 2)

samaria_lote_pca <- PCA(samaria_lote, scale.unit=TRUE, graph=FALSE)

# Variability explained by each factor
fviz_screeplot(samaria_lote_pca, addlabels=TRUE) + theme_bw()

# Graph of variables
fviz_pca_var(samaria_lote_pca, col.var="steelblue") + theme_bw()

# Control variable colors using their contributions
# Use gradient color
fviz_pca_var(samaria_lote_pca, axes=c(1, 2), col.var="contrib") + scale_color_gradient2(low="white", mid="blue", high="red", midpoint=96) + theme_bw()

# Variable contributions on axis 1
fviz_contrib(samaria_lote_pca, choice="var", axes=1)

# Variable contributions on axes 1 + 2
fviz_contrib(samaria_lote_pca, choice="var", axes=1:2)

# Graph of individuals
fviz_pca_ind(samaria_lote_pca, geom="text")

# Control automatically the color of individuals using the cos2
# cos2 = the quality of the individuals on the factor map
# Use points only
# Use gradient color
fviz_pca_ind(samaria_lote_pca, col.ind="cos2", geom="point") +  scale_color_gradient2(low="blue", mid="white", high="red", midpoint=0.6) + theme_bw()

# Color by groups
p <- fviz_pca_ind(samaria_lote_pca, geom="point", habillage=as.factor(samaria_lote$Week), addEllipses=TRUE, ellipse.level=0.95) + theme_bw()
p <- p + scale_color_brewer(palette ="Set1")
p

# Biplot of individuals and variables
fviz_pca_biplot(samaria_lote_pca, geom="point") + theme_bw()



# Time series of peso del racimo

library(readxl)

all <- read.csv('Z:/DATOS_PROCESADOS/_cosecha/all_clima.csv')
library(ggplot2)

all$fechaCosecha <- as.Date(as.character(all$fechaCosecha))
all$Peso_racimo2 <- scale(all$Peso_racimo)

ggplot(data=all, aes(x=fechaCosecha, y=Peso_racimo, colour=IDLote)) + geom_line() + theme_bw()








library(ClustOfVar)

#quantitative variables
data(decathlon)
tree <- hclustvar(decathlon[,1:10])
plot(tree)
#qualitative variables with missing values
data(vnf)
tree_NA <- hclustvar(X.quali=vnf)
plot(tree_NA)
dev.new()
vnf2<-na.omit(vnf)
tree <- hclustvar(X.quali=vnf2)
plot(tree)
#mixture of quantitative and qualitative variables
data(wine)
X.quanti <- wine[,c(3:29)]
X.quali <- wine[,c(1,2)]
tree <- hclustvar(X.quanti,X.quali)
plot(tree)
stab<-stability(tree, B=20)
plot(stab, nmax=7)


library(cluster)
daisy()
pam()

setwd("Z:/DATOS_PROCESADOS/_cosecha/_cobana") 
library("cluster")
# import and prepare data
myData <- read.csv(file='cobana_fertilizaciones.csv')
myData <- myData[,c("Year","Week","Finca","Racimos_cosechar_area","Merma","Recobro","Embolsados_lote","Perc_corta","Perc_premio","Ratio_premio",
                    "Ratio_total","Cajas_corta","Cajas_premio","Cajas_total","Peso_racimo","Grado","fertilizaciones","tipo_abono","tipo_aplicacion_fert")]
# make distance matrix
mydm <- daisy(myData)
cobana_clust  <- agnes(mydm, method="weighted")
plot(cobana_clust,  which.plot=2)
si1 <- silhouette(cutree(cobana_clust, k=8), daisy(myData))
plot(si1, nmax=120, cex.names=0.5)
abline(v=0.5, col=2)

summary(cobana_clust)

# do a whole range of clustering analyses 
fromk <- 1
tok <- 35
myresult <- data.frame() 
myarray <- array(c(fromk:tok)) 
myresult <- apply(myarray, 1, pam, x=mydm) 
# collect and plot scores
widthes <- matrix(data=NA, nrow=tok, ncol=2) 
colnames(widthes) <- c("k","width") 
for (i in fromk:tok) 
{ 
  widthes[i,1] <- i 
  if (i>1) 
  { 
    widthes[i,2] <- myresult[[i]]$silinfo$avg.width 
  } 
} 
plot(widthes[,2] ~ widthes[,1]) 
widthes
# write results, in this case of k = 5
write.table(unlist(myresult[5])$clustering, file="outfilename", sep="\t")
















