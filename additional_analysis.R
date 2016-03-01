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

g <- ggplot(data=scenarios, aes(x=peso_racimo, colour=scenario)) + stat_ecdf(size=1.2)
g <- g + xlab('Peso del racimo (kg)') + ylab('Densidad acumulada')
g <- g + guides(colour=guide_legend(title=NULL))
g <- g + theme_bw()
g <- g + theme(axis.text  = element_text(size=18),
               axis.title = element_text(size=20, face="bold"),
               legend.text = element_text(size=18))
g
ggsave(filename='Z:/DOCUMENTOS/Informes/ecdf_scenario.png', plot=g, width=10, height=8, units='in')

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

