# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Edaphoclimatic classification Colombia
# H. Achicanoy & G. Calberto
# CIAT, 2015
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

options(warn=-1)
library(raster)
library(rgdal)
library(maptools)
library(sp)

# Order files
# wkdir <- 'D:/ToBackup/AEPS-Big_data/Maps/Colombia_soils_crop'
# setwd(wkdir)

# # Original names
# setwd('D:/ToBackup/AEPS-Big_data/Maps/Colombia_soils_crop/clay_percent/CLYPPT_sd1_M')
# file.rename(from=list.files(), to=gsub(pattern='M_1km_T371.tif$', replacement='A.tif', list.files()))
# file.rename(from=list.files(), to=gsub(pattern='M_1km_T335.tif$', replacement='C.tif', list.files()))
# file.rename(from=list.files(), to=gsub(pattern='M_1km_T336.tif$', replacement='D.tif', list.files()))
# file.rename(from=list.files(), to=gsub(pattern='M_1km_T299.tif$', replacement='E.tif', list.files()))
# file.rename(from=list.files(), to=gsub(pattern='M_1km_T300.tif$', replacement='F.tif', list.files()))
# 
# setwd('D:/ToBackup/AEPS-Big_data/Maps/Colombia_soils_crop/soil_taxonomy_taxousda/TAXOUSDA')
# file.rename(from=list.files(), to=gsub(pattern='1km_T371.tif$', replacement='A.tif', list.files()))
# file.rename(from=list.files(), to=gsub(pattern='1km_T335.tif$', replacement='C.tif', list.files()))
# file.rename(from=list.files(), to=gsub(pattern='1km_T336.tif$', replacement='D.tif', list.files()))
# file.rename(from=list.files(), to=gsub(pattern='1km_T299.tif$', replacement='E.tif', list.files()))
# file.rename(from=list.files(), to=gsub(pattern='1km_T300.tif$', replacement='F.tif', list.files()))
# 
# # Transformed names
# setwd('D:/ToBackup/AEPS-Big_data/Maps/Colombia_soils_crop/clay_percent/CLYPPT_sd4_M')
# file.rename(from=list.files(), to=gsub(pattern='_1km', replacement='', list.files()))

# # Variables to analyse
# varList <- list.files()
# mtch <- grep('cec', varList)
# varList <- varList[mtch:length(varList)]; rm(mtch)
# 
# # Create directories
# lapply(1:length(varList), function(i)
# {
#   varDir <- paste('./', varList[i], sep='')
#   lList <- LETTERS[1:6]
#   lList <- lList[-2]
#   dir.create2 <- function(path){dir.create(path)}
#   dir.create2 <- Vectorize(FUN = dir.create2, vectorize.args='path')
#   dir.create2(path=paste(varDir, '/', lList, sep=''))
# })
# 
# # Move files into new directories
# lapply(1:length(varList), function(i)
# {
#   cat('\nProcessing:',varList[i],'\n')
#   # =-=-=-=-=-=-=-=-=-=-=-=-=-= #
#   # Original directory          #
#   # =-=-=-=-=-=-=-=-=-=-=-=-=-= #
#   varDir <- paste('./', varList[i], sep='')
#   grep2 <- Vectorize(FUN=function(pattern, x){grep(pattern, x)}, vectorize.args='pattern')
#   intFolders <- list.dirs(varDir, recursive=FALSE)[grep2(pattern=paste('_sd',1:4,'_M',sep=''),list.dirs(varDir, recursive=FALSE))]
#   
#   aDirOut <- paste(varDir, '/A', sep=''); if(!file.exists(aDirOut)){dir.create(aDirOut)}
#   cDirOut <- paste(varDir, '/C', sep=''); if(!file.exists(cDirOut)){dir.create(cDirOut)}
#   dDirOut <- paste(varDir, '/D', sep=''); if(!file.exists(dDirOut)){dir.create(dDirOut)}
#   eDirOut <- paste(varDir, '/E', sep=''); if(!file.exists(eDirOut)){dir.create(eDirOut)}
#   fDirOut <- paste(varDir, '/F', sep=''); if(!file.exists(fDirOut)){dir.create(fDirOut)}
#   
#   lapply(1:length(intFolders), function(j)
#   {
#     cat('Extracting files from:',intFolders[j],'\n')
#     # List & and copy internal files
#     intFiles <- list.files(intFolders[j], full.names=TRUE)
#     aFiles <- intFiles[grep(pattern='*_A.tif$', intFiles)]; file.copy(from=aFiles, to=aDirOut, overwrite=FALSE)
#     cFiles <- intFiles[grep(pattern='*_C.tif$', intFiles)]; file.copy(from=cFiles, to=cDirOut, overwrite=FALSE)
#     dFiles <- intFiles[grep(pattern='*_D.tif$', intFiles)]; file.copy(from=dFiles, to=dDirOut, overwrite=FALSE)
#     eFiles <- intFiles[grep(pattern='*_E.tif$', intFiles)]; file.copy(from=eFiles, to=eDirOut, overwrite=FALSE)
#     fFiles <- intFiles[grep(pattern='*_F.tif$', intFiles)]; file.copy(from=fFiles, to=fDirOut, overwrite=FALSE)
#   })
#   
#   file.remove(intFolders)
#   
# })

# # Run python code in order to create a mosaike
# library(PythonInR)
# autodetectPython('C:/Python34/python.exe')
# 
# setMethod("pySetPoly", signature(key="character", value = "integer"),
#           function(key, value){
#             success <- pySetSimple(key, list(vector=unname(value), names=names(value), rClass=class(value)))
#             cmd <- sprintf("%s = PythonInR.prVector(%s['vector'], %s['names'], %s['rClass'])", 
#                            key, key, key, key)
#             pyExec(cmd)
#           })
# 
# pySetPoly <- PythonInR:::pySetPoly
# showMethods("pySetPoly")
# 
# pySet("x", 1:3)
# pyPrint(x)
# pyType("x")
# 
# setMethod("pySetPoly",
#           signature(key="character", value = "integer"),
#           function(key, value){
#             PythonInR:::pySetSimple(key, value)
#           })
# 
# pySet("x", 1:3)
# pyPrint(x)
# pyType("x")

# Change names of files
# list2chng <- lapply(1:length(varList), function(i){list.dirs(path=paste('./', varList[i], sep=''),recursive=F)})

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Setting temporary directory
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# rasterOptions(tmpdir="D:/rtemp") # local
rasterOptions(tmpdir="/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA/RESULTADOS/Clasificacion_edafoclimatica/_maps/rtemp")

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# 1. Load climate rasters
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# climFiles <- list.dirs(path='D:/ToBackup/AEPS-Big_data/Maps/Colombia_worldclim_crop', recursive=TRUE) # local
climFiles <- list.dirs(path='/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA/RESULTADOS/Clasificacion_edafoclimatica/_maps/Colombia_worldclim_crop', recursive=TRUE)

grep2 <- function(pattern, x){grep(pattern, x)}
grep2 <- Vectorize(FUN=grep2, vectorize.args='pattern')

climFiles <- climFiles[grep2(pattern=paste('*bio_',1:20,'/bio_',1:20,sep=''), x=climFiles)]
climRasters <- raster::stack(climFiles)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# 2. Load soil rasters
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# soilFiles <- list.dirs(path='D:/ToBackup/AEPS-Big_data/Maps/Colombia_soils_center_north/_extract', recursive=TRUE) # local
soilFiles <- list.dirs(path='/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA/RESULTADOS/Clasificacion_edafoclimatica/_maps/Colombia_soils_crop/_extract', recursive=TRUE)
bulkFiles <- soilFiles[grep2(pattern=paste('*bulk_density_mosaic_col/bld_sd',1:4,sep=''), soilFiles)]
restFiles <- soilFiles[grep2(pattern='*band_1', soilFiles)]
soilFiles <- c(bulkFiles, restFiles); rm(bulkFiles, restFiles)

depth1Files <- c(soilFiles[grep(pattern='*_sd1', soilFiles)], soilFiles[length(soilFiles)]); depth1Rasters <- raster::stack(depth1Files[-length(depth1Files)])
depth2Files <- c(soilFiles[grep(pattern='*_sd2', soilFiles)], soilFiles[length(soilFiles)]); depth2Rasters <- raster::stack(depth2Files[-length(depth2Files)])
depth3Files <- c(soilFiles[grep(pattern='*_sd3', soilFiles)], soilFiles[length(soilFiles)]); depth3Rasters <- raster::stack(depth3Files[-length(depth3Files)])
depth4Files <- c(soilFiles[grep(pattern='*_sd4', soilFiles)], soilFiles[length(soilFiles)]); depth4Rasters <- raster::stack(depth4Files[-length(depth4Files)])

allCombinations <- list(raster::stack(climRasters, depth1Rasters),
                        raster::stack(climRasters, depth2Rasters),
                        raster::stack(climRasters, depth3Rasters),
                        raster::stack(climRasters, depth4Rasters))

rm(climRasters, depth1Rasters, depth2Rasters, depth3Rasters, depth4Rasters)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# 3. For each combination of climate data and different depth of soils run DBSCAN
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

depths <- c('0_5cm', '5_15cm', '15_30cm', '30_60cm')
# Results were saved on Harold's personal space within server
lapply(1:length(depths), function(comb){
  
  library(ff)
  
  # Create a table where nrow = ncell of Colombia's raster, ncol = number of variables to analyze
  rTemplate <- allCombinations[[comb]][[1]]
  rTemplate <- ff(1,dim=c(ncell(rTemplate),28),vmode="double") # 28 variables (climate and soils) [USDA classification was not used]
  
  library(parallel)
  
  # source('D:/ToBackup/AEPS-Big_data/Convenio_MADR/Clasificacion_edafoclimatica/mclapply2.R') # local
  mclapply(1:28, function(i){z <- allCombinations[[comb]][[i]]; t <- getValues(z); cat('Processing: biovariable',i,'\n'); rTemplate[,i] <- t[]; return(cat("Done\n"))}, mc.cores=10)
  
  rTemplate <- as.ffdf(rTemplate)
  names(rTemplate) <- paste0("variable.",1:28)
  rTemplate <- as.data.frame(rTemplate)
  rownames(rTemplate) <- 1:nrow(rTemplate)
  rTemplate <- rTemplate[complete.cases(rTemplate),] # Use complete data
  
  # ------------------------------------ #
  # Run DBSCAN, alternative 1
  # ------------------------------------ #
  # library(fpc)
  # set.seed(1235)
  # dsResults <- dbscan(rTemplate, 0.2)
  
  # ------------------------------------ #
  # Run DBSCAN, alternative 2
  # ------------------------------------ #
  library(dbscan)
  
  set.seed(1235)
  dbResults <- dbscan::dbscan(rTemplate, eps=100, minPts=28+1) # Identify eps value, k value is data's dimensionality + 1
  # save(dbResults, file='/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA/RESULTADOS/Clasificacion_edafoclimatica/DBSCAN_colombia.RData') # Problems with destiny folder
  save(dbResults, file=paste('/home/hachicanoy/DBSCAN100-1_colombia_',depths[comb],'.RData',sep=''))
  
  # Reconstruct Colombia's raster with cluster categories
  
  template <- allCombinations[[comb]][[1]]
  template[] <- NA
  template[as.numeric(rownames(rTemplate))] <- dbResults$cluster
  plot(template)
  
  writeRaster(template, filename=paste('/home/hachicanoy/DBSCAN100-1_clusters_colombia_',depths[comb],'.tif',sep=''), format="GTiff", overwrite=TRUE)
  
  # Delete all temporary files created during process
  removeTmpFiles(h=0)
  
  return(cat('Done!\n'))
  
})

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# 4. For each combination of climate data and different depth of soils evaluate results
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Load packages

options(warn=-1)
if(!require(readxl)){install.packages('readxl'); library(readlx)} else {library(readxl)}
if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)}
if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)}
if(!require(rasterVis)){install.packages('rasterVis'); library(rasterVis)} else {library(rasterVis)}

setwd('/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA')

# Load cluster rasters, for this process use Magdalena and La Guajira rasters

# rList <- list.files(path='C:/WorkSpace/extract/output', pattern='*.tif$', full.names=T) # local
rList <- list.files(path='./RESULTADOS/Clasificacion_edafoclimatica/_results/climate_soils/mag_gua', pattern='*.tif$', full.names=T) # local
rList <- rList[c(1,4,2,3)]
rStack <- raster::stack(rList); rm(rList)

# Extract name and coordinates from each farm in order to classify them on the calculated clusters

# harvest_data <- read_excel(path='D:/ToBackup/AEPS-Big_data/Convenio_MADR/Banano/_data/Banasan_data.xlsx', sheet=1) # local
harvest_data <- read_excel(path='./DATOS_PROCESADOS/Banasan_data.xlsx', sheet=1)
harvest_data <- as.data.frame(harvest_data)
coord <- unique(harvest_data[,c('Finca','Longitud','Latitud')])
rownames(coord) <- 1:nrow(coord)
clusters4depth <- cbind(coord, as.data.frame(raster::extract(x=rStack, y=coord[,c('Longitud','Latitud')], method='simple')))
clusters4depth <- as.data.frame(clusters4depth)
colnames(clusters4depth)[4:ncol(clusters4depth)] <- paste('depth', 1:4, sep='')

# Map of clusters found
names(rStack) <- c("Profundidad_0_5cm", "Profundidad_5_15cm", "Profundidad_15_30cm", "Profundidad_30_60cm")
# dptos <- shapefile('C:/WorkSpace/extract/mascara/magdalena_guajira.shp') # local
dptos <- shapefile('./RESULTADOS/Clasificacion_edafoclimatica/_maps/Magdalena_Guajira_clusters_crop/mascara/magdalena_guajira.shp')
# png(filename='D:/ToBackup/AEPS-Big_data/Convenio_MADR/Informes/clusters.png', width=8, height=8, units='in', pointsize=30, res=300) # local
png(filename='./DOCUMENTOS/Informe/clusters.png', width=8, height=8, units='in', pointsize=30, res=300)
  levelplot(rStack, par.settings=RdBuTheme) + layer(sp.polygons(dptos))
dev.off()

# Important: For all evaluated depths the farms don't change of class
# Identified clusters: 2 and 6

# Work only with clusters 2 and 6
rStack2 <- rStack
rStack2[rStack2[] != 2 & rStack2[] != 6] <- NA

# png(filename='D:/ToBackup/AEPS-Big_data/Convenio_MADR/Informes/clusters_fincas.png', width=8, height=8, units='in', pointsize=30, res=300) # local
png(filename='./DOCUMENTOS/Informe/clusters_fincas.png', width=8, height=8, units='in', pointsize=30, res=300)
  levelplot(rStack2, par.settings=RdBuTheme) + layer(sp.polygons(dptos)) + layer(sp.points(obj=SpatialPoints(coords=coord[,2:3]), col='green'))
dev.off()
rm(coord)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# 4.1 Evaluate yield by cluster
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

harvest_data$clusterClass <- NA
for(i in 1:nrow(clusters4depth))
{
  harvest_data$clusterClass[harvest_data$Finca==clusters4depth$Finca[i]] <- clusters4depth$depth1[clusters4depth$Finca==clusters4depth$Finca[i]]
}; rm(i, clusters4depth)

# Explore differences by variable between clusters

harvest_data <- harvest_data[which(harvest_data$Peso_racimo!=0),]
rownames(harvest_data) <- 1:nrow(harvest_data)

# Weight of banana bunch
if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)}
p <- ggplot(harvest_data, aes(Peso_racimo, fill=factor(clusterClass)))
p <- p + xlab('Peso (kg/racimo)') + ylab('Density')
p <- p + geom_density(alpha=0.2)
p <- p + geom_rug(col=rgb(.5,0,0,alpha=.2))
p <- p + theme_minimal() + theme_bw()
p <- p + theme(axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               axis.title.x = element_text(face="bold",size=11),
               axis.title.y = element_text(face="bold",size=11),
               legend.title = element_text(face="bold",size=11),
               legend.text  = element_text(size=10))
p <- p + labs(fill="Clúster")
# ggsave(filename='D:/ToBackup/AEPS-Big_data/Convenio_MADR/Informes/peso_racimo_cluster.png', plot=p, width=8, height=8, units='in', dpi=300) # local
ggsave(filename='./DOCUMENTOS/Informe/peso_racimo_cluster.png', plot=p, width=8, height=8, units='in', dpi=300)

# Decrease's percent
harvest_data_merma <- harvest_data[which(harvest_data$Merma > 0),]
library(ggplot2)
p <- ggplot(harvest_data_merma, aes(Merma, fill=factor(clusterClass)))
p <- p + xlab('Porcentaje (%)') + ylab('Density')
p <- p + geom_density(alpha=0.2)
p <- p + geom_rug(col=rgb(.5,0,0,alpha=.2))
p <- p + theme_minimal() + theme_bw()
p <- p + theme(axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               axis.title.x = element_text(face="bold",size=11),
               axis.title.y = element_text(face="bold",size=11),
               legend.title = element_text(face="bold",size=11),
               legend.text  = element_text(size=10))
p <- p + labs(fill="Clúster")
# ggsave(filename='D:/ToBackup/AEPS-Big_data/Convenio_MADR/Informes/perc_merma_cluster.png', plot=p, width=8, height=8, units='in', dpi=300) # local
ggsave(filename='./DOCUMENTOS/Informe/perc_merma_cluster.png', plot=p, width=8, height=8, units='in', dpi=300)

# Short's percent
library(ggplot2)
p <- ggplot(harvest_data, aes(Perc_corta, fill=factor(clusterClass)))
p <- p + xlab('Porcentaje (%)') + ylab('Density')
p <- p + geom_density(alpha=0.2)
p <- p + geom_rug(col=rgb(.5,0,0,alpha=.2))
p <- p + theme_minimal() + theme_bw()
p <- p + theme(axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               axis.title.x = element_text(face="bold",size=11),
               axis.title.y = element_text(face="bold",size=11),
               legend.title = element_text(face="bold",size=11),
               legend.text  = element_text(size=10))
p <- p + labs(fill="Clúster")
# ggsave(filename='D:/ToBackup/AEPS-Big_data/Convenio_MADR/Informes/perc_corta_cluster.png', plot=p, width=8, height=8, units='in', dpi=300) # local
ggsave(filename='./DOCUMENTOS/Informe/perc_corta_cluster.png', plot=p, width=8, height=8, units='in', dpi=300)

# Porcentaje de premio
library(ggplot2)
p <- ggplot(harvest_data, aes(Perc_premio, fill=factor(clusterClass)))
p <- p + xlab('Porcentaje (%)') + ylab('Density')
p <- p + geom_density(alpha=0.2)
p <- p + geom_rug(col=rgb(.5,0,0,alpha=.2))
p <- p + theme_minimal() + theme_bw()
p <- p + theme(axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               axis.title.x = element_text(face="bold",size=11),
               axis.title.y = element_text(face="bold",size=11),
               legend.title = element_text(face="bold",size=11),
               legend.text  = element_text(size=10))
p <- p + labs(fill="Clúster")
# ggsave(filename='D:/ToBackup/AEPS-Big_data/Convenio_MADR/Informes/perc_premio_cluster.png', plot=p, width=8, height=8, units='in', dpi=300) # local
ggsave(filename='./DOCUMENTOS/Informe/perc_premio_cluster.png', plot=p, width=8, height=8, units='in', dpi=300)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# 4.2 For each combination of climate data and different depth of soils evaluate climate results
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

cluster_coord <- as.data.frame(xyFromCell(object=rStack[[2]],cell=1:ncell(rStack[[2]])))
cluster_coord$cluster <- rStack[][,2]
cluster_coord <- cluster_coord[complete.cases(cluster_coord),]
rownames(cluster_coord) <- 1:nrow(cluster_coord)

# rasterOptions(tmpdir="D:/rtemp") # local
rasterOptions(tmpdir='./RESULTADOS/rtemp')

# climFiles <- list.dirs(path='D:/ToBackup/AEPS-Big_data/Maps/Colombia_worldclim_crop', recursive=TRUE) # local
climFiles <- list.dirs(path='./RESULTADOS/Clasificacion_edafoclimatica/_maps/Colombia_worldclim_crop', recursive=TRUE)

grep2 <- function(pattern, x){grep(pattern, x)}
grep2 <- Vectorize(FUN=grep2, vectorize.args='pattern')
climFiles <- climFiles[grep2(pattern=paste('*bio_',1:20,'/bio_',1:20,sep=''), x=climFiles)]
climRasters <- raster::stack(climFiles); rm(climFiles)

# soilFiles <- list.dirs(path='D:/ToBackup/AEPS-Big_data/Maps/Colombia_soils_crop/_extract', recursive=TRUE) # local
soilFiles <- list.dirs(path='./RESULTADOS/Clasificacion_edafoclimatica/_maps/Colombia_soils_crop/_extract', recursive=TRUE)
bulkFiles <- soilFiles[grep2(pattern=paste('*bulk_density_mosaic_col/bld_sd',1:4,sep=''), soilFiles)]
restFiles <- soilFiles[grep2(pattern='*band_1', soilFiles)]
soilFiles <- c(bulkFiles, restFiles); rm(bulkFiles, restFiles)
depth2Files <- c(soilFiles[grep(pattern='*_sd2', soilFiles)], soilFiles[length(soilFiles)]); depth2Rasters <- raster::stack(depth2Files[-length(depth2Files)])
# soilNames <- gsub(pattern='_mosaic_col',replacement='',list.files(path='D:/ToBackup/AEPS-Big_data/Maps/Colombia_soils_crop/_extract')) # local
soilNames <- gsub(pattern='_mosaic_col',replacement='',list.files(path='./RESULTADOS/Clasificacion_edafoclimatica/_maps/Colombia_soils_crop/_extract'))
names(depth2Rasters) <- soilNames[1:8]; rm(soilNames); rm(depth2Files)

allRasters <- raster::stack(climRasters, depth2Rasters)
rm(climRasters, soilFiles, depth2Rasters)

cluster_coord <- cbind(cluster_coord, as.data.frame(raster::extract(x=allRasters, y=cluster_coord[,c('x','y')])))
# write.csv(cluster_coord, file='D:/ToBackup/AEPS-Big_data/Convenio_MADR/Informes/clusterSWD4DMFA.csv', row.names=F) # local
write.csv(cluster_coord, file='./DOCUMENTOS/Informe/clusterSWD4DMFA.csv', row.names=F)

grep2 <- function(pattern, x){grep(pattern, x)}
grep2 <- Vectorize(FUN=grep2, vectorize.args='pattern')

IDclusters <- c(0, 1, 2, 3, 6)

# Solo clústers representativos
cluster_coord2 <- cluster_coord
rownames(cluster_coord2) <- 1:nrow(cluster_coord2)
cluster_coord2_0 <- subset(cluster_coord2, subset=cluster_coord2$cluster==0)
cluster_coord2_1 <- subset(cluster_coord2, subset=cluster_coord2$cluster==1)
cluster_coord2_2 <- subset(cluster_coord2, subset=cluster_coord2$cluster==2)
cluster_coord2_3 <- subset(cluster_coord2, subset=cluster_coord2$cluster==3)
cluster_coord2_6 <- subset(cluster_coord2, subset=cluster_coord2$cluster==6)
cluster_coord2 <- rbind(cluster_coord2_0, cluster_coord2_1, cluster_coord2_2, cluster_coord2_3, cluster_coord2_6)
rm(cluster_coord2_0, cluster_coord2_1, cluster_coord2_2, cluster_coord2_3, cluster_coord2_6)
cluster_coord2$cluster <- as.factor(cluster_coord2$cluster)
cluster_coord2 <- cluster_coord2[complete.cases(cluster_coord2),]
rownames(cluster_coord2) <- 1:nrow(cluster_coord2)

# Kruskal-Walis test by variable
if(!require(agricolae)){install.packages('agricolae'); library(readlx)} else {library(agricolae)}

lapply(names(cluster_coord2)[4:ncol(cluster_coord2)], function(i)
{
  
  varInf <- read.csv('./RESULTADOS/Clasificacion_edafoclimatica/varInfo_labels.csv')
  
  cat('Processing:',i,'\n')
  post.kruskal   <- kruskal(y=eval(parse(text=paste('cluster_coord2$',i,sep=''))), trt=cluster_coord2$cluster, alpha=0.05, group=T, p.adj='bonferroni', console=F)
  pk.groups      <- post.kruskal$means # By mean within-group
  pk.groups$trt  <- rownames(pk.groups)
  pk.groups      <- merge(pk.groups, post.kruskal$groups, by='trt')
  pk.dist        <- dist(pk.groups[,2])
  pk.clus        <- hclust(pk.dist, method = "average")
  pk.clus$labels <- paste0(gsub("[[:space:]]", "", pk.groups$trt), "_", pk.groups$M, sep="")
  
  # Only for temperature variables (try to define a strategy for other variables)
  if(varInf$var_source[which(varInf$var==i)]=='Temperatura'|varInf$var_source[which(varInf$var==i)]=='ph')
  {
    kruskal.inf <- data.frame(cluster   = rownames(post.kruskal$means),
                              var_value = round(post.kruskal$means[,1]/10,2),
                              coef_var  = round(post.kruskal$means[,2]/post.kruskal$means[,1],2))
    kruskal.inf <- kruskal.inf[order(kruskal.inf$var_value),]
    kruskal.inf$order <- as.numeric(rownames(kruskal.inf))
    rownames(kruskal.inf) <- 1:nrow(kruskal.inf)
    
    pk.clus$order <- kruskal.inf$order
  } else{
    kruskal.inf <- data.frame(cluster   = rownames(post.kruskal$means),
                              var_value = round(post.kruskal$means[,1],2),
                              coef_var  = round(post.kruskal$means[,2]/post.kruskal$means[,1],2))
    kruskal.inf <- kruskal.inf[order(kruskal.inf$var_value),]
    kruskal.inf$order <- as.numeric(rownames(kruskal.inf))
    rownames(kruskal.inf) <- 1:nrow(kruskal.inf)
    
    pk.clus$order <- kruskal.inf$order
  }
  
  # Plotting information
  colfunc <- colorRampPalette(c("springgreen", "royalblue"))
  
  png(filename=paste('./DOCUMENTOS/Informe/', i, '.png', sep=''), res=300, pointsize=15, width=8, height=8, units='in')
  par(xpd=TRUE)
  plot(pk.clus, hang=-1, xlab='', ylab='Disimilaridad', ylim=c(-round(pk.clus$height[length(pk.clus$height)]*.4,2),Inf), main=paste(varInf$nice_label[which(varInf$var==i)]), sub="")
  points(x=1:5, y=rep(-round(pk.clus$height[length(pk.clus$height)]*.2,2),5), cex=10, col=colfunc(5), pch=20)
  text(x=1:5, y=rep(-round(pk.clus$height[length(pk.clus$height)]*.2,2),5), labels=kruskal.inf$var_value)
  dev.off()
  
  return(cat('Done!\n'))
  
})

# Por variable puedo tener:
# 1. Promedio en el grupo
# 2. Desviación estandar (coeficiente de variación)
# 3. 



# vector of colors labelColors = c('red', 'blue', 'darkgreen', 'darkgrey',
# 'purple')
labelColors = c("#CDB380", "#036564", "#EB6841")
# cut dendrogram in 4 clusters
clusMember = cutree(pk.clus, 3)
# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}
# using dendrapply
clusDendro = dendrapply(dend, colLab)
# make plot
par(mar=c(7,1.7,0.5,0.5)+0.1)
plot(clusDendro, type="triangle", cex.axis=0.8, cex.lab=0.8)








# Temperatura promedio anual
library(ggplot2)
p <- ggplot(cluster_coord2, aes(bio_1, fill=factor(cluster)))
p <- p + xlab('Grados centigrados * 10') + ylab('Density')
p <- p + geom_density(alpha=0.2)
p <- p + geom_rug(col=rgb(.5,0,0,alpha=.2))
p <- p + theme_minimal() + theme_bw()
p <- p + theme(axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               axis.title.x = element_text(face="bold",size=11),
               axis.title.y = element_text(face="bold",size=11),
               legend.title = element_text(face="bold",size=11),
               legend.text  = element_text(size=10))
p <- p + labs(fill="Clúster")
ggsave(filename='D:/ToBackup/AEPS-Big_data/Convenio_MADR/Informes/bio1_cluster.png', plot=p, width=8, height=8, units='in', dpi=300)

library(FactoMineR)

dmfa_results <- DMFA(don=cluster_coord2[,3:ncol(cluster_coord2)], num.fact=1)
save.image('D:/ToBackup/AEPS-Big_data/Convenio_MADR/ASBAMA/Informes/caracterizacion_clusters.RData')

###################################################################
# Analisis de datos climaticos por clusters
###################################################################

# Utilizar tecnicas multivariadas para todos los clusters

cluster_coord <- as.data.frame(xyFromCell(object=rStack[[2]],cell=1:ncell(rStack[[2]])))
cluster_coord$cluster <- rStack[][,2]
cluster_coord <- cluster_coord[complete.cases(cluster_coord),]
rownames(cluster_coord) <- 1:nrow(cluster_coord)

rasterOptions(tmpdir="D:/rtemp") # local

climFiles <- list.dirs(path='D:/ToBackup/AEPS-Big_data/Maps/Colombia_worldclim_crop', recursive=TRUE) # local
grep2 <- function(pattern, x){grep(pattern, x)}
grep2 <- Vectorize(FUN=grep2, vectorize.args='pattern')
climFiles <- climFiles[grep2(pattern=paste('*bio_',1:20,'/bio_',1:20,sep=''), x=climFiles)]
climRasters <- raster::stack(climFiles)

soilFiles <- list.dirs(path='D:/ToBackup/AEPS-Big_data/Maps/Colombia_soils_center_north/_extract', recursive=TRUE) # local
bulkFiles <- soilFiles[grep2(pattern=paste('*bulk_density_mosaic_col/bld_sd',1:4,sep=''), soilFiles)]
restFiles <- soilFiles[grep2(pattern='*band_1', soilFiles)]
soilFiles <- c(bulkFiles, restFiles); rm(bulkFiles, restFiles)
depth2Files <- c(soilFiles[grep(pattern='*_sd2', soilFiles)], soilFiles[length(soilFiles)]); depth2Rasters <- raster::stack(depth2Files[-length(depth2Files)])

allRasters <- raster::stack(climRasters, depth2Rasters)
rm(climFiles, climRasters, soilFiles)

cluster_coord <- cbind(cluster_coord, as.data.frame(raster::extract(x=allRasters, y=cluster_coord[,c('x','y')])))
write.csv(cluster_coord, file='D:/ToBackup/AEPS-Big_data/Convenio_MADR/Informes/clusterSWD4DMFA.csv', row.names=F)

table(cluster_coord$cluster)

grep2 <- function(pattern, x){grep(pattern, x)}
grep2 <- Vectorize(FUN=grep2, vectorize.args='pattern')

IDclusters <- c(0, 1, 2, 3, 6)

# Solo clústers representativos
cluster_coord2 <- cluster_coord
rownames(cluster_coord2) <- 1:nrow(cluster_coord2)
cluster_coord2 <- cluster_coord2[match(IDclusters, cluster_coord2$cluster),] # unlist(grep2(pattern=IDclusters, x=cluster_coord2$cluster))
cluster_coord2_0 <- subset(cluster_coord2, subset=cluster_coord2$cluster==0)
cluster_coord2_1 <- subset(cluster_coord2, subset=cluster_coord2$cluster==1)
cluster_coord2_2 <- subset(cluster_coord2, subset=cluster_coord2$cluster==2)
cluster_coord2_3 <- subset(cluster_coord2, subset=cluster_coord2$cluster==3)
cluster_coord2_6 <- subset(cluster_coord2, subset=cluster_coord2$cluster==6)
cluster_coord2 <- rbind(cluster_coord2_0, cluster_coord2_1, cluster_coord2_2, cluster_coord2_3, cluster_coord2_6)
rm(cluster_coord2_0, cluster_coord2_1, cluster_coord2_2, cluster_coord2_3, cluster_coord2_6)
cluster_coord2$cluster <- as.factor(cluster_coord2$cluster)
cluster_coord2 <- cluster_coord2[complete.cases(cluster_coord2),]

names(cluster_coord2)

# Temperatura promedio anual
library(ggplot2)
p <- ggplot(cluster_coord2, aes(bio_1, fill=factor(cluster)))
p <- p + xlab('Grados centigrados * 10') + ylab('Density')
p <- p + geom_density(alpha=0.2)
p <- p + geom_rug(col=rgb(.5,0,0,alpha=.2))
p <- p + theme_minimal() + theme_bw()
p <- p + theme(axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               axis.title.x = element_text(face="bold",size=11),
               axis.title.y = element_text(face="bold",size=11),
               legend.title = element_text(face="bold",size=11),
               legend.text  = element_text(size=10))
p <- p + labs(fill="Clúster")
ggsave(filename='D:/ToBackup/AEPS-Big_data/Convenio_MADR/Informes/bio1_cluster.png', plot=p, width=8, height=8, units='in', dpi=300)

library(FactoMineR)

dmfa_results <- DMFA(don=cluster_coord2[,3:ncol(cluster_coord2)], num.fact=1)
save.image('D:/ToBackup/AEPS-Big_data/Convenio_MADR/Informes/caracterizacion_clusters.RData')










# =========================================================================================================================================== #
# Caracterización de clústers edafoclimáticos
# =========================================================================================================================================== #

cluster_coord <- read.csv('D:/ToBackup/AEPS-Big_data/Convenio_MADR/ASBAMA/Informes/clusterSWD4DMFA.csv')

grep2 <- function(pattern, x){grep(pattern, x)}
grep2 <- Vectorize(FUN=grep2, vectorize.args='pattern')

IDclusters <- c(1, 2, 3, 6)

# Solo clústers representativos
cluster_coord2 <- cluster_coord
cluster_coord2_1 <- subset(cluster_coord2, subset=cluster_coord2$cluster==1)
cluster_coord2_2 <- subset(cluster_coord2, subset=cluster_coord2$cluster==2)
cluster_coord2_3 <- subset(cluster_coord2, subset=cluster_coord2$cluster==3)
cluster_coord2_6 <- subset(cluster_coord2, subset=cluster_coord2$cluster==6)
cluster_coord2 <- rbind(cluster_coord2_1, cluster_coord2_2, cluster_coord2_3, cluster_coord2_6)
rm(cluster_coord2_1, cluster_coord2_2, cluster_coord2_3, cluster_coord2_6)
cluster_coord2$cluster <- as.factor(cluster_coord2$cluster)
cluster_coord2 <- cluster_coord2[complete.cases(cluster_coord2),]
rownames(cluster_coord2) <- 1:nrow(cluster_coord2)
rm(cluster_coord)

library(FactoMineR)

# pca_g0 <- FactoMineR::PCA(cluster_coord2[cluster_coord2$cluster==0,-c(1:3)], scale.unit=TRUE)
# View(pca_g0$eig)
# View(pca_g0$var$contrib)

pca_g1 <- FactoMineR::PCA(cluster_coord2[cluster_coord2$cluster==1,-c(1:3)], scale.unit=TRUE, graph=FALSE)
pca_g2 <- FactoMineR::PCA(cluster_coord2[cluster_coord2$cluster==2,-c(1:3)], scale.unit=TRUE, graph=FALSE)
pca_g3 <- FactoMineR::PCA(cluster_coord2[cluster_coord2$cluster==3,-c(1:3)], scale.unit=TRUE, graph=FALSE)
pca_g6 <- FactoMineR::PCA(cluster_coord2[cluster_coord2$cluster==6,-c(1:3)], scale.unit=TRUE, graph=FALSE)

g1_var <- apply(X=pca_g1$var$cor[,1:3], MARGIN=2, FUN=function(x){z <- x > 0.9|x < -0.9; return(z)})
g2_var <- apply(X=pca_g2$var$cor[,1:5], MARGIN=2, FUN=function(x){z <- x > 0.9|x < -0.9; return(z)})
g3_var <- apply(X=pca_g3$var$cor[,1:3], MARGIN=2, FUN=function(x){z <- x > 0.9|x < -0.9; return(z)})
g6_var <- apply(X=pca_g6$var$cor[,1:5], MARGIN=2, FUN=function(x){z <- x > 0.9|x < -0.9; return(z)})

g1_var <- rownames(g1_var)[rowSums(g1_var)>0]
g2_var <- rownames(g2_var)[rowSums(g2_var)>0]
g3_var <- rownames(g3_var)[rowSums(g3_var)>0]
g6_var <- rownames(g6_var)[rowSums(g6_var)>0]

g_var <- c(g1_var, g2_var, g3_var, g6_var)
g_var2 <- unique(g_var)
g_var2 <- g_var2[-grep2(pattern='bio_20$', x=g_var2)]

temp <- g_var2[grep2(pattern=paste('bio_',1:11,'$',sep=''), x=g_var2)]
prec <- g_var2[grep2(pattern=paste('bio_',12:19,'$',sep=''), x=g_var2)]
soil <- g_var2[setdiff(1:length(g_var2), grep2(pattern=paste('bio_',1:19,'$',sep=''), x=g_var2))]

library(ggplot2)

eig_table1 <- pca_g1$eig
eig_table1$Cluster <- 'Cluster 1'
eig_table1$princ_comp <- 1:nrow(eig_table1)
eig_table1$cond <- eig_table1$`cumulative percentage of variance` < 80
eig_table1$cond[1:(which(eig_table1$cond==TRUE)[length(which(eig_table1$cond==TRUE))] + 1)] <- 1
eig_table1$cond[(which(eig_table1$cond==TRUE)[length(which(eig_table1$cond==TRUE))] + 2):length(eig_table1$cond)] <- 0

eig_table2 <- pca_g2$eig
eig_table2$Cluster <- 'Cluster 2'
eig_table2$princ_comp <- 1:nrow(eig_table2)
eig_table2$cond <- eig_table2$`cumulative percentage of variance` < 80
eig_table2$cond[1:(which(eig_table2$cond==TRUE)[length(which(eig_table2$cond==TRUE))] + 1)] <- 1
eig_table2$cond[(which(eig_table2$cond==TRUE)[length(which(eig_table2$cond==TRUE))] + 2):length(eig_table2$cond)] <- 0

eig_table3 <- pca_g3$eig
eig_table3$Cluster <- 'Cluster 3'
eig_table3$princ_comp <- 1:nrow(eig_table3)
eig_table3$cond <- eig_table3$`cumulative percentage of variance` < 80
eig_table3$cond[1:(which(eig_table3$cond==TRUE)[length(which(eig_table3$cond==TRUE))] + 1)] <- 1
eig_table3$cond[(which(eig_table3$cond==TRUE)[length(which(eig_table3$cond==TRUE))] + 2):length(eig_table3$cond)] <- 0

eig_table6 <- pca_g6$eig
eig_table6$Cluster <- 'Cluster 6'
eig_table6$princ_comp <- 1:nrow(eig_table6)
eig_table6$cond <- eig_table6$`cumulative percentage of variance` < 80
eig_table6$cond[1:(which(eig_table6$cond==TRUE)[length(which(eig_table6$cond==TRUE))] + 1)] <- 1
eig_table6$cond[(which(eig_table6$cond==TRUE)[length(which(eig_table6$cond==TRUE))] + 2):length(eig_table6$cond)] <- 0

eig_table <- rbind(eig_table1, eig_table2, eig_table3, eig_table6)
rm(eig_table1, eig_table2, eig_table3, eig_table6)

p <- ggplot(eig_table, aes(x=factor(princ_comp, sort(unique(princ_comp))), y=`cumulative percentage of variance`, fill=factor(cond))) + geom_bar(stat="identity")
p <- p + xlab('Componente principal') + ylab('Porcentaje de varianza acumulado (%)') +  geom_hline(yintercept=80)
p <- p + theme_minimal() + theme_bw()
p <- p + facet_wrap(~Cluster, ncol=2)
p <- p + theme(legend.position="none")
p <- p + theme(axis.text.x = element_text(size=6))
ggsave(filename='D:/ToBackup/AEPS-Big_data/Convenio_MADR/ASBAMA/Informes/pca_cluster.png', plot=p, width=7, height=6, units='in', dpi=300)


library(dplyr)
library(tidyr)
library(ggplot2)

# ========================================== #
# Escalar datos
# ========================================== #

median_conditions <- apply(cluster_coord2[,-c(1:3)], MARGIN=2, median)

# median_scale <- function(x){z <- (x - median(x, na.rm=TRUE))/mad(x, constant=1, na.rm=TRUE); return(z)}
# mean_scale <- function(x){z <- (x- mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE); return(z)}
# cluster_coord2_scale <- cluster_coord2 %>% group_by(cluster) %>% mutate_each_(funs(median_scale),vars=c("bio_1","bio_2","bio_3","bio_4","bio_5","bio_6","bio_7","bio_8","bio_9","bio_10","bio_11",
#                                                                          "bio_12","bio_13","bio_14","bio_15","bio_16","bio_17","bio_18","bio_19",
#                                                                          "bio_20",
#                                                                          "bulk_density","cec","clay_percent","coarse_fragments","organic_carbon","pH","sand_percent","silt_percent"))
# cluster_coord2_scale <- as.data.frame(cluster_coord2_scale)
# cluster_coord2_scale[,4:ncol(cluster_coord2_scale)] <- round(cluster_coord2_scale[,4:ncol(cluster_coord2_scale)], 2)

# ========================================== #
# Calcular la mediana por cluster
# ========================================== #

median_values <- cluster_coord2 %>% group_by(cluster) %>% summarise_each(funs(median))
median_values <- as.data.frame(median_values)

median_changes <- lapply(1:length(median_conditions), function(i)
{
  median_values2 <- median_values[,-c(1:3)]
  z <- (median_conditions[i] - median_values2[,i])/median_conditions[i]
  z <- as.data.frame(z); colnames(z) <- names(median_values2)[i]
  return(z)
}) 
median_changes <- Reduce(function(...) cbind(..., deparse.level=1), median_changes)
median_changes$Cluster <- factor(IDclusters)

median_values2 <- median_changes %>% gather(variable, median, -Cluster)

median_data <- median_values2
rm(median_values, median_values2, median_dev, median_dev2)

median_data <- median_data[-which(median_data$variable=='bio_20'),]
median_data$variable <- factor(median_data$variable, unique(as.character(median_data$variable)))
median_data$Cluster <- factor(median_data$Cluster, sort(unique(as.character(median_data$Cluster))))

median_data$var_group <- NA
median_data$var_group[grep2(pattern=paste('bio_',1:11,'$',sep=''), x=median_data$variable)] <- 'Temperatura'
median_data$var_group[grep2(pattern=paste('bio_',12:19,'$',sep=''), x=median_data$variable)] <- 'Precipitación'
median_data$var_group[setdiff(1:length(median_data$variable), grep2(pattern=paste('bio_',1:19,'$',sep=''), x=median_data$variable))] <- 'Suelos'

median_data <- median_data[-which(median_data$variable=='cec'|median_data$variable=='clay_percent'|median_data$variable=='coarse_fragments'|median_data$variable=='pH'|median_data$variable=='sand_percent'|median_data$variable=='silt_percent'),]
median_data$variable <- factor(median_data$variable, unique(as.character(median_data$variable)))
median_data$Cluster <- factor(median_data$Cluster, sort(unique(as.character(median_data$Cluster))))

p <- ggplot(median_data, aes(fill=Cluster, y=-median*100, x=variable))
p <- p + geom_bar(position="dodge", stat="identity")
dodge <- position_dodge(width=0.9)
p <- p + geom_bar(position=dodge) # + geom_errorbar(limits, position=dodge, width=0.25)
p <- p + xlab('Variable') + ylab('Porcentaje de cambio respecto a la mediana general (%)')
p <- p + facet_wrap(~var_group, ncol=1, scales = "free")
ggsave(filename='D:/ToBackup/AEPS-Big_data/Convenio_MADR/ASBAMA/Informes/descripcion_variables.png', plot=p, width=7, height=8.5, units='in', dpi=300)

median_data_s3 <- median_data[-which(median_data$Cluster==3),]

p <- ggplot(median_data_s3, aes(fill=Cluster, y=-median*100, x=variable))
p <- p + geom_bar(position="dodge", stat="identity")
dodge <- position_dodge(width=0.9)
p <- p + geom_bar(position=dodge) # + geom_errorbar(limits, position=dodge, width=0.25)
p <- p + xlab('Variable') + ylab('Porcentaje de cambio respecto a la mediana general (%)')
p <- p + facet_wrap(~var_group, ncol=1, scales = "free")
ggsave(filename='D:/ToBackup/AEPS-Big_data/Convenio_MADR/ASBAMA/Informes/descripcion_variables_s3.png', plot=p, width=7, height=8.5, units='in', dpi=300)


p <- ggplot(cluster_coord2, aes(x=cluster, y=bio_20))
p <- p + geom_boxplot()
p <- p + xlab('Cluster') + ylab('Altitud (MSNM)')
p <- p + theme_minimal() + theme_bw()
ggsave(filename='D:/ToBackup/AEPS-Big_data/Convenio_MADR/ASBAMA/Informes/boxplot_altitud_cluster.png', plot=p, width=6, height=6, units='in', dpi=300)
