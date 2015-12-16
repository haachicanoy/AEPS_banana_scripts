# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Edaphoclimatic classification Magdalena & La Guajira
# H. Achicanoy & G. Calberto
# CIAT, 2015
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

options(warn=-1)
library(raster)
library(rgdal)
library(maptools)
library(sp)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Setting temporary directory
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

setwd('/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA')

# rasterOptions(tmpdir="D:/rtemp") # local
rasterOptions(tmpdir="./RESULTADOS/Clasificacion_edafoclimatica/_maps/rtemp")

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# 1. Load climate rasters
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# climFiles <- list.dirs(path='D:/ToBackup/AEPS-Big_data/Maps/Colombia_worldclim_crop', recursive=TRUE) # local
climFiles <- paste('./RESULTADOS/Clasificacion_edafoclimatica/_maps/Magdalena_Guajira/_worldclim_crop/bio_',1:20, sep='')
climRasters <- raster::stack(climFiles)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# 2. Load soil rasters
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

grep2 <- function(pattern, x){grep(pattern, x)}
grep2 <- Vectorize(FUN=grep2, vectorize.args='pattern')

# soilFiles <- list.dirs(path='D:/ToBackup/AEPS-Big_data/Maps/Colombia_soils_center_north/_extract', recursive=TRUE) # local
soilFiles <- list.dirs(path='./RESULTADOS/Clasificacion_edafoclimatica/_maps/Magdalena_Guajira/_soils_crop', recursive=TRUE)
soilFiles <- soilFiles[grep2(pattern='*_sd[0-9]$', soilFiles)]

depth1Files <- soilFiles[grep(pattern='*_sd1', soilFiles)]; depth1Rasters <- raster::stack(depth1Files)
depth2Files <- soilFiles[grep(pattern='*_sd2', soilFiles)]; depth2Rasters <- raster::stack(depth2Files)
depth3Files <- soilFiles[grep(pattern='*_sd3', soilFiles)]; depth3Rasters <- raster::stack(depth3Files)
depth4Files <- soilFiles[grep(pattern='*_sd4', soilFiles)]; depth4Rasters <- raster::stack(depth4Files)

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
  # kNNdistplot(rTemplate, k=28+1)
  # distances <- kNNdist(rTemplate, k=28+1) # Para las cuatro profundidades se presenta el mismo patrón de comportamiento por ende sólo se require de la
  #                                           estimación de un parámetro
  dbResults <- dbscan::dbscan(rTemplate, eps=100, minPts=28+1) # Identify eps value, k value is data's dimensionality + 1
  # save(dbResults, file='/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA/RESULTADOS/Clasificacion_edafoclimatica/DBSCAN_colombia.RData') # Problems with destiny folder
  save(dbResults, file=paste('./RESULTADOS/Clasificacion_edafoclimatica/_maps/Magdalena_Guajira/_clusters/DBSCAN100-1_magua_',depths[comb],'.RData',sep=''))
  
  # Reconstruct deparment's raster with cluster categories
  
  template <- allCombinations[[comb]][[1]]
  template[] <- NA
  template[as.numeric(rownames(rTemplate))] <- dbResults$cluster
  plot(template)
  
  writeRaster(template, filename=paste('./RESULTADOS/Clasificacion_edafoclimatica/_maps/Magdalena_Guajira/_clusters/DBSCAN100-1_clusters_magua_',depths[comb],'.tif',sep=''), format="GTiff", overwrite=TRUE)
  
  # Delete all temporary files created during process
  removeTmpFiles(h=0)
  
  return(cat('Done!\n'))
  
})

