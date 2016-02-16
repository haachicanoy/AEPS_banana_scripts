# CONVERTIR TABLA A SHAPEFILE by Fabio Castro
# class(final.filtered)
# names(final.filtered)
# 
# coordinates(final.filtered) <- ~LONGITUDE+LATITUDE
# class(final.filtered)
# plot(final.filtered, pch=19)
# 
# writeOGR(final.filtered, dsn = "F:/Proyectos_2015/Indonesia/Data/_shp/_prec_fires", layer= "prec_fires_enero", driver = "ESRI Shapefile")

# ----------------------------------------------------------------------------------------------------------------- #
# Load packages
# ----------------------------------------------------------------------------------------------------------------- #

options(warn=-1)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# ----------------------------------------------------------------------------------------------------------------- #
# Set work directory
# ----------------------------------------------------------------------------------------------------------------- #

dirFol <- '/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA'
# dirFol <- '//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA'
# dirFol <- 'Z:/'
setwd(dirFol)

# ----------------------------------------------------------------------------------------------------------------- #
# Harvest data
# ----------------------------------------------------------------------------------------------------------------- #

tempDF <- read.csv('./DATOS_PROCESADOS/_cosecha/samaria_cosechas.csv')
tempDF$Id_Lote <- as.character(tempDF$Id_Lote)
tempDF_finca <- tempDF[which(nchar(tempDF$Id_Lote)==4),]; rownames(tempDF_finca) <- 1:nrow(tempDF_finca)
tempDF_lote <- tempDF[which(nchar(tempDF$Id_Lote)!=4),]; rownames(tempDF_lote) <- 1:nrow(tempDF_lote)
rm(tempDF)

write.csv(tempDF_lote, './DATOS_PROCESADOS/_cosecha/samaria_cosechas_lote.csv', row.names=FALSE)
write.csv(tempDF_finca, './DATOS_PROCESADOS/_cosecha/samaria_cosechas_finca.csv', row.names=FALSE)

# ----------------------------------------------------------------------------------------------------------------- #
# Plot harvest data
# ----------------------------------------------------------------------------------------------------------------- #

g <- gc(); rm(list=ls())

options(warn=-1)
library(readxl)
library(raster)
library(rgdal)
library(maptools)
library(sp)

dirFol <- '/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA'
# dirFol <- '//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA'
# dirFol <- 'Z:/'
setwd(dirFol)

cosechas <- read.csv('./DATOS_PROCESADOS/_cosecha/samaria_cosechas_lote.csv')
grep2 <- Vectorize(FUN=grep, vectorize.args='pattern')

# Create a dummy date for cosechas database
for(i in 1:nrow(cosechas))
{
  if(nchar(cosechas$Week[i])==1){
    cosechas$Week[i] <- paste('0', cosechas$Week[i], sep='')
  }
}; rm(i)
cosechas$Date <- paste(cosechas$Year, cosechas$Week, sep='-')
x <- seq(from=as.Date("2000-01-01", format='%Y-%m-%d'), to=as.Date("2015-12-31", format='%Y-%m-%d'), by='day')
x <- x[match(cosechas$Date, format(x, "%Y-%U"))]
cosechas$Date <- x
cosechas <- cosechas[which(!is.na(cosechas$Date)),]
rm(x)

colnames(cosechas)[which(colnames(cosechas)=='Date')] <- 'fechaCosecha'
cosechas$fechaSiembra <- cosechas$fechaCosecha - 267

write.csv(cosechas, './DATOS_PROCESADOS/_cosecha/samaria_cosechas_lote.csv', row.names=FALSE)

# ----------------------------------------------------------------------------------------------------------------- #
# Farm harvest data
# ----------------------------------------------------------------------------------------------------------------- #

g <- gc(); rm(list=ls())

options(warn=-1)
library(readxl)
library(raster)
library(rgdal)
library(maptools)
library(sp)

dirFol <- '/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA'
# dirFol <- '//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA'
# dirFol <- 'Z:/'
setwd(dirFol)

cosechas <- read.csv('./DATOS_PROCESADOS/_cosecha/samaria_cosechas_finca.csv')
grep2 <- Vectorize(FUN=grep, vectorize.args='pattern')

# Create a dummy date for cosechas database
for(i in 1:nrow(cosechas))
{
  if(nchar(cosechas$Week[i])==1){
    cosechas$Week[i] <- paste('0', cosechas$Week[i], sep='')
  }
}; rm(i)
cosechas$Date <- paste(cosechas$Year, cosechas$Week, sep='-')
x <- seq(from=as.Date("2000-01-01", format='%Y-%m-%d'), to=as.Date("2015-12-31", format='%Y-%m-%d'), by='day')
x <- x[match(cosechas$Date, format(x, "%Y-%U"))]
cosechas$Date <- x
cosechas <- cosechas[which(!is.na(cosechas$Date)),]
rm(x)

colnames(cosechas)[which(colnames(cosechas)=='Date')] <- 'fechaCosecha'
cosechas$fechaSiembra <- cosechas$fechaCosecha - 267

write.csv(cosechas, './DATOS_PROCESADOS/_cosecha/samaria_cosechas_finca.csv', row.names=FALSE)
