# Create climate indicators (modification)
# H. Achicanoy
# CIAT, 2015

# ----------------------------------------------------------------------------------------------------------------- #
# Load packages
# ----------------------------------------------------------------------------------------------------------------- #

g <- gc(); rm(list=ls())
options(warn=-1)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# ----------------------------------------------------------------------------------------------------------------- #
# Set work directory
# ----------------------------------------------------------------------------------------------------------------- #

dirFol  <- "/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA"
# dirFol <- "//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA"
dirFol <- 'Z:/'
wkDir <- paste(dirFol); setwd(wkDir)

# ----------------------------------------------------------------------------------------------------------------- #
# Read database (change according necesities)
# ----------------------------------------------------------------------------------------------------------------- #

# baseManejo <- read.csv('./DATOS_PROCESADOS/_cosecha/_cobana/cobana_siembras.csv')
baseManejo <- read_excel('./DATOS_PROCESADOS/_merma/all_merma.xlsx', sheet=1)
baseManejo <- as.data.frame(baseManejo)
names(baseManejo) <- c('IDLote', 'Year', 'Week', 'Merma', 'Date')
baseManejo$Date <- as.Date(baseManejo$Date, format='%Y-%m-%d')
names(baseManejo)[length(names(baseManejo))] <- 'fechaCosecha'
baseManejo$fechaSiembra <- as.Date(baseManejo$fechaCosecha-267)

baseManejoGua <- baseManejo[grep(pattern='^6T', x=baseManejo$IDLote),]; rownames(baseManejoGua) <- 1:nrow(baseManejoGua)
baseManejoMag <- baseManejo[setdiff(1:nrow(baseManejo), grep(pattern='^6T', x=baseManejo$IDLote)),]; rownames(baseManejoMag) <- 1:nrow(baseManejoMag)

# ----------------------------------------------------------------------------------------------------------------- #
# Load climate functions
# ----------------------------------------------------------------------------------------------------------------- #

source('./RESULTADOS/Modelling/_scripts/climFunctions.R')
# load('D:/ToBackup/AEPS-Big_data/Flagship_arroz_Peru/arroz_jaen/Analisis/_scritps/All-Functions-AEPS_BD.RData')

# ----------------------------------------------------------------------------------------------------------------- #
# Set climate directory
# ----------------------------------------------------------------------------------------------------------------- #

climDir1 <- paste(dirFol,'/DATOS_PROCESADOS/_clima/IDEAM/Climate_to', sep='') # /DATOS_PROCESADOS/_clima/Tecbaco/Climate_to
climDir2 <- paste(dirFol,'/DATOS_PROCESADOS/_clima/Tecbaco/Climate_to', sep='') # /DATOS_PROCESADOS/_clima/Tecbaco/Climate_to

# ----------------------------------------------------------------------------------------------------------------- #
# Read original climate files
# ----------------------------------------------------------------------------------------------------------------- #

climFiles <- list.files(path=climDir1, pattern='.txt$', full.names=TRUE)
climOrder <- gsub(pattern='.txt$', replacement='', x=list.files(path=climDir1, pattern='.txt$', full.names=FALSE))

baseClima1 <- unifDatos(climFiles=climFiles, namesC=climOrder)
baseClima1$FECHA <- as.Date(baseClima1$FECHA, format='%Y-%m-%d')

climFiles <- list.files(path=climDir2, pattern='.txt$', full.names=TRUE)
climOrder <- gsub(pattern='.txt$', replacement='', x=list.files(path=climDir2, pattern='.txt$', full.names=FALSE))

baseClima2 <- unifDatos(climFiles=climFiles, namesC=climOrder)
baseClima2$FECHA <- as.Date(baseClima2$FECHA, format='%Y-%m-%d')

rm(climFiles, climOrder, climDir1, climDir2)

# ----------------------------------------------------------------------------------------------------------------- #
# Create climate indicators by phenological crop phase
# ----------------------------------------------------------------------------------------------------------------- #

# Define climate indicator
climVar <- c("mean(TMAX)","mean(TMIN)","mean((TMAX+TMIN)/2)","mean(TMAX-TMIN)","sum(TMAX>34)/length(TMAX)","sum(TMIN<=20)/length(TMIN)",
             "sum(RAIN)","sum(RAIN>=10)/length(RAIN)","sum(RAIN<=7)/length(RAIN)","mean(RHUM)","sum(ESOL)")

# Define indicator's name
namFun  <- c("TX_avg","TM_avg","T_avg","Diurnal_Range_avg","TX_freq_34","TM_freq_20",
             "P_accu","P_10_freq","P_inf7_freq","RH_avg", "SR_accu")

periodBase     <- 267                              # Duracion total ciclo productivo      Default values: periodBase  <- 120
FaseCultivo    <- c("DIFF","FLOW","DEVL")   # Nombre corto por etapas de cultivo   Default values: FaseCultivo <- c("VEG","REP","LLEN")
diasPorFase    <- c(154, 35, 78)               # Dias de cada etapa                   Default values: diasPorFase <- <- c(48,41,31)
namFec         <- c("fechaSiembra","fechaCosecha") # Nombres de la fecha de siembra       Default values: namFec <- c("fecha_siembra","fecha_cosecha")

a1 <- climIndicatorsGenerator(climVar=climVar, namFun=namFun, Fase=FaseCultivo,
                              periodcul=periodBase, diasFase=diasPorFase, cosechBase=baseManejoMag,
                              namFecha=namFec, climBase=baseClima1, onePhase=FALSE)
a2 <- climIndicatorsGenerator(climVar=climVar, namFun=namFun, Fase=FaseCultivo,
                              periodcul=periodBase, diasFase=diasPorFase, cosechBase=baseManejoGua,
                              namFecha=namFec, climBase=baseClima2, onePhase=FALSE)

# ----------------------------------------------------------------------------------------------------------------- #
# Save results
# ----------------------------------------------------------------------------------------------------------------- #

wkDir <- paste(dirFol,'/DATOS_PROCESADOS/_merma/', sep=''); setwd(wkDir)

a1 <- data.frame(baseManejoMag, a1)
a2 <- data.frame(baseManejoGua, a2)

a <- rbind(a1, a2)
a <- a[complete.cases(a),]

write.csv(a, "all_merma_clima_cycle.csv", row.names=FALSE)
