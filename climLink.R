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
# dirFol  <- "//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA"
wkDir <- paste(dirFol); setwd(wkDir)

# ----------------------------------------------------------------------------------------------------------------- #
# Read database (change according necesities)
# ----------------------------------------------------------------------------------------------------------------- #

baseManejo <- read.csv('./DATOS_PROCESADOS/_cosecha/cobana_cosechas_fertilizaciones.csv')
# baseManejo <- read_excel('./DATOS_PROCESADOS/Cobana_data.xlsx', sheet='Cosechas')
baseManejo <- as.data.frame(baseManejo)
baseManejo$ID <- paste(baseManejo$IDFinca,'-',baseManejo$Year,'-',baseManejo$Week, sep='')
baseManejo$Date <- as.Date(baseManejo$Date, '%Y-%m-%d')
baseManejo <- baseManejo[,c(43,42,22:41,20)]

# ----------------------------------------------------------------------------------------------------------------- #
# Load climate functions
# ----------------------------------------------------------------------------------------------------------------- #

source('./RESULTADOS/Identificacion_factores_limitantes/_scripts/climFunctions.R')
# load('D:/ToBackup/AEPS-Big_data/Flagship_arroz_Peru/arroz_jaen/Analisis/_scritps/All-Functions-AEPS_BD.RData')

# ----------------------------------------------------------------------------------------------------------------- #
# Set climate directory
# ----------------------------------------------------------------------------------------------------------------- #

climDir <- paste(dirFol,'/DATOS_PROCESADOS/_clima/IDEAM/Climate_to', sep='')
setwd(climDir)

# ----------------------------------------------------------------------------------------------------------------- #
# Read original climate files
# ----------------------------------------------------------------------------------------------------------------- #

climFiles <- list.files(path=getwd(), pattern='.txt$', full.names=TRUE)
climOrder <- gsub(pattern='.txt$', replacement='', x=list.files(path=getwd(), pattern='.txt$', full.names=FALSE))

baseClima <- unifDatos(climFiles=climFiles, namesC=climOrder)
baseClima$FECHA <- as.Date(baseClima$FECHA, format='%Y-%m-%d')

# ----------------------------------------------------------------------------------------------------------------- #
# Fix date format
# ----------------------------------------------------------------------------------------------------------------- #

x <- seq(from=as.Date("2000-01-01", format='%Y-%m-%d'), to=as.Date("2015-12-31", format='%Y-%m-%d'), by='day')
x <- x[match(paste(baseManejo$Year, '-', baseManejo$Week, sep=''), format(x, "%Y-%U"))]

baseManejo$fechaCosecha <- x; rm(x)
baseManejo <- baseManejo[which(!is.na(baseManejo$fechaCosecha)),]
baseManejo$fechaSiembra <- as.Date(baseManejo$fechaCosecha-267)
rownames(baseManejo) <- 1:nrow(baseManejo)

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

a <- climIndicatorsGenerator(climVar=climVar, namFun=namFun, Fase=FaseCultivo,
                             periodcul=periodBase, diasFase=diasPorFase, cosechBase=baseManejo,
                             namFecha=namFec, climBase=baseClima, onePhase=TRUE)

# ----------------------------------------------------------------------------------------------------------------- #
# Save results
# ----------------------------------------------------------------------------------------------------------------- #

wkDir <- paste(dirFol,'/DATOS_PROCESADOS/_cosecha', sep=''); setwd(wkDir)
a <- data.frame(baseManejo,a)
a <- a[,c(1:23,26:36,24)]
write.csv(a,"cobana_cosechas_fertilizaciones_clima_cycle.csv", row.names=FALSE)
