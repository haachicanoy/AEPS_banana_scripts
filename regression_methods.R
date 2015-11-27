# file data-analysis-AEPS-BigData.R
# 
# This file contains a script to develop regressions with machine learning methodologies
#
#
# author: Hugo Andres Dorado 02-16-2015
#  
#This script is free: you can redistribute it and/or modify
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 

#-----------------------------------------------------------------------------------------------------------------


#SCRIPT BUILED FOR R VERSION 3.0.2 
#PACKAGES
options(warn=-1)
if(!require(gtools)){install.packages('gtools');library(gtools)} else{library(gtools)}
if(!require(gridBase)){install.packages('gridBase');library(gridBase)} else{library(gridBase)}
if(!require(relaimpo)){install.packages('relaimpo');library(relaimpo)} else{library(relaimpo)}
if(!require(caret)){install.packages('caret');library(caret)} else{library(caret)}
if(!require(party)){install.packages('party');library(party)} else{library(party)}
if(!require(randomForest)){install.packages('randomForest');library(randomForest)} else{library(randomForest)}
if(!require(snowfall)){install.packages('snowfall');library(snowfall)} else{library(snowfall)}
if(!require(earth)){install.packages('earth');library(earth)} else{library(earth)}
if(!require(reshape)){install.packages('reshape');library(reshape)} else{library(reshape)}
if(!require(agricolae)){install.packages('agricolae');library(agricolae)} else{library(agricolae)}
if(!require(stringr)){install.packages('stringr');library(stringr)} else{library(stringr)}

#Work Directory

dirFol  <- "/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA/DATOS_PROCESADOS/_clima/IDEAM/Climate_to"
# dirFol  <- "//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA/DATOS_PROCESADOS/_clima/IDEAM/Climate_to"
setwd(dirFol)

#DataBase structure

dataSet <- read.csv('indicadoresClimaticosCobana4model.csv')
dataSet <- dataSet[complete.cases(dataSet),]; rownames(dataSet) <- 1:nrow(dataSet)
dataSet$splitVar <- 'All'

inputs  <- 1:44  # inputs columns
segme   <- 46    # split column
output  <- 45    # output column

namsDataSet <- names(dataSet)

#Creating the split factors

contVariety <- table(dataSet[,segme])
variety0    <- names(sort(contVariety[contVariety>=30]))

if(length(variety0)==1){variety = variety0 }else{variety = factor(c(variety0,"All"))}

load('All-Functions-AEPS_BD.RData')

variety <- 'All'

#creating folders
createFolders(dirFol, variety)

#DataSets ProcesosF
dataSetProces(variety, dataSet, segme, corRed="caret")

# LINEAR REGRESSION; only when all inputs are cuantitative;  
# lineaRegresionFun(variety, dirLocation=paste0(getwd(),"/"), ylabs="Yield (Ton/Ha)")

# MULTILAYER PERCEPTRON
multilayerPerceptronFun(variety, dirLocation=paste0(getwd(),"/"), nb.it=30, ylabs="Yield (Ton/Ha)", pertuRelevance=T, ncores=3) # All

# RANDOM FOREST
randomForestFun(variety, nb.it=30, ncores=23) # All

# CONDITIONAL FOREST; especify if you have categorical variables
conditionalForestFun(variety, nb.it=30, ncores=23) # All

# ----------------------------------------------------------------------------------------------------------------------------------------------------- #
# Using only climate
# ----------------------------------------------------------------------------------------------------------------------------------------------------- #

#SCRIPT BUILED FOR R VERSION 3.0.2 
#PACKAGES
options(warn=-1)
if(!require(gtools)){install.packages('gtools');library(gtools)} else{library(gtools)}
if(!require(gridBase)){install.packages('gridBase');library(gridBase)} else{library(gridBase)}
if(!require(relaimpo)){install.packages('relaimpo');library(relaimpo)} else{library(relaimpo)}
if(!require(caret)){install.packages('caret');library(caret)} else{library(caret)}
if(!require(party)){install.packages('party');library(party)} else{library(party)}
if(!require(randomForest)){install.packages('randomForest');library(randomForest)} else{library(randomForest)}
if(!require(snowfall)){install.packages('snowfall');library(snowfall)} else{library(snowfall)}
if(!require(earth)){install.packages('earth');library(earth)} else{library(earth)}

#Work Directory

dirFol  <- "D:/ToBackup/AEPS-Big_data/Flagship_arroz_Peru/arroz_jaen/Analisis/_climate_ts/"
# dirFol  <- "/home/hachicanoy/jaen_analysis/"
setwd(dirFol)

data2 <- read.csv('./only_climate.csv', row.names=1)

#DataBase structure

data2$splitVar <- 'All'
data2 <- data2[,c('variedad',"TX_avg_ALL","TM_avg_ALL","T_avg_ALL","Diurnal_Range_avg_ALL","TX_freq_35_ALL","P_accu_ALL","P_10_freq_ALL","RH_avg_ALL",'splitVar','RDT')]
data2$tipoSiembra <- as.factor(data2$tipoSiembra)
data2$variedad <- as.factor(data2$variedad)
data2$splitVar <- as.factor(data2$splitVar)

inputs  <- 1:10  # inputs columns
segme   <- 10    # split column
output  <- 11    # output column

dataSet   <- data2
namsDataSet <- names(dataSet)

#Creating the split factors

contVariety <- table(dataSet[,segme])
variety0    <- names(sort(contVariety[contVariety>=30]))

if(length(variety0)==1){variety = variety0 }else{variety = factor(c(variety0,"All"))}

#creating folders

# load('/home/hachicanoy/jaen_analysis/All-Functions-AEPS_BD.RData')
load("/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/OPEN_BIGDATA_AEPS/REGRESSION_MODELS/All-Functions-AEPS_BD.RData")
variety <- 'All'

createFolders(dirFol, variety)

#DataSets ProcesosF
dataSetProces(variety=variety, dataSet=dataSet, segme=segme, corRed="caret")

# LINEAR REGRESSION; only when all inputs are cuantitative;  
# lineaRegresionFun(variety, dirLocation=paste0(getwd(),"/"), ylabs="Yield (Ton/Ha)")

# MULTILAYER PERCEPTRON
multilayerPerceptronFun(variety, dirLocation=paste0(getwd(),"/"), nb.it=30, ylabs="Yield (Ton/Ha)", pertuRelevance=T, ncores=3) # All

# RANDOM FOREST
randomForestFun(variety, nb.it=30, ncores=23) # All

# CONDITIONAL FOREST; especify if you have categorical variables
conditionalForestFun(variety, nb.it=30, ncores=23) # All

# ----------------------------------------------------------------------------------------------------------------------------------------------------- #
# Using both climate and soils
# ----------------------------------------------------------------------------------------------------------------------------------------------------- #

#SCRIPT BUILED FOR R VERSION 3.0.2 
#PACKAGES
options(warn=-1)
if(!require(gtools)){install.packages('gtools');library(gtools)} else{library(gtools)}
if(!require(gridBase)){install.packages('gridBase');library(gridBase)} else{library(gridBase)}
if(!require(relaimpo)){install.packages('relaimpo');library(relaimpo)} else{library(relaimpo)}
if(!require(caret)){install.packages('caret');library(caret)} else{library(caret)}
if(!require(party)){install.packages('party');library(party)} else{library(party)}
if(!require(randomForest)){install.packages('randomForest');library(randomForest)} else{library(randomForest)}
if(!require(snowfall)){install.packages('snowfall');library(snowfall)} else{library(snowfall)}
if(!require(earth)){install.packages('earth');library(earth)} else{library(earth)}

#Work Directory

dirFol  <- "D:/ToBackup/AEPS-Big_data/Flagship_arroz_Peru/arroz_jaen/Analisis/_climate_ts/"
# dirFol  <- "/home/hachicanoy/jaen_analysis/"
setwd(dirFol)

data2 <- read.csv('./all_soil_climate.csv', row.names=1)

#DataBase structure

data2$splitVar <- 'All'
data2 <- data2[,c('variedad',"CE","pH","arena","arcilla","limo","matOrganica","P_disponible","K_disponible","CIC","CaCO3","TX_avg_ALL","TM_avg_ALL","T_avg_ALL","Diurnal_Range_avg_ALL","TX_freq_35_ALL","P_accu_ALL","P_10_freq_ALL","RH_avg_ALL",'splitVar','RDT')]
data2$variedad <- as.factor(data2$variedad)
data2$splitVar <- as.factor(data2$splitVar)

inputs  <- 1:19  # inputs columns
segme   <- 20    # split column
output  <- 21    # output column

dataSet   <- data2
namsDataSet <- names(dataSet)

#Creating the split factors

contVariety <- table(dataSet[,segme])
variety0    <- names(sort(contVariety[contVariety>=30]))

if(length(variety0)==1){variety = variety0 }else{variety = factor(c(variety0,"All"))}

#creating folders

# load('/home/hachicanoy/jaen_analysis/All-Functions-AEPS_BD.RData')
load("/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/OPEN_BIGDATA_AEPS/REGRESSION_MODELS/All-Functions-AEPS_BD.RData")
variety <- 'All'

createFolders(dirFol, variety)

#DataSets ProcesosF
dataSetProces(variety=variety, dataSet=dataSet, segme=segme, corRed="caret")

# LINEAR REGRESSION; only when all inputs are cuantitative;  
# lineaRegresionFun(variety, dirLocation=paste0(getwd(),"/"), ylabs="Yield (Ton/Ha)")

# MULTILAYER PERCEPTRON
multilayerPerceptronFun(variety, dirLocation=paste0(getwd(),"/"), nb.it=30, ylabs="Yield (Ton/Ha)", pertuRelevance=T, ncores=3) # All

# RANDOM FOREST
randomForestFun(variety, nb.it=30, ncores=3) # All

# CONDITIONAL FOREST; especify if you have categorical variables
conditionalForestFun(variety, nb.it=30, ncores=23) # All

# ----------------------------------------------------------------------------------------------------------------------------------------------------- #
# Using only soils
# ----------------------------------------------------------------------------------------------------------------------------------------------------- #

#SCRIPT BUILED FOR R VERSION 3.0.2 
#PACKAGES
options(warn=-1)
if(!require(gtools)){install.packages('gtools');library(gtools)} else{library(gtools)}
if(!require(gridBase)){install.packages('gridBase');library(gridBase)} else{library(gridBase)}
if(!require(relaimpo)){install.packages('relaimpo');library(relaimpo)} else{library(relaimpo)}
if(!require(caret)){install.packages('caret');library(caret)} else{library(caret)}
if(!require(party)){install.packages('party');library(party)} else{library(party)}
if(!require(randomForest)){install.packages('randomForest');library(randomForest)} else{library(randomForest)}
if(!require(snowfall)){install.packages('snowfall');library(snowfall)} else{library(snowfall)}
if(!require(earth)){install.packages('earth');library(earth)} else{library(earth)}

#Work Directory

dirFol  <- "D:/ToBackup/AEPS-Big_data/Flagship_arroz_Peru/arroz_jaen/Analisis/_climate_ts/"
# dirFol  <- "/home/hachicanoy/jaen_analysis/"
setwd(dirFol)

data2 <- read.csv('./only_soil.csv')

#DataBase structure

data2$splitVar <- 'All'
data2 <- data2[,c('tipoSiembra','variedad',"CE","pH","arena","arcilla","limo","matOrganica","P_disponible","K_disponible","CIC","CaCO3",'splitVar','RDT')]
data2$tipoSiembra <- as.factor(data2$tipoSiembra)
data2$variedad <- as.factor(data2$variedad)
data2$splitVar <- as.factor(data2$splitVar)

inputs  <- 1:12  # inputs columns
segme   <- 13    # split column
output  <- 14    # output column

dataSet   <- data2
namsDataSet <- names(dataSet)

#Creating the split factors

contVariety <- table(dataSet[,segme])
variety0    <- names(sort(contVariety[contVariety>=30]))

if(length(variety0)==1){variety = variety0 }else{variety = factor(c(variety0,"All"))}

#creating folders

# load('/home/hachicanoy/jaen_analysis/All-Functions-AEPS_BD.RData')
load("/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/OPEN_BIGDATA_AEPS/REGRESSION_MODELS/All-Functions-AEPS_BD.RData")
variety <- 'All'

createFolders(dirFol, variety)

#DataSets ProcesosF
dataSetProces(variety=variety, dataSet=dataSet, segme=segme, corRed="caret")

# LINEAR REGRESSION; only when all inputs are cuantitative;  
# lineaRegresionFun(variety, dirLocation=paste0(getwd(),"/"), ylabs="Yield (Ton/Ha)")

# MULTILAYER PERCEPTRON
multilayerPerceptronFun(variety, dirLocation=paste0(getwd(),"/"), nb.it=30, ylabs="Yield (Ton/Ha)", pertuRelevance=T, ncores=3) # All

# RANDOM FOREST
randomForestFun(variety, nb.it=30, ncores=3) # All

# CONDITIONAL FOREST; especify if you have categorical variables
conditionalForestFun(variety, nb.it=30, ncores=23) # All
