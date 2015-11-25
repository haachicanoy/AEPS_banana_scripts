# Create climate indicators (modification)
# H. Achicanoy
# CIAT, 2015

# Load packages

library(dplyr)
library(tidyr)
library(ggplot2)

# Set work directory

setwd('/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA')
# setwd('//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA')

# Read database

baseManejo <- read.csv('./DATOS_PROCESADOS/_cosecha/cosechas_suelo_foliarTest.csv')

# Load climate functions

source('./RESULTADOS/Identificacion_factores_limitantes/_scripts/climFunctions.R')
# load('D:/ToBackup/AEPS-Big_data/Flagship_arroz_Peru/arroz_jaen/Analisis/_scritps/All-Functions-AEPS_BD.RData')

# Set climate directory

climDir <- './DATOS_PROCESADOS/_clima/IDEAM/Climate_to'
setwd(climDir)

# Read original climate files

climFiles <- list.files(path=getwd(), full.names=TRUE)
climOrder <- gsub(pattern='.txt', replacement='', x=list.files(path=getwd(), full.names=FALSE))

baseClima <- unifDatos(climFiles=climFiles, namesC=climOrder)
baseClima$FECHA <- as.Date(baseClima$FECHA, format='%Y-%m-%d')

# Fix date format

baseManejo$fechaCosecha <- as.Date(baseManejo$fechaCosecha, format='%Y-%m-%d')
baseManejo$fechaSiembra <- as.Date(baseManejo$fechaCosecha-321)

baseManejo <- baseManejo[which(!as.vector(is.na(baseManejo$fechaCosecha))),]
rownames(baseManejo) <- 1:nrow(baseManejo)

#CREAR INDICADORES CLIMATICOS POR FASE DEL CULTIVO

# Indicar calculo de indicador climatico
climVar <- c("mean(TMAX)","mean(TMIN)","mean((TMAX+TMIN)/2)","mean(TMAX-TMIN)","sum(TMAX >=35)/length(TMAX)",
             "sum(RAIN)","sum(RAIN>=10)/length(RAIN)","mean(RHUM)","sum(ESOL)")

# Indicar nombre de funcion
namFun  <- c("TX_avg","TM_avg","T_avg","Diurnal_Range_avg",
             "TX_freq_35","P_accu","P_10_freq","RH_avg", "SR_accu")

periodBase     <- 120 # Duracion total ciclo productivo                               Default values: periodBase  <- 120
FaseCultivo    <- c("ALL") # Nombre corto por etapas de cultivo                       Default values: FaseCultivo <- c("VEG","REP","LLEN")
diasPorFase    <- c(120) # Dias de cada etapa                                         Default values: diasPorFase <- <- c(48,41,31)
namFec         <- c("fechaSiembra","fechaCosecha") # Nombres de la fecha de siembra   Default values: namFec      <- c("fecha_siembra","fecha_cosecha")

# Calculo de indicadores climaticos

calculoIndicador <- function(climDB,stageIni,stageEnd,climVar)
{
  nEvents <- 1:length(stageIni)
  clIndFin <- data.frame({
    do.call(rbind,{
      lapply(nEvents,
             function(x){
               climEvent <- climDB[climDB[,1] %in% as.Date(stageIni[x]:stageEnd[x], 
                                                           origin="1970-01-01"),]
               return(with(climEvent,{climInd <- unlist(lapply(climVar,
                                                               function(y){
                                                                 eval(parse(text=y))
                                                               }
               ));climInd})
               )
             } 
      )}
    )
  })
  names(clIndFin) <- namFun
  clIndFin
}

climIndicatorsGenerator <- function(climVar,namFun,Fase,periodcul,diasFase,cosechBase,namFecha,climBase)
{
  
  rnames <- row.names(cosechBase)
  percDias <- diasFase/periodcul
  diasCult <- as.numeric(cosechBase[,namFecha[2]]-cosechBase[,namFecha[1]])
  
  diaAcum <- round(do.call(rbind,lapply(diasCult,function(x){x*cumsum(percDias)})),0)#[,-length(diasFase)]
  
  cropDates <- do.call(data.frame,
                       lapply(1:ncol(diaAcum),function(x){
                         as.Date(cosechBase[,namFecha[1]]+diaAcum[,x], origin="1899-12-30")
                       }
                       )
  )
  
  names(cropDates) <- paste("Stage",1:ncol(cropDates),sep="_")
  
  cosechBase <- data.frame(cosechBase,cropDates) 
  
  stageDatesNam <- c(namFecha[1],names(cropDates),namFecha[2])
  
  stageDates <- with(cosechBase,cosechBase[stageDatesNam])
  
  totStg <- length(Fase)
  
  stageInd <- 1:totStg
  data.frame(cropDates,
             do.call(data.frame,
                     lapply(stageInd,function(i){
                       if(i==totStg)
                       {
                         stage1 <- stageDates[,i]
                         stage2 <- stageDates[,i+1]   
                         cInd <- calculoIndicador(climBase,stage1,stage2,climVar)
                         names(cInd) <- paste(names(cInd),Fase[i],sep="_")
                         
                       }else{
                         stage1 <- stageDates[,i]
                         stage2 <- stageDates[,i+1]  -1                    
                         cInd <- calculoIndicador(climBase,stage1,stage2,climVar)
                         names(cInd) <- paste(names(cInd),Fase[i],sep="_")
                       }
                       return(cInd)
                     }
                     )
             )
  )
}

a <- climIndicatorsGenerator(climVar=climVar, namFun=namFun, Fase=FaseCultivo,
                             periodcul=periodBase, diasFase=diasPorFase, cosechBase=baseManejo,
                             namFecha=namFec, climBase=baseClima)

# ESCRIBIR LOS DATOS
write.csv(data.frame(baseManejo,a),"indicadoresClimaticos.csv")
