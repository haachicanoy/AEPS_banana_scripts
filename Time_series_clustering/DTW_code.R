library(gtools)
library(ggplot2)
library(dtw)
library(plyr)

rm(list=ls())

First_Processing <- function(dataBase,climeData)
{
  listEvents=list()
  for(j in 1:dim(dataBase)[1]){
    aux=climeData[climeData[[1]] %in% c(dataBase$FECHA_SIEMBRA[j]:dataBase$FECHA_COSECHA[j]),]
    ID=rep(dataBase[j,1],dim(aux)[1])
    listEvents[[j]]=data.frame(ID,aux)
  }
  return(do.call(rbind,listEvents))
}

unifDatos <- function(climFiles,namesC,formFecha="%Y-%m-%d")
{
  listaInfo      <- lapply(climFiles,function(x){read.table(x,header=T)})
  FECHA          <- as.Date(listaInfo[[1]][,1],format = formFecha)
  listProces     <- lapply(listaInfo,function(x){x[,2]})  
  listCompleta   <- do.call(cbind,listProces)
  dfListCompleta <- data.frame(FECHA,as.data.frame(listCompleta))
  names(dfListCompleta) <- c("FECHA",namesC)
  
  return(dfListCompleta)
}  

###############  Librerias y load de als funciones

source("C:/Users/haachicanoy/Documents/GitHub/AEPS_banana_scripts/dtwMultivariado.R", chdir = TRUE)
source("C:/Users/haachicanoy/Documents/GitHub/AEPS_banana_scripts/procesClustering.R")

###############  Lectura de datos y Direccionamiento del workspace

dirFol <- ""
setwd(dirFol)

filenames <- list.files("Serie de Clima", pattern="*.txt", full.names=TRUE)

nomClim<-c("ESOL","RAIN","RHUM","TMAX","TMIN")

climeData=unifDatos(filenames,nomClim)

EventsInfo=read.csv("ToySet_Clustering.csv")
EventsInfo$FECHA_SIEMBRA=as.Date(EventsInfo$FECHA_SIEMBRA,format("%m/%d/%Y"))
EventsInfo$FECHA_COSECHA=as.Date(EventsInfo$FECHA_COSECHA,format("%m/%d/%Y"))

###############  Procesamiento de los datos

ClusterData<-First_Processing(EventsInfo,climeData)

procesData(ClusterData,datVar = "FECHA",dateFormat = "")

load("listClimatEvent.RData")

locality <- "Yopal" 

###############  Cambiando a formato multivariado de tiempo

tsnleventsN <- lapply(evenN,ts)

###############  Creando distancia DTW

distAllMatrix <- distDtwMV(tsnleventsN)

save(distAllMatrix,file = "distMatrixCluster.RData")

load("distMatrixCluster.RData")

###############  Clustering Jerárquico

hClustEvents <- hirarCluster(distAllMatrix)

IdEvent <- names(evenF)

eventsClasi<-data.frame(IdEvent,hClustEvents)
names(eventsClasi)<-c("ID","Cluster")

write.csv(eventsClasi,"eventsClasificated.csv")

############# Graficando los Clusters

plotClusterDTW(hClustEvents,evenF,title = locality )


############# Comparando Variedades En los cluster

library(ggplot2)

AllData<-plyr::join_all(list(EventsInfo,eventsClasi),by="ID")

table(AllData$Cluster)



ggplot(AllData,aes(factor(Cluster),RENDIMIENTO_HA))+geom_boxplot()+
  xlab("Cluster")+ylab("Rendimiento\n")+
  theme(text=element_text(size=18),
        axis.title.y=element_text(size = rel(1.3),colour = "#999999"),
        axis.title.x=element_text(size = rel(1.2),colour = "#888888"),
        axis.text.x  = element_text(angle=0, hjust=0.5),
        panel.background=element_rect(fill="white",colour = "black"),
        panel.grid.major = element_line(colour = "gray"))

DataClust<-AllData[AllData$Cluster=="10",]

ggplot(DataClust,aes(VARIEDAD,RENDIMIENTO_HA))+geom_boxplot()+
  xlab("Variedad")+ylab("Rendimiento\n")+
  theme(text=element_text(size=18),
        axis.title.y=element_text(size = rel(1.3),colour = "#999999"),
        axis.title.x=element_text(size = rel(1.2),colour = "#888888"),
        axis.text.x  = element_text(angle=0, hjust=0.5),
        panel.background=element_rect(fill="white",colour = "black"),
        panel.grid.major = element_line(colour = "gray"))

