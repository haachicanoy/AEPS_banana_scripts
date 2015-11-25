# Modifications of funcionesClimaticas.R
# H. Achicanoy
# CIAT, 2015

# -------------------------------------------------------- #
# UNIFICACION DE DATOS EN SOLO ARCHIVO
# -------------------------------------------------------- #

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


# -------------------------------------------------------- #
# CREAR INDICADOR
# -------------------------------------------------------- #

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


# -------------------------------------------------------- #
# GENERADOR DE INDICADOR
# -------------------------------------------------------- #

climIndicatorsGenerator <- function(climVar,namFun,Fase,periodcul,diasFase,cosechBase,namFecha,climBase)
{
    
  rnames <- row.names(cosechBase)
  percDias <- diasFase/periodcul
  diasCult <- as.numeric(cosechBase[,namFecha[2]]-cosechBase[,namFecha[1]])
  
  diaAcum <- round(do.call(rbind,lapply(diasCult,function(x){x*cumsum(percDias)})),0)[,-length(diasFase)]
  
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