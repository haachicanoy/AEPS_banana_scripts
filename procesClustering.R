
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



joinEventsClim <- function(climStat,cropData,datCS = "DATE", sowDat= "sowDate", harvDat ="harvestDate",formDate = "%d/%m/%Y" )
{
    climStat[,datCS]   <- as.Date(climStat[,datCS],format = formDate)
    cropData[,sowDat]  <- as.Date(cropData[,sowDat],format = formDate)
    cropData[,harvDat] <- as.Date(cropData[,harvDat],format = formDate)
    
    if( min(cropData[,sowDat]) < min(climStat[,datCS]) |  max(cropData[,harvDat]) > max(climStat[,datCS])  )warning("There are events with dates out of station dates")
    
    lotNam <- row.names(cropData)
    
    cropDates <- lapply(1:nrow(cropData),function(x){EVENT <- lotNam[x];DATE <- as.Date(cropData[,sowDat][x]:cropData[,harvDat][x],
                   origin="1970-01-01"); eventDate <- data.frame(EVENT,DATE); merge(eventDate,climStat,by.x = "DATE" , by.y = datCS, all.x 
                                                                                   = T, all.y = F,sort = T) })
    do.call(rbind,cropDates)[,c(2,1,3:ncol(cropDates[[1]]))]
}



procesData <- function(climEvent,idVar = "ID",datVar="Date",dateFormat=""){
  
  require(gtools)
  
  climaEventos0 <-  climEvent
  climaEventos0[,datVar]   <- as.Date(climaEventos0[,datVar],format="")
  
  
  #NORMALIZACION
  
  vars <- c("ESOL","RAIN","RHUM", "TMAX","TMIN")
  
  
  climDataNorm0               <-  as.data.frame(do.call(cbind,lapply(vars,function(x){y <- climaEventos0[x][,1];z<-(y-mean(y))/sd(y);return(z)})))
  colnames(climDataNorm0)     <-  vars
  
  climDataNorm      <- climaEventos0
  climDataNorm[vars] <- climDataNorm0
  
  tempM <- colMeans(climaEventos0[vars])
  sdM   <- apply(climaEventos0[vars],2,sd)
  
  parClim     <- data.frame(rbind(tempM,sdM ))
 
  #
  
  if(sum(is.na(climaEventos0[,datVar])>0)){stop("PROBLEMA DE FORMATO")}else{}
  if(sum(is.na(climDataNorm[,datVar])>0)){stop("PROBLEMA DE FORMATO")}else{}
  
  events <- unique(climaEventos0[,idVar])

  levents <- list(0)
  leventsN <- list(0)
  
  for(i in 1:length(events))
  {
    event        <- subset(climaEventos0,climaEventos0[,idVar]==events[i])
    event$day    <- 1:nrow(event)
    levents[[i]] <- event
    
    eventN        <- subset(climDataNorm,climDataNorm[,idVar]==events[i])
    eventN$day    <- 1:nrow(eventN)
    leventsN[[i]] <- eventN

  }

  names(levents) <- events

  nlevents <- lapply(levents,function(x){x=x[c(idVar,vars,"day")];return(x)})
  nleventsN <- lapply(leventsN,function(x){x=x[c(idVar,vars,"day")];return(x)})
  
  allEvents <- do.call(rbind,nlevents)
  allEventsN <- do.call(rbind,nleventsN)

  sorData <- reshape(allEvents,idvar =idVar,timevar = "day", direction = "wide")
  sorDataN <- reshape(allEventsN,idvar =idVar,timevar = "day", direction = "wide")
  
  namSortData <- names(sorData)[2:ncol(sorData)]
  namSortDataN <- names(sorDataN)[2:ncol(sorData)]
  
  mixNam      <- mixedsort(namSortData)
  mixNamN     <- mixedsort(namSortDataN)

  pos <- 0
  posN <- 0
  
  for(i in 1:length(namSortData)){pos[i] <- which( mixNam[i]==namSortData)+1}
  for(i in 1:length(namSortDataN)){posN[i] <- which( mixNamN[i]==namSortDataN)+1}
  
  sorDataF <- sorData[,pos]
  sorDataFN <- sorDataN[,posN]

  row.names(sorDataF) <- events
  row.names(sorDataFN) <- events
  
  write.csv(sorDataF,"climateToCluster.csv")
  
  evenF <- lapply(nlevents,function(x){x[,-c(1,7)]})
  evenN <- lapply(nleventsN,function(x){x[,-c(1,7)]})
  
  save(evenF,events,evenN,parClim,file = "listClimatEvent.RData")

}


#HIRARCHICAL CLUSTERING

hirarCluster <- function(distMatrix)
{
  hcAll      <- hclust(distMatrix,method="average")
  
  upDate="Y"

 
  while(upDate!="N")
  {
    print("Choose the maximun number of cluster to expand the graphic:")
    maxNumb <- readLines(n = 1)
    
    barplot(sort(hcAll$height,decreasing=T)[1:maxNumb],names.arg=1:maxNumb,col="lightseagreen",main="Select the number of Cluster",xlab="Number of cluster",ylab="height") 
    abline(h=0)
    
    print("Do you want update the barplot? Y/N")
    upDate = readLines(n = 1)
  }

  tiff("barplotGraph.tiff",width = 740,height = 400)   
  barplot(sort(hcAll$height,decreasing=T)[1:maxNumb],names.arg=1:maxNumb,col="lightseagreen",main="Select the number of Cluster",xlab="Number of cluster",ylab="height") 
  abline(h=0)  
  dev.off()
  
  print("Number of cluster:")
  
  nClust <- readLines(n = 1)
  
  membAll <- cutree(hcAll,k=nClust)
  
  if(!file.exists("AllCluster")){dir.create("AllCluster")}else{}

  return(membAll)
}


plotClusterDTW <- function(membAll,nlevents,title="")
{  
  #STABLISHING PARAMETERS XLIM AND YLIM
  
  minVal  <- apply(do.call(rbind,lapply(nlevents,function(x){apply(x,2,min)})),2,min)
  maxVal  <- apply(do.call(rbind,lapply(nlevents,function(x){apply(x,2,max)})),2,max)
  
      
  limts <- as.data.frame(rbind(minVal,maxVal))
  
 # maxDurac <- max(durac)
  
  for(i in 1:length(unique(membAll))){
    
    #tiff(paste0("AllCluster/cluster_",i,".tiff"),width =608, height = 229, pointsize = 15)
    tiff(paste0("AllCluster/cluster_",i,".tiff"),width = 4, height = 10,res=300,units = 'in')
    
    layout(cbind(1:5))
    par(oma = c(0, 0, 3, 0))
    
    subBas <- nlevents[which(membAll==i)]
    durac   <- do.call(c,lapply(subBas,function(x){nrow(x)}))
    maxDurac <- max(durac)
    
    plot(1:durac[1],subBas[[1]]$TMAX,ylim=limts$TMAX,col=0,ylab="Celsius degrees",xlab="day",main="TMAX",cex.axis=1.2,cex.lab=1.2)
    lapply(subBas,function(x){lines(1:nrow(x),x$TMAX,col="gray")})
    subBas1 <- lapply(subBas, function(x){vw <- as.matrix(x) ; mat <- matrix(NA,maxDurac,5); mat[1:nrow(vw),] <- vw; mat <- as.data.frame(mat); names(mat) <- names(x);mat } )
    lines(1:maxDurac,apply(do.call(cbind,lapply(subBas1,function(x){x$TMAX})),1,mean,na.rm=T),col="red",lwd=2,cex.axis=1.2,cex.lab=1.2,ylab="Cenlsius degrees")
    
    plot(1:durac[1],subBas[[1]]$TMIN,ylim=limts$TMIN,col=0,ylab="Celsius degrees",xlab="day",main="TMIN",cex.axis=1.2,cex.lab=1.2)
    lapply(subBas,function(x){lines(1:nrow(x),x$TMIN,col="gray")})
    subBas1 <- lapply(subBas, function(x){vw <- as.matrix(x) ; mat <- matrix(NA,maxDurac,5); mat[1:nrow(vw),] <- vw; mat <- as.data.frame(mat); names(mat) <- names(x);mat } )
    lines(1:maxDurac,apply(do.call(cbind,lapply(subBas1,function(x){x$TMIN})),1,mean,na.rm=T),col="red",lwd=2,cex.axis=1.2,cex.lab=1.2,ylab="Cenlsius degrees")
    
    
    plot(1:durac[1],subBas[[1]]$RAIN,ylim=limts$RAIN,col=0,ylab="mm",xlab="day",main="RAIN",cex.axis=1.2,cex.lab=1.2)
    lapply(subBas,function(x){lines(1:nrow(x),x$RAIN,col="gray")})
    subBas1 <- lapply(subBas, function(x){vw <- as.matrix(x) ; mat <- matrix(NA,maxDurac,5); mat[1:nrow(vw),] <- vw; mat <- as.data.frame(mat); names(mat) <- names(x);mat } )
    lines(1:maxDurac,apply(do.call(cbind,lapply(subBas1,function(x){x$RAIN})),1,mean,na.rm=T),col="red",lwd=2,cex.axis=1.2,cex.lab=1.2,ylab="mm")

    plot(1:durac[1],subBas[[1]]$RHUM,ylim=limts$RHUM,col=0,ylab="Percent",xlab="day",main="RHUM",cex.axis=1.2,cex.lab=1.2)
    lapply(subBas,function(x){lines(1:nrow(x),x$RHUM,col="gray")})
    subBas1 <- lapply(subBas, function(x){vw <- as.matrix(x) ; mat <- matrix(NA,maxDurac,5); mat[1:nrow(vw),] <- vw; mat <- as.data.frame(mat); names(mat) <- names(x);mat } )
    lines(1:maxDurac,apply(do.call(cbind,lapply(subBas1,function(x){x$RHUM})),1,mean,na.rm=T),col="red",lwd=2,cex.axis=1.2,cex.lab=1.2,ylab="Porcentaje")
    
    plot(1:durac[1],subBas[[1]]$ESOL,ylim=limts$ESOL,col=0,ylab="Cal/cm2",xlab="day",main="ESOL",cex.axis=1.2,cex.lab=1.2)
    lapply(subBas,function(x){lines(1:nrow(x),x$ESOL,col="gray")})
    subBas1 <- lapply(subBas, function(x){vw <- as.matrix(x) ; mat <- matrix(NA,maxDurac,5); mat[1:nrow(vw),] <- vw; mat <- as.data.frame(mat); names(mat) <- names(x);mat } )
    lines(1:maxDurac,apply(do.call(cbind,lapply(subBas1,function(x){x$ESOL})),1,mean,na.rm=T),col="red",lwd=2,cex.axis=1.2,cex.lab=1.2,ylab="Cal/cm2")

    mtext(paste0(title," (",format(Sys.Date(), "%b-%d-%Y"),")","\nCluster ",i),outer=T,font=2)

    
    dev.off()
  }
  
}


monthlyProces <- function(membAll,dailyEvents,title,forecast= NULL)
{
  #BECOME THE DAILY EVENTS TO MONTHLY
  
  
  monthlyClim <- function(x)
  { 
    attach(x)    
    rhumM <- c(mean(RHUM[1:30]),mean(RHUM[31:60]),mean(RHUM[61:90]),mean(RHUM[91:121])) 
    esolM <- c(sum(ESOL[1:30]),sum(ESOL[31:60]),sum(ESOL[61:90]),sum(ESOL[91:121]))
    tmaxM <- c(mean(TMAX[1:30]),mean(TMAX[31:60]),mean(TMAX[61:90]),mean(TMAX[91:121]))
    tminM <- c(mean(TMIN[1:30]),mean(TMIN[31:60]),mean(TMIN[61:90]),mean(TMIN[91:121]))
    rainM <- c(sum(RAIN[1:30]),sum(RAIN[31:60]),sum(RAIN[61:90]),sum(RAIN[91:121]))
    detach(x)
    climSummary <- cbind(tmaxM,tminM,rainM,rhumM,esolM)
    return(climSummary)
  }
  
  listMonEvents <- lapply(lapply(dailyEvents,monthlyClim),as.data.frame)
  
  
  #GRAFICOS POR MES
  
  minVal <- apply(do.call(rbind,lapply(listMonEvents,function(x){apply(x,2,min)})),2,min)
  maxVal <- apply(do.call(rbind,lapply(listMonEvents,function(x){apply(x,2,max)})),2,max)
  
  
  #ESTANDARIZACION
  
  meanValM <- apply(do.call(rbind,listMonEvents),2,mean)
  sdValM   <- apply(do.call(rbind,listMonEvents),2,sd)
  
  limts <- as.data.frame(rbind(minVal,maxVal))
  
  for(i in 1:length(unique(membAll))){
    #width =608, height = 229, pointsize = 15
    tiff(paste0("monthlyClimate/cluster_",i,".tiff"),width = 3, height = 10,res=300,units = 'in')
    
    
    layout(cbind(1:5))
    par(oma = c(0, 0, 3, 0))
    
    subBas <- listMonEvents[which(membAll==i)]
    
    
    plot(1:4,listMonEvents[[1]]$tmaxM,ylim=limts$tmaxM,col=0,ylab="Celsius degrees",xlab="Month",main="Tmax",cex.axis=1.2,cex.lab=1.2)
    lapply(subBas,function(x){lines(1:4,x$tmaxM,col="gray")})
    lines(1:4,apply(do.call(cbind,lapply(subBas,function(x){x$tmaxM})),1,mean),col="red",lwd=2)
    
    plot(1:4,subBas[[1]]$tminM,ylim=limts$tminM,col=0,ylab="Celsius degrees",xlab="Month",main="Tmin",cex.axis=1.2,cex.lab=1.2)
    lapply(subBas,function(x){lines(1:4,x$tminM,col="gray")})
    lines(1:4,apply(do.call(cbind,lapply(subBas,function(x){x$tminM})),1,mean),col="red",lwd=2)
    
    plot(1:4,subBas[[1]]$rainM,ylim=limts$rainM,col=0,ylab="mm",xlab="Month",main="Rain",cex.axis=1.2,cex.lab=1.2)
    lapply(subBas,function(x){lines(1:4,x$rainM,col="gray")})
    lines(1:4,apply(do.call(cbind,lapply(subBas,function(x){x$rainM})),1,mean),col="red",lwd=2)

    plot(1:4,listMonEvents[[1]]$rhumM,ylim=limts$rhumM,col=0,ylab="Percent",xlab="Month",main="Rhum",cex.axis=1.2,cex.lab=1.2)
    lapply(subBas,function(x){lines(1:4,x$rhumM,col="gray")})
    lines(1:4,apply(do.call(cbind,lapply(subBas,function(x){x$rhumM})),1,mean),col="red",lwd=2)
    
    plot(1:4,listMonEvents[[1]]$esolM,ylim=limts$esolM,col=0,ylab="Cal/cm2",xlab="Month",main="Esol",cex.axis=1.2,cex.lab=1.2)
    lapply(subBas,function(x){lines(1:4,x$esolM,col="gray")})
    lines(1:4,apply(do.call(cbind,lapply(subBas,function(x){x$esolM})),1,mean),col="red",lwd=2)
    
    mtext(paste0(title," (",format(Sys.Date(), "%b-%d-%Y"),")","\nCluster ",i),outer=T,font=2)
    
    dev.off()
    
    
  }
    
    if(!is.null(forecast))
    {
     for(i in 1:length(unique(membAll))){ 
      
       subBas <- listMonEvents[which(membAll==i)]
       
       for(j in 1:length(unique(forecast$escenario)))
       {
        tiff(paste0("forecast/cluster_",i,"Esce ",j,".tiff"),width = 8, height = 3,res=300,units = 'in')  
        layout(rbind(1:5))
        par(oma = c(0, 0, 3, 0))
    
        subEsc <- subset(forecast,escenario==j,c("TMAX","TMIN","RAIN"))
    
        plot(1:4,listMonEvents[[1]]$rhumM,ylim=limts[,1],col=0,ylab="Celsius degrees",xlab="Month",main="Tmax",cex.axis=1.2,cex.lab=1.2)
        lapply(subBas,function(x){lines(1:4,x$rhumM,col="gray")})
        lines(1:4,subEsc[,1],col="red",lwd=2)
        
        plot(1:4,listMonEvents[[1]]$esolM,ylim=limts[,1],col=0,ylab="Celsius degrees",xlab="Month",main="Tmax",cex.axis=1.2,cex.lab=1.2)
        lapply(subBas,function(x){lines(1:4,x$esolM,col="gray")})
        lines(1:4,subEsc[,1],col="red",lwd=2)        
        
        plot(1:4,listMonEvents[[1]]$tmaxM,ylim=limts[,1],col=0,ylab="Celsius degrees",xlab="Month",main="Tmax",cex.axis=1.2,cex.lab=1.2)
        lapply(subBas,function(x){lines(1:4,x$tmaxM,col="gray")})
        lines(1:4,subEsc[,1],col="red",lwd=2)
    
        plot(1:4,subBas[[1]]$tminM,ylim=limts[,2],col=0,ylab="Celsius degrees",xlab="Month",main="Tmin",cex.axis=1.2,cex.lab=1.2)
        lapply(subBas,function(x){lines(1:4,x$tminM,col="gray")})
        lines(1:4,subEsc[,2],col="red",lwd=2)
    
        plot(1:4,subBas[[1]]$rainM,ylim=limts[,3],col=0,ylab="mm",xlab="Month",main="Rain",cex.axis=1.2,cex.lab=1.2)
        lapply(subBas,function(x){lines(1:4,x$rainM,col="gray")})
        lines(1:4,subEsc[,3],col="red",lwd=2)
    
        mtext(paste0(title," (",format(Sys.Date(), "%b-%d-%Y"),")","\nPronostico Cluster ",i,"\nEscenario ",j),outer=T,font=2)
    
        dev.off() 
       }       
     }
     
     for(i in 1:length(unique(forecast$escenario)))
     {   
      tiff(paste0("forecast/Escenario_",i,".tiff"),width = 8, height = 3,res=300,units = 'in')  
      layout(rbind(1:3))
      par(oma = c(0, 0, 3, 0))
     
      subEsc <- subset(forecast,escenario==i,c("TMAX","TMIN","RAIN"))
     
      plot(1:4,subEsc[,1],ylim=limts[,1],col="blue",type="l",ylab="Celsius degrees",xlab="Month",main="Tmax",cex.axis=1.2,cex.lab=1.2)
     
      plot(1:4,subEsc[,2],ylim=limts[,2],col="blue",type="l",ylab="Celsius degrees",xlab="Month",main="Tmin",cex.axis=1.2,cex.lab=1.2)

      plot(1:4,subEsc[,3],ylim=limts[,3],col="blue",type="l",ylab="mm",xlab="Month",main="Rain",cex.axis=1.2,cex.lab=1.2)

     
      mtext(paste0(title," (",format(Sys.Date(), "%b-%d-%Y"),")","Escenario ",i),outer=T,font=2)
     
      dev.off() 
     } 
    }else{}
  
  
  nListMonEvents <- list()
  
  for(i in 1:length(listMonEvents))
  {
    nListMonEvents[[i]] <- (listMonEvents[[i]]-matrix(meanValM,4,3,byrow=T))*(1/(matrix(sdValM,4,3,byrow=T)))
  }
  
  monthListNts <- lapply(nListMonEvents,ts)
  
  save(monthListNts,listMonEvents,meanValM,sdValM,file = "listClimatEventMonthly.RData") 
}


#PRONOSTIC

pronosticProces <- function(forecast,monthListNts,meanValM,sdValM,kNeigh,membAll)
{  
  require(dtw)
  
  forecast1    <- forecast[c("TMAX","TMIN","RAIN")]
  
  normForecas1 <- as.data.frame((forecast1-matrix(meanValM,nrow(forecast1),ncol(forecast1),byrow=T))*(1/(matrix(sdValM,nrow(forecast1),ncol(forecast1),byrow=T))))
  
  normForecas1$escenario <- forecast$escenario
  
  lForecast <- list()
  
  for(i in 1:max(forecast[,1]))
  {
    lForecast[[i]] <- subset(normForecas1,escenario==i,select=c("TMAX","TMIN","RAIN"))
    
  }
  
  clusterForecast <- list(0)
  for(i in 1:length(unique(forecast$escenario)))
  {
    k         <- kNeigh
    newTs     <- lForecast[[i]]
    dist       <- 0
    for(j in 1:length(monthListNts)){ dist[j] <- dtw(newTs,monthListNts[[j]])$dist}
    s <- sort(as.vector(dist), index.return=TRUE)
    clusterForecast[[i]] <-table(membAll[s$ix[1:k]])
    print(paste("nearest neighbor scenario: ",i))
    print(table(membAll[s$ix[1:k]]))
  }
  return(clusterForecast)
  
  
}

