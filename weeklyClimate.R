# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Calculate weekly climate
# H. Achicanoy
# CIAT, 2016
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

climDir <- 'Z:/DATOS_PROCESADOS/_clima/IDEAM/Climate_to'
files <- list.files(path=climDir, pattern='*.txt$', full.names=TRUE)
files <- lapply(files, function(x){read.table(x, header=TRUE)})
for(i in 1:length(files))
{
  files[[i]]$Dates <- as.Date(files[[i]]$Dates)
  files[[i]]$Week <- strftime(files[[i]]$Dates, format="%W")
  #files[[i]]$Week[which(files[[i]]$Week=='00')] <- '53'
  files[[i]]$Week <- paste(strftime(files[[i]]$Dates, format="%Y"),'-',files[[i]]$Week,sep='')
}; rm(i)


weeks <- unique(files[[1]]$Week)

weekClim <- lapply(1:length(files), function(i)
{
  weekVal <- lapply(1:length(weeks), function(j)
  {
    if(i==1){ # ESOL
      esol <- sum(files[[i]]$Value[which(files[[i]]$Week==weeks[j])], na.rm=TRUE)
      week <- weeks[j]
      frame = data.frame(week=week, esol=esol)
    } else {
      if(i==2){ # RAIN
        rain <- sum(files[[i]]$Value[which(files[[i]]$Week==weeks[j])], na.rm=TRUE)
        week <- weeks[j]
        frame = data.frame(week=week, rain=rain)
      } else {
        if(i==3){ # RHUM
          rhum <- mean(files[[i]]$Value[which(files[[i]]$Week==weeks[j])], na.rm=TRUE)
          week <- weeks[j]
          frame = data.frame(week=week, rhum=rhum)
        } else {
          if(i==4){ # TMAX
            tmax <- max(files[[i]]$Value[which(files[[i]]$Week==weeks[j])], na.rm=TRUE) # Using max
            #tmax <- mean(files[[i]]$Value[which(files[[i]]$Week==weeks[j])], na.rm=TRUE) # Using mean
            week <- weeks[j]
            frame = data.frame(week=week, tmax=tmax)
          } else {
            if(i==5){ # TMIN
              tmin <- min(files[[i]]$Value[which(files[[i]]$Week==weeks[j])], na.rm=TRUE) # Using min
              #tmin <- mean(files[[i]]$Value[which(files[[i]]$Week==weeks[j])], na.rm=TRUE) # Using mean
              week <- weeks[j]
              frame = data.frame(week=week, tmin=tmin)
            }
          }
        }
      }
    }
    return(frame)
  })
  weekVal = Reduce(function(...){rbind(..., deparse.level=1)}, weekVal)
  return(weekVal)
})

weekClim <- Reduce(function(...) merge(..., all=T), weekClim)
write.csv(weekClim, 'Z:/DATOS_PROCESADOS/_clima/IDEAM/Climate_to/weeklyClimate.csv', row.names=FALSE)

scale(weekClim[,-1])

# system("convert C:/Users/haachicanoy/Documents/Histogram.pdf C:/Users/haachicanoy/Documents/Histogram.png", wait=TRUE)

##################################################
# Compare Yield with climate (use Banasan data)
##################################################

# Weekly climate data
weekClim <- read.csv('Z:/DATOS_PROCESADOS/_clima/IDEAM/Climate_to/weeklyClimate.csv')
weekClim <- data.frame(Date=weekClim[,1], as.data.frame(scale(weekClim[,-1]))) # Scaled climate data
weekClim$Date <- as.character(weekClim$Date)
weekClim$Date[grep(pattern='*-00$', x=weekClim$Date, value=FALSE)] <- paste(as.numeric(unlist(lapply(strsplit(x=weekClim$Date[grep(pattern='*-00$', x=weekClim$Date, value=FALSE)], split='-'), function(x){x[[1]]})))-1, '-53', sep='')
x <- seq(from=as.Date("2000-01-01", format='%Y-%m-%d'), to=as.Date("2015-12-31", format='%Y-%m-%d'), by='day')
x <- x[match(weekClim$Date, format(x, "%Y-%U"))]
weekClim$DateComplete <- x
weekClim <- weekClim[which(!is.na(weekClim$DateComplete)),]
rm(x)

# Weekly harvest data
library(readxl)
harvest <- read_excel('Z:/DATOS_PROCESADOS/Banasan_data_GC.xlsx', sheet='Cosechas')
harvest <- harvest[,c('Finca', 'Year_cosecha', 'Semana_cosecha', 'Peso_racimo')]
for(i in 1:nrow(harvest)){
  if(nchar(harvest$Semana_cosecha[i])==1){
    harvest$Semana_cosecha[i] <- paste('0', harvest$Semana_cosecha[i], sep='')
  } else {
    cat('Nothing happens\n')
  }
}; rm(i)
harvest$Date <- paste(harvest$Year_cosecha, '-', harvest$Semana_cosecha, sep='')
x <- seq(from=as.Date("2000-01-01", format='%Y-%m-%d'), to=as.Date("2015-12-31", format='%Y-%m-%d'), by='day')
x <- x[match(harvest$Date, format(x, "%Y-%U"))]
harvest$DateComplete <- x
harvest <- harvest[which(!is.na(harvest$DateComplete)),]
rm(x)
harvest$Peso_racimo[which(harvest$Peso_racimo==0)] <- NA
harvest <- harvest[complete.cases(harvest),]
rownames(harvest) <- 1:nrow(harvest)

# Plotting Peso del racimo by farm

library(xts)
library(dplyr)
library(tidyr)
library(ggplot2)

plot1 <- ggplot(data=harvest, aes(x=DateComplete, y=Peso_racimo, fill=Finca, colour=Finca)) + geom_line()
plot1 <- plot1 + guides(fill=guide_legend(ncol=2)) + ggtitle('Peso promedio del racimo (kg)/Finca a través del tiempo')
plot1 <- plot1 + xlab('Fecha') + ylab('Peso del racimo (kg)')
plot1
ggsave(filename='C:/Users/haachicanoy/Desktop/peso_racimo_patterns.pdf', plot=plot1, width=14, height=8, units='in', dpi=300)

# convert -verbose -density 300 Fig1.pdf -quality 100 -sharpen 0x1.0 -alpha off Fig1.png # From Julian
# system("convert -verbose -density 300 C:/Users/haachicanoy/Desktop/peso_racimo_patterns.pdf -quality 100 -sharpen 0x1.0 -alpha off C:/Users/haachicanoy/Desktop/peso_racimo_patterns.png")
# system("convert -verbose -density 300 /home/hachicanoy/Histogram.pdf -quality 100 -sharpen 0x1.0 -alpha off /home/hachicanoy/Histogram.png", wait=TRUE)

farms <- sort(unique(as.character(harvest$Finca)))
harvest <- lapply(1:length(farms), function(i)
{
  sub_harvest <- subset(harvest, subset=harvest$Finca==farms[i])
  sub_harvest <- data.frame(Finca=sub_harvest$Finca, DateComplete=sub_harvest$DateComplete, Peso_racimo=scale(sub_harvest$Peso_racimo))
  return(sub_harvest)
})
harvest <- Reduce(function(...){rbind(..., deparse.level=1)}, harvest)
plot1 <- ggplot(data=harvest, aes(x=DateComplete, y=Peso_racimo, fill=Finca, colour=Finca)) + geom_line()
plot1 <- plot1 + guides(fill=guide_legend(ncol=2)) + ggtitle('Peso promedio del racimo (kg)/Finca a través del tiempo')
plot1 <- plot1 + xlab('Fecha') + ylab('Peso del racimo (kg)') # + geom_smooth()
plot1

peso_list <- lapply(1:length(farms), function(i)
{
  cat('\nProcessing:',farms[[i]],'\n\n')
  sub_harvest <- harvest %>% filter(Finca==farms[[i]]) %>% select(DateComplete, Peso_racimo)
  sub_harvest <- xts(sub_harvest$Peso_racimo, as.Date(sub_harvest$DateComplete, format='%Y-%m-%d'))
  return(sub_harvest)
})

clim_list <- lapply(2:(ncol(weekClim)-1), function(i)
{
  wClim <- xts(weekClim[,i], as.Date(weekClim$DateComplete, format='%Y-%m-%d'))
  return(wClim)
})

require(dtw)
library(xts)
library(ggplot2)

# Function to calculate DTW distance
distDtwMV <- function(listObje)
{
  require(dtw)
  len      <-  length(listObje)
  listDist <- matrix(0,nrow=len,ncol=len)
  
  disDtw <- array(0,len)
  # pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)
  pb <- txtProgressBar(min = 0, max = len, style = 3)
  for(i in 1:len){ 
    
    for(j in i:len){
      listDist[j,i]<- dtw(listObje[[i]],listObje[[j]])$distance
    }
    setTxtProgressBar(pb, i)
    #  info <- sprintf("%d%% done", round((i/(len)*100)))
    # setWinProgressBar(pb, i/(len)*100, label=info)
  }
  close(pb)
  #close(pb)
  rownames <- names(listObje)
  colnames <- names(listObje)
  return(as.dist(listDist))
}

# Function to do hierarchical clustering
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
  if(!file.exists("monthlyClimate")){dir.create("monthlyClimate")}else{}
  if(!file.exists("forecast")){dir.create("forecast")}else{}
  
  return(membAll)
}

# Make process for each farm to compare
lappy(1:length(peso_list), function(i)
{
  # Processing
  all_list <- c(peso_list[[i]], clim_list)
  names(all_list) <- c(farms[[i]], 'ESOL', 'RAIN', 'RHUM', 'TMAX', 'TMIN')
  results <- distDtwMV(all_list)
  results_hirarCluster <- hirarCluster(results)
  
  # Do plots
  cluster1 <- all_list[grep(pattern=1, x=results_hirarCluster)]
  cluster2 <- all_list[grep(pattern=2, x=results_hirarCluster)]
  cluster3 <- all_list[grep(pattern=3, x=results_hirarCluster)]
  
  ### Cluster 1
  cluster1 <- lapply(1:length(cluster1), function(i)
  {
    time.df <- data.frame(Date=index(cluster1[[i]]), Peso_racimo=coredata(cluster1[[i]])); time.df$Finca <- names(cluster1)[[i]]
    return(time.df)
  })
  cluster1 <- Reduce(function(...) rbind(..., deparse.level=1), cluster1)
  
  plot1 <- ggplot(cluster1, aes(x=Date, y=Peso_racimo, colour=Finca)) + geom_line()
  plot1 <- plot1 + xlab('Fecha') + ylab('Peso del racimo (kg)') + ggtitle('Cluster 1')
  plot1 <- plot1 + theme_bw() + theme(panel.background = element_blank(),
                                      panel.grid.major.x = element_blank(),
                                      panel.grid.minor.x = element_blank(),
                                      panel.grid.major.y = element_blank(),
                                      panel.grid.minor.y = element_blank(),
                                      title = element_text(face='bold',size=15),
                                      axis.text.x = element_text(size=10),
                                      axis.text.y = element_text(size=10),
                                      axis.title.x = element_text(face="bold",size=12),
                                      axis.title.y = element_text(face="bold",size=12),
                                      legend.title = element_text(face="bold",size=11),
                                      legend.text  = element_text(size=9))
  plot1
  
  ### Cluster 2
  cluster2 <- lapply(1:length(cluster2), function(i)
  {
    time.df <- data.frame(Date=index(cluster2[[i]]), Peso_racimo=coredata(cluster2[[i]])); time.df$Finca <- names(cluster2)[[i]]
    return(time.df)
  })
  cluster2 <- Reduce(function(...) rbind(..., deparse.level=1), cluster2)
  
  plot2 <- ggplot(cluster2, aes(x=Date, y=Peso_racimo, colour=Finca)) + geom_line()
  plot2 <- plot2 + xlab('Fecha') + ylab('Peso del racimo (kg)') + ggtitle('Cluster 2')
  plot2 <- plot2 + theme_bw() + theme(panel.background = element_blank(),
                                      panel.grid.major.x = element_blank(),
                                      panel.grid.minor.x = element_blank(),
                                      panel.grid.major.y = element_blank(),
                                      panel.grid.minor.y = element_blank(),
                                      title = element_text(face='bold',size=15),
                                      axis.text.x = element_text(size=10),
                                      axis.text.y = element_text(size=10),
                                      axis.title.x = element_text(face="bold",size=12),
                                      axis.title.y = element_text(face="bold",size=12),
                                      legend.title = element_text(face="bold",size=11),
                                      legend.text  = element_text(size=9))
  plot2
  
  ### Cluster 3
  cluster3 <- lapply(1:length(cluster3), function(i)
  {
    time.df <- data.frame(Date=index(cluster3[[i]]), Peso_racimo=coredata(cluster3[[i]])); time.df$Finca <- names(cluster3)[[i]]
    return(time.df)
  })
  cluster3 <- Reduce(function(...) rbind(..., deparse.level=1), cluster3)
  
  plot3 <- ggplot(cluster3, aes(x=Date, y=Peso_racimo, colour=Finca)) + geom_line()
  plot3 <- plot3 + xlab('Fecha') + ylab('Peso del racimo (kg)') + ggtitle('Cluster 3')
  plot3 <- plot3 + theme_bw() + theme(panel.background = element_blank(),
                                      panel.grid.major.x = element_blank(),
                                      panel.grid.minor.x = element_blank(),
                                      panel.grid.major.y = element_blank(),
                                      panel.grid.minor.y = element_blank(),
                                      title = element_text(face='bold',size=15),
                                      axis.text.x = element_text(size=10),
                                      axis.text.y = element_text(size=10),
                                      axis.title.x = element_text(face="bold",size=12),
                                      axis.title.y = element_text(face="bold",size=12),
                                      legend.title = element_text(face="bold",size=11),
                                      legend.text  = element_text(size=9))
  plot3
  
  return()
})


