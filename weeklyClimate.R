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

weekVar <- lapply(1:length(files), function(i)
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

weekVar <- Reduce(function(...) merge(..., all=T), weekVar)
write.csv(weekVar, 'Z:/DATOS_PROCESADOS/_clima/IDEAM/Climate_to/weeklyClimate.csv', row.names=FALSE)

scale(weekVar[,-1])

# system("convert C:/Users/haachicanoy/Documents/Histogram.pdf C:/Users/haachicanoy/Documents/Histogram.png", wait=TRUE)


dataSet$fechaCosecha <- as.Date(dataSet$fechaCosecha)
plot(dataSet$fechaCosecha, dataSet$Peso_racimo, type='l')
