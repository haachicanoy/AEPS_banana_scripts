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
setwd(dirFol)

# ----------------------------------------------------------------------------------------------------------------- #
# Sowings data
# ----------------------------------------------------------------------------------------------------------------- #

siembras <- read_excel(path='./DATOS_PROCESADOS/Cobana_data.xlsx', sheet='Siembra')

diffID <- siembras %>% select(IDFinca, Year_establish, Month_establish, Week_establish)
diffID <- unique(diffID)

farm_sowings <- lapply(1:nrow(diffID), function(i)
{
  df <- siembras[which(siembras$IDFinca==diffID$IDFinca[i] &
                         siembras$Year_establish==diffID$Year_establish[i] &
                         siembras$Month_establish==diffID$Month_establish[i] &
                         siembras$Week_establish==diffID$Week_establish[i]),]
  sowings <- nrow(df)
  planting_system <- sort(as.character(unique(df$Planting_system))); if(length(planting_system)>1){planting_system <- paste(planting_system, collapse='_')} else {cat('Only one category exists\n')}
  seed_type       <- sort(as.character(unique(df$Seed_type))); if(length(seed_type)>1){seed_type <- paste(seed_type, collapse='_')} else {cat('Only one category exists\n')}
  variety         <- sort(as.character(unique(df$Variety))); if(length(variety)>1){variety <- paste(variety, collapse='_')} else {cat('Only one category exists\n')}
  seeds_acum      <- sum(as.numeric(df$Number_seeds), na.rm=TRUE)
  df <- data.frame(IDFinca=diffID$IDFinca[i],
                   Year_establish=diffID$Year_establish[i],
                   Month_establish=diffID$Month_establish[i],
                   Week_establish=diffID$Week_establish[i],
                   sowings=sowings,
                   planting_system=planting_system,
                   seed_type=seed_type,
                   variety=variety,
                   seeds_acum=seeds_acum)
  return(df)
})
farm_sowings <- Reduce(function(...) rbind(..., deparse.level=1), farm_sowings)
write.csv(farm_sowings, './DATOS_PROCESADOS/_siembras/cobana_siembras.csv', row.names=FALSE)
rm(farm_sowings, siembras)

# ----------------------------------------------------------------------------------------------------------------- #
# Fertilization data
# ----------------------------------------------------------------------------------------------------------------- #

fertilizacion <- read_excel(path='./DATOS_PROCESADOS/Cobana_data.xlsx', sheet='Fertilizacion')

diffID <- fertilizacion %>% select(IDFinca, Year_application, Month_fertilizacion, Week_fertilizacion)
diffID <- unique(diffID); diffID <- diffID[complete.cases(diffID),]

farm_fertilization <- lapply(1:nrow(diffID), function(i)
{
  df <- fertilizacion[which(fertilizacion$IDFinca==diffID$IDFinca[i] &
                              fertilizacion$Year_application==diffID$Year_application[i] &
                              fertilizacion$Month_fertilizacion==diffID$Month_fertilizacion[i] &
                              fertilizacion$Week_fertilizacion==diffID$Week_fertilizacion[i]),]
  fertilizaciones      <- nrow(df)
  tipo_abono           <- sort(as.character(unique(df$Tipo_abono))); if(length(tipo_abono)>1){tipo_abono <- paste(tipo_abono, collapse='_')} else {cat('Only one category exists\n')}
  tipo_aplicacion_fert <- sort(as.character(unique(df$Tipo_aplicacion_fertilizacion))); if(length(tipo_aplicacion_fert)>1){tipo_aplicacion_fert <- paste(tipo_aplicacion_fert, collapse='_')} else {cat('Only one category exists\n')}
  compuestos_acum      <- data.frame(t(apply(X=df[,c("N","P2O5","K2O","CaO","MgO","S","B","Zn","Cu","Si","Cl","K2MgCa2(SO4)4H2O","Gallinaza","Fe","Mn","MO","KCl")], MARGIN=2, FUN=function(x){z <- sum(x, na.rm=TRUE)})))
  df <- data.frame(IDFinca=diffID$IDFinca[i],
                   Year_application=diffID$Year_application[i],
                   Month_fertilizacion=diffID$Month_fertilizacion[i],
                   Week_fertilizacion=diffID$Week_fertilizacion[i],
                   fertilizaciones=fertilizaciones,
                   tipo_abono=tipo_abono,
                   tipo_aplicacion_fert=tipo_aplicacion_fert)
  df <- cbind(df, compuestos_acum)
  return(df)
})
farm_fertilization <- Reduce(function(...) rbind(..., deparse.level=1), farm_fertilization)
write.csv(farm_fertilization, './DATOS_PROCESADOS/_fertilizaciones/cobana_fertilizaciones.csv', row.names=FALSE)
rm(farm_fertilization, fertilizacion)

# ----------------------------------------------------------------------------------------------------------------- #
# Controles data
# ----------------------------------------------------------------------------------------------------------------- #

controles <- read_excel(path='./DATOS_PROCESADOS/Cobana_data.xlsx', sheet='Controles')

diffID <- controles %>% select(IDFinca, Year_control, Month_control, Week_control)
diffID <- unique(diffID); diffID <- diffID[complete.cases(diffID),]

farm_controls <- lapply(1:nrow(diffID), function(i)
{
  df <- controles[which(controles$IDFinca==diffID$IDFinca[i] &
                          controles$Year_control==diffID$Year_control[i] &
                          controles$Month_control==diffID$Month_control[i] &
                          controles$Week_control==diffID$Week_control[i]),]
  controls         <- nrow(df)
  molecula_activa  <- sort(as.character(unique(df$Familia_molecula_activa))); if(length(molecula_activa)>1){molecula_activa <- paste(molecula_activa, collapse='_')} else {cat('Only one category exists\n')}
  dosis_acum       <- sum(as.numeric(df$Dosis_usada_control), na.rm=TRUE)
  duracion_control <- mean(as.numeric(df$Duracion_control), na.rm=TRUE)
  df <- data.frame(IDFinca=diffID$IDFinca[i],
                   Year_control=diffID$Year_control[i],
                   Month_control=diffID$Month_control[i],
                   Week_control=diffID$Week_control[i],
                   controls=controls,
                   molecula_activa=molecula_activa,
                   dosis_acum=dosis_acum,
                   duracion_control=duracion_control)
  return(df)
})
farm_controls <- Reduce(function(...) rbind(..., deparse.level=1), farm_controls)
write.csv(farm_controls, './DATOS_PROCESADOS/_controles/cobana_controles.csv', row.names=FALSE)
rm(farm_controls, controles)

# ----------------------------------------------------------------------------------------------------------------- #
# Monitoreo data
# ----------------------------------------------------------------------------------------------------------------- #

monitoreo <- read_excel(path='./DATOS_PROCESADOS/Cobana_data.xlsx', sheet='Monitoreo')

diffID <- monitoreo %>% select(IDFinca, Year_monitoreo, Month_monitoreo, Week_monitoreo)
diffID <- unique(diffID); diffID <- diffID[complete.cases(diffID),]

farm_monitoreo <- lapply(1:nrow(diffID), function(i)
{
  df <- monitoreo[which(monitoreo$IDFinca==diffID$IDFinca[i] &
                          monitoreo$Year_monitoreo==diffID$Year_monitoreo[i] &
                          monitoreo$Month_monitoreo==diffID$Month_monitoreo[i] &
                          monitoreo$Week_monitoreo==diffID$Week_monitoreo[i]),]
  monitoreos            <- nrow(df)
  total_casos           <- sum(as.numeric(df$Numero_casos_monitoreo), na.rm=TRUE)
  cantidad_tot_aplicada <- sum(as.numeric(df$Cantidad_aplicada_monitoreo), na.rm=TRUE)
  df <- data.frame(IDFinca=diffID$IDFinca[i],
                   Year_monitoreo=diffID$Year_monitoreo[i],
                   Month_monitoreo=diffID$Month_monitoreo[i],
                   Week_monitoreo=diffID$Week_monitoreo[i],
                   monitoreos=monitoreos,
                   total_casos=total_casos,
                   cantidad_tot_aplicada=cantidad_tot_aplicada)
  return(df)
})
farm_monitoreo <- Reduce(function(...) rbind(..., deparse.level=1), farm_monitoreo)
write.csv(farm_monitoreo, './DATOS_PROCESADOS/_monitoreos/cobana_monitoreos.csv', row.names=FALSE)
rm(farm_monitoreo, monitoreo)

# ----------------------------------------------------------------------------------------------------------------- #
# Unir cosechas, monitoreos, controles, fertilizaciones, siembras y análisis de suelo
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

cosechas        <- read_excel('./DATOS_PROCESADOS/Cobana_data.xlsx', sheet='Cosechas')
siembras        <- read.csv('./DATOS_PROCESADOS/_siembras/cobana_siembras.csv'); siembras$Month_establish <- NULL
fertilizaciones <- read.csv('./DATOS_PROCESADOS/_fertilizaciones/cobana_fertilizaciones.csv'); fertilizaciones$Month_fertilizacion <- NULL
# controles       <- read.csv('./DATOS_PROCESADOS/_controles/cobana_controles.csv'); controles$Month_control <- NULL
# monitoreos      <- read.csv('./DATOS_PROCESADOS/_monitoreos/cobana_monitoreos.csv'); monitoreos$Month_monitoreo <- NULL
# suelo           <- read_excel('./DATOS_PROCESADOS/Cobana_data.xlsx', sheet='Suelo')

grep2 <- Vectorize(FUN=grep, vectorize.args='pattern')

names(cosechas)[grep2(pattern=c('Year_cosecha', 'Semana_cosecha'),names(cosechas))] <- c('Year','Week')
names(siembras)[grep2(pattern=c('Year_establish', 'Week_establish'),names(siembras))] <- c('Year','Week')
names(fertilizaciones)[grep2(pattern=c('Year_application', 'Week_fertilizacion'),names(fertilizaciones))] <- c('Year','Week')
# names(controles)[grep2(pattern=c('Year_control', 'Week_control'),names(controles))] <- c('Year','Week')

siembras <- siembras[,c('IDFinca', 'Year', 'Week', 'sowings', 'seed_type', 'variety', 'seeds_acum')]
fertilizaciones <- fertilizaciones[,c('IDFinca', 'Year', 'Week', 'fertilizaciones', 'tipo_abono', 'tipo_aplicacion_fert')]

# Create a dummy date for fertilizaciones database
for(i in 1:nrow(fertilizaciones))
{
  if(nchar(fertilizaciones$Week[i])==1){
    fertilizaciones$Week[i] <- paste('0',fertilizaciones$Week[i],sep='')
  }
}; rm(i)
fertilizaciones$Date <- paste(fertilizaciones$Year, fertilizaciones$Week, sep='-')
x <- seq(from=as.Date("2000-01-01", format='%Y-%m-%d'), to=as.Date("2015-12-31", format='%Y-%m-%d'), by='day')
x <- x[match(fertilizaciones$Date, format(x, "%Y-%U"))]
fertilizaciones$Date <- x
fertilizaciones <- fertilizaciones[which(!is.na(fertilizaciones$Date)),]
rm(x)

# Create a dummy date for siembras database
for(i in 1:nrow(siembras))
{
  if(nchar(siembras$Week[i])==1){
    siembras$Week[i] <- paste('0',siembras$Week[i],sep='')
  }
}; rm(i)
siembras$Date <- paste(siembras$Year, siembras$Week, sep='-')
x <- seq(from=as.Date("2000-01-01", format='%Y-%m-%d'), to=as.Date("2015-12-31", format='%Y-%m-%d'), by='day')
x <- x[match(siembras$Date, format(x, "%Y-%U"))]
siembras$Date <- x
siembras <- siembras[which(!is.na(siembras$Date)),]
rm(x)

# Move to harvest date
# Try with different dates to move
fertilizaciones$Date <- fertilizaciones$Date + 267 # Dates to move [DEV: 78, FLOW: 135, Complete cycle: 267]
fertilizaciones$Week <- as.numeric(format(fertilizaciones$Date+3, "%U"))
fertilizaciones$Year <- as.numeric(format(fertilizaciones$Date,'%Y'))
fertilizaciones <- fertilizaciones[fertilizaciones$Week>0,]

siembras$Date <- siembras$Date + 267 # Dates to move [DEV: 78, FLOW: 135, Complete cycle: 267]
siembras$Week <- as.numeric(format(siembras$Date+3, "%U"))
siembras$Year <- as.numeric(format(siembras$Date,'%Y'))
siembras <- siembras[siembras$Week>0,]

# ----------------------------------------------------------------------------------------------------------------- #
# Make merge between cosechas y controles
# ----------------------------------------------------------------------------------------------------------------- #

# cosechasControles <- base::merge(x=cosechas, y=controles, by=c('IDFinca','Year','Week'), all.x=TRUE)
# 
# # Create a dummy date for cosechas database
# cosechasControles$Date <- paste(cosechasControles$Year, cosechasControles$Week, sep='-')
# x <- seq(from=as.Date("2000-01-01", format='%Y-%m-%d'), to=as.Date("2015-12-31", format='%Y-%m-%d'), by='day')
# x <- x[match(cosechasControles$Date, format(x, "%Y-%U"))]
# cosechasControles$Date <- x; rm(x)
# 
# cosechasControles <- cosechasControles[which(!is.na(cosechasControles$Date)),]
# rownames(cosechasControles) <- 1:nrow(cosechasControles)
# write.csv(cosechasControles, 'cobana_cosechas_controles.csv', row.names=FALSE)

# ----------------------------------------------------------------------------------------------------------------- #
# Make merge between cosechas y fertilizaciones
# ----------------------------------------------------------------------------------------------------------------- #

cosechasFert <- base::merge(x=cosechas, y=fertilizaciones, by=c('IDFinca','Year','Week'), all.x=TRUE)
cosechasFert <- cosechasFert[which(!is.na(cosechasFert$Date)),]
rownames(cosechasFert) <- 1:nrow(cosechasFert)

write.csv(cosechasFert, './DATOS_PROCESADOS/_cosecha/_cobana/cobana_fertilizaciones.csv', row.names=FALSE)

# ----------------------------------------------------------------------------------------------------------------- #
# Make merge between cosechas y siembras
# ----------------------------------------------------------------------------------------------------------------- #

cosechasSiembras <- base::merge(x=cosechas, y=siembras, by=c('IDFinca','Year','Week'), all.x=TRUE)
cosechasSiembras <- cosechasSiembras[which(!is.na(cosechasSiembras$Date)),]
rownames(cosechasSiembras) <- 1:nrow(cosechasSiembras)

write.csv(cosechasSiembras, './DATOS_PROCESADOS/_cosecha/_cobana/cobana_siembras.csv', row.names=FALSE)

# ggplot(data=cosechasSiembras, aes(x=reorder(seed_type, Peso_racimo, FUN=median), y=Peso_racimo, colour=variety)) + geom_jitter(position=position_jitter(0.2), shape=17, size=3)
# ggplot(data=cosechasSiembras, aes(x=seeds_acum, y=Peso_racimo, colour=Finca)) + geom_point(cex=3)

# Create a dummy date for cosechas database
cosechasSiembras$Date <- paste(cosechasSiembras$Year, cosechasSiembras$Week, sep='-')
x <- seq(from=as.Date("2000-01-01", format='%Y-%m-%d'), to=as.Date("2015-12-31", format='%Y-%m-%d'), by='day')
x <- x[match(cosechasSiembras$Date, format(x, "%Y-%U"))]
cosechasSiembras$Date <- x; rm(x)

cosechasSiembras <- cosechasSiembras[which(!is.na(cosechasSiembras$Date)),]
rownames(cosechasSiembras) <- 1:nrow(cosechasSiembras)
write.csv(cosechasSiembras, 'cobana_cosechas_siembras.csv', row.names=FALSE)

# # Make merge between cosechas, controles, fertilizaciones y monitoreos
# cosechasFinal <- base::merge(x=cosechas, y=controles, by=c('IDFinca','Year','Week'), all.x=TRUE)
# cosechasFinal <- base::merge(x=cosechasFinal, y=fertilizaciones, by=c('IDFinca','Year','Week'), all.x=TRUE)
# 
# # Create a dummy date for cosechas database
# cosechasFinal$Date <- paste(cosechasFinal$Year, cosechasFinal$Week, sep='-')
# x <- seq(from=as.Date("2000-01-01", format='%Y-%m-%d'), to=as.Date("2015-12-31", format='%Y-%m-%d'), by='day')
# x <- x[match(cosechasFinal$Date, format(x, "%Y-%U"))]
# cosechasFinal$Date <- x; rm(x)
# 
# cosechasFinal <- cosechasFinal[which(!is.na(cosechasFinal$Date)),]
# rownames(cosechasFinal) <- 1:nrow(cosechasFinal)
# write.csv(cosechasFinal, './DATOS_PROCESADOS/_cosecha/cobana_control_fert_mon2analyse.csv', row.names=FALSE)
