# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Calculate modal soil and foliar data for each farm
# H. Achicanoy & G. Calberto
# CIAT, 2015
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

options(warn=-1)
library(readxl)
library(raster)
library(rgdal)
library(maptools)
library(sp)

setwd('/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA')
# setwd('//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA')

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# LOAD Harvest data
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

harvest_data <- read_excel('./DATOS_PROCESADOS/Banasan_data.xlsx', sheet='Cosechas')

# VERIFY RECORDS

# harvest_data_mermas_neg <- harvest_data[harvest_data$Merma<0,]
# write.csv(harvest_data_mermas_neg, '/home/hachicanoy/mermas_negativas.csv', row.names=F)
# harvest_data_recobros_1 <- harvest_data[harvest_data$Recobro>=1,]
# write.csv(harvest_data_recobros_1, './RESULTADOS/Identificacion_factores_limitantes/_scripts/recobros_mayores1.csv', row.names=F)

# PLOT harvest data: racimos cosechados/ha

library(ggplot2)
for(i in 1:length(harvest_data$Semana_cosecha))
{
  if(nchar(harvest_data$Semana_cosecha[i])==1){
    harvest_data$Semana_cosecha[i] <- paste('0',harvest_data$Semana_cosecha[i],sep='')
  } else {
    cat('Date is ok\n')
  }
}; rm(i)

harvest_data$harvest_date <- paste(harvest_data$Year_cosecha, harvest_data$Semana_cosecha, sep='-')
harvest_data$harvest_date <- as.Date(paste(harvest_data$harvest_date,1),"%Y-%U %u")

harvest_data$Semana_cosecha <- as.numeric(harvest_data$Semana_cosecha)

# Racimos cosechados por hectarea, todas las fincas
plot <- ggplot(harvest_data, aes(x=harvest_date, y=Racimos_cosechar/Area_finca, fill=Finca, colour=Finca)) + geom_line()
plot <- plot + xlab('Semana de cosecha') + ylab('Racimos cosechados/ha')
ggsave(plot, filename='./DATOS_PROCESADOS/racimos_cosechados_all.png', width=10, height=8, units='in', dpi=300)

# Racimos cosechados por hectarea variando año de cosecha por finca
fincas <- sort(as.character(unique(harvest_data$Finca)))
lapply(1:length(fincas), function(i)
{
  sub_data <- harvest_data[harvest_data$Finca==fincas[i],]; rownames(sub_data) <- 1:nrow(sub_data)
  plot1 <- ggplot(sub_data, aes(x=Semana_cosecha, y=Racimos_cosechar/Area_finca, fill=as.factor(Year_cosecha), colour=as.factor(Year_cosecha))) + geom_line()
  plot1 <- plot1 + xlab('Semana de cosecha') + ylab('Racimos cosechados/ha')
  plot1 <- plot1 + labs(color='Año cosecha')
  ggsave(plot1, filename=paste('./DATOS_PROCESADOS/racimos_cosechados_',fincas[i],'_by_year.png'), width=8, height=8, units='in', dpi=300)
})

# Racimos cosechados por hectarea variando finca por año de cosecha
years <- sort(unique(harvest_data$Year_cosecha))
lapply(1:length(years), function(i)
{
  sub_data <- harvest_data[harvest_data$Year_cosecha==years[i],]; rownames(sub_data) <- 1:nrow(sub_data)
  plot2 <- ggplot(sub_data, aes(x=harvest_date, y=Racimos_cosechar/Area_finca, fill=Finca, colour=Finca)) + geom_line()
  plot2 <- plot2 + xlab('Semana de cosecha') + ylab('Racimos cosechados/ha')
  plot2 <- plot2 + labs(color='Finca')
  ggsave(plot2, filename=paste('./DATOS_PROCESADOS/racimos_cosechados_',years[i],'_by_finca.png'), width=8, height=8, units='in', dpi=300)
})

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# LOAD Soil data
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

soil_data <- read_excel('./DATOS_PROCESADOS/Banasan_data.xlsx', sheet='Suelo')
summary(soil_data)

varNames <- names(soil_data)[7:length(names(soil_data))]
varNames <- varNames[-which(varNames=='Textura')]

# PLOT Distribution's variable
# Distributions of each variable by Farm and Year
lapply(1:length(varNames), function(i)
{
  library(ggplot2)
  eval(parse(text=paste('p <- ggplot(data=soil_data, aes(x=',varNames[i],')) + geom_density(colour="darkgreen", fill="darkgreen")',sep='')))
  p <- p + facet_grid(Año_muestreo~Id_Finca)
  p <- p + theme_minimal() + theme_bw()
  p <- p + theme(axis.text.x = element_text(size=10),
                 axis.text.y = element_text(size=10),
                 axis.title.x = element_text(face="bold",size=11),
                 axis.title.y = element_text(face="bold",size=11),
                 legend.title = element_text(face="bold",size=11),
                 legend.text  = element_text(size=10))
  p <- p + ylab('Densidad')
  ggsave(filename=paste('./RESULTADOS/Identificacion_factores_limitantes/Soils_analysis/_farm_year_distributions/',varNames[i],'.png', sep=''), plot=p, width=8, height=8, units='in', dpi=300)
  return(cat('Done!\n'))
})

g = gc(); rm(list=ls())

# CALCULATE Lote raw
soil_data$Id_Lote_raw <- substr(soil_data$Id_Lote, start=5, stop=nchar(soil_data$Id_Lote))
soil_data <- soil_data[,c("Id_Lote", "Id_Finca", "Id_Lote_raw", 
                          "Año_muestreo", "Mes_muestreo", "Dia.semana_muestreo", "Codigo_muestra",
                          "Arena_perc", "Limo_perc", "Arcilla_perc", "Textura", "pH",
                          "Acidez_intercambiable_meq.100g", "Soil_MO_perc", "Soil_P_ppm",
                          "Soil_S_ppm", "Soil_K_meq.100g", "Soil_Ca_meq.100g", "Soil_Mg_meq.100g",
                          "Soil_Na_meq.100g", "Soil_Fe_meq.100g", "Soil_Mn_meq.100g", "Soil_Cu_meq.100g",
                          "Soil_Zn_meq.100g", "Soil_B_meq.100g", "Soil_Perc_sat.K", "Soil_Perc_sat.Ca",
                          "Soil_Perc_sat.Mg", "Soil_Perc_sat.Na", "Soil_Perc_sat.Al")]
loteID <- strsplit(x=soil_data$Id_Lote_raw, split='[[:punct:]]')
freq <- unlist(lapply(1:length(loteID), function(i){z <- length(loteID[[i]]); return(z)}))
soil_data$Freq <- freq; rm(freq)
soil_data <- soil_data[rep(row.names(soil_data), soil_data$Freq), 1:(ncol(soil_data)-1)]
soil_data$Id_Lote_raw <- unlist(loteID); rm(loteID)
rownames(soil_data) <- 1:nrow(soil_data)
write.csv(soil_data, './RESULTADOS/Identificacion_factores_limitantes/Soils_analysis/soil_data_by_plot.csv', row.names=F)
soil_data <- read.csv('./RESULTADOS/Identificacion_factores_limitantes/Soils_analysis/soil_data_by_plot.csv')

varNames <- names(soil_data)[8:length(names(soil_data))]
varNames <- varNames[-which(varNames=='Textura')]

fincasList <- sort(as.character(unique(soil_data$Id_Finca)))

# PLOT Distribution's variable
# Distributions of each variable by Farm and Plot
lapply(1:length(varNames), function(i)
{
  lapply(1:length(fincasList), function(j)
  {
    library(ggplot2)
    var.values <- as.vector(na.omit(soil_data[soil_data$Id_Finca==fincasList[j],varNames[i]]))
    if(length(var.values)>0)
    {
      eval(parse(text=paste('p <- ggplot(data=soil_data[soil_data$Id_Finca=="',fincasList[j],'",], aes(x=Id_Lote_raw, y=',varNames[i],', color=as.factor(Año_muestreo), shape=as.factor(Año_muestreo))) + geom_jitter(position=position_jitter(0.2))',sep='')))
      p <- p + theme_minimal() + theme_bw()
      p <- p + theme(axis.text.x = element_text(size=10),
                     axis.text.y = element_text(size=10),
                     axis.title.x = element_text(face="bold",size=11),
                     axis.title.y = element_text(face="bold",size=11),
                     legend.title = element_text(face="bold",size=11),
                     legend.text  = element_text(size=10))
      p <- p + labs(color='Year') + labs(shape='Year')
      ggsave(filename=paste('./RESULTADOS/Identificacion_factores_limitantes/Soils_analysis/_plot_farm_year_distributions/',varNames[i],'_',fincasList[j],'.png', sep=''), plot=p, width=10, height=8, units='in', dpi=300)
    } else {
      cat(varNames[i],'does not have values for',fincasList[j],'year\n')
    }
  })
  return(cat('Done!\n'))
})

g = gc(); rm(list=ls())

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# CALCULATE Modal soil for hydric balance
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

soil_data <- read.csv('./RESULTADOS/Identificacion_factores_limitantes/Soils_analysis/soil_data_by_plot.csv')

# Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux <- as.vector(na.omit(ux))
  ux[which.max(tabulate(match(x, ux)))]
}

library(ks)
# Calculate modal soil by year
years <- unique(soil_data$Año_muestreo)
mode_by_year <- lapply(1:length(years), function(i)
{
  sub_soil <- subset(soil_data, subset=soil_data$Año_muestreo==years[i]); rownames(sub_soil) <- 1:nrow(sub_soil)
  mode_calculated <- apply(sub_soil, 2, Mode)
  mode_calculated <- as.data.frame(t(mode_calculated))
  mode_calculated <- mode_calculated[,c('Año_muestreo','Arena_perc','Limo_perc','Arcilla_perc','Textura','pH',
                                        'Acidez_intercambiable_meq.100g','Soil_MO_perc','Soil_P_ppm','Soil_S_ppm',
                                        'Soil_K_meq.100g','Soil_Ca_meq.100g','Soil_Mg_meq.100g','Soil_Na_meq.100g',
                                        'Soil_Fe_meq.100g','Soil_Mn_meq.100g','Soil_Cu_meq.100g','Soil_Zn_meq.100g','Soil_B_meq.100g',
                                        'Soil_Perc_sat.K','Soil_Perc_sat.Ca','Soil_Perc_sat.Mg','Soil_Perc_sat.Na','Soil_Perc_sat.Al')]
  
  # Modal texture
  texture <- as.matrix(sub_soil[,c('Arena_perc','Limo_perc')])
  tryCatch(expr={
    kernel.dens <- kde(texture)
  },
  error=function(e){
    cat("Kernel density process failed:",years[i],"\n")
    return("Done\n")
  })
  if(exists('kernel.dens')){
    # kernel.dens <- kde(texture)
    kernelMatrix <- kernel.dens$estimate
    rownames(kernelMatrix) <- as.character(kernel.dens$eval.points[[1]])
    colnames(kernelMatrix) <- as.character(kernel.dens$eval.points[[2]])
    values <- which(kernelMatrix==max(kernelMatrix), arr.ind=TRUE)
    values <- as.numeric(c(rownames(kernelMatrix)[values[1]], colnames(kernelMatrix)[values[2]]))
    values[3] <- 100-sum(values)
    mode_calculated[,c('Arena_perc','Limo_perc','Arcilla_perc')] <- as.factor(values)
    mode_calculated <- mode_calculated[,c('Año_muestreo','Arena_perc','Limo_perc','Arcilla_perc','Textura','pH',
                                          'Acidez_intercambiable_meq.100g','Soil_MO_perc','Soil_P_ppm','Soil_S_ppm',
                                          'Soil_K_meq.100g','Soil_Ca_meq.100g','Soil_Mg_meq.100g','Soil_Na_meq.100g',
                                          'Soil_Fe_meq.100g','Soil_Mn_meq.100g','Soil_Cu_meq.100g','Soil_Zn_meq.100g','Soil_B_meq.100g',
                                          'Soil_Perc_sat.K','Soil_Perc_sat.Ca','Soil_Perc_sat.Mg','Soil_Perc_sat.Na','Soil_Perc_sat.Al')]
    return(mode_calculated)
  } else {
    cat('Missing values exist\n')
  }
  return(mode_calculated)
})
mode_by_year <- Reduce(function(...) rbind(..., deparse.level=1), mode_by_year)
write.csv(mode_by_year, './DATOS_PROCESADOS/_suelos/suelo_modal_region_balance_hidrico.csv', row.names=FALSE)
rm(mode_by_year)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# CALCULATE Modal soil for AEPS (variable, farm and year)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

soil_data <- read.csv('./RESULTADOS/Identificacion_factores_limitantes/Soils_analysis/soil_data_by_plot.csv')

# Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux <- as.vector(na.omit(ux))
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate modal soil by year and farm
years <- unique(soil_data$Año_muestreo)
farms <- sort(as.character(unique(soil_data$Id_Finca)))
mode_by_year_farm <- lapply(1:length(years), function(i)
{
  year_results <- lapply(1:length(farms), function(j)
  {
    sub_soil <- subset(soil_data, subset=(soil_data$Año_muestreo==years[i] & soil_data$Id_Finca==farms[j]))
    if(nrow(sub_soil)>0){
      
      rownames(sub_soil) <- 1:nrow(sub_soil)
      mode_calculated <- apply(sub_soil, 2, Mode)
      mode_calculated <- as.data.frame(t(mode_calculated))
      
      # Modal texture
      texture <- as.matrix(sub_soil[,c('Arena_perc','Limo_perc')])
      tryCatch(expr={
        library(ks)
        kernel.dens <- kde(texture)
      },
      error=function(e){
        cat("Kernel density process failed:",years[i],"and",farms[j],"\n")
        return("Done\n")
      })
      if(exists('kernel.dens')){
        # kernel.dens <- kde(texture)
        kernelMatrix <- kernel.dens$estimate
        rownames(kernelMatrix) <- as.character(kernel.dens$eval.points[[1]])
        colnames(kernelMatrix) <- as.character(kernel.dens$eval.points[[2]])
        values <- which(kernelMatrix==max(kernelMatrix), arr.ind=TRUE)
        values <- as.numeric(c(rownames(kernelMatrix)[values[1]], colnames(kernelMatrix)[values[2]]))
        values[3] <- 100-sum(values)
        mode_calculated[,c('Arena_perc','Limo_perc','Arcilla_perc')] <- as.factor(values)
        mode_calculated <- mode_calculated[,c("Id_Finca","Año_muestreo","Arena_perc","Limo_perc","Arcilla_perc","Textura","pH",
                                              "Acidez_intercambiable_meq.100g","Soil_MO_perc","Soil_P_ppm","Soil_S_ppm",
                                              "Soil_K_meq.100g","Soil_Ca_meq.100g","Soil_Mg_meq.100g","Soil_Na_meq.100g",
                                              "Soil_Fe_meq.100g","Soil_Mn_meq.100g","Soil_Cu_meq.100g","Soil_Zn_meq.100g",
                                              "Soil_B_meq.100g","Soil_Perc_sat.K","Soil_Perc_sat.Ca","Soil_Perc_sat.Mg",
                                              "Soil_Perc_sat.Na","Soil_Perc_sat.Al")]
        mode_calculated[,c('Id_Finca','Año_muestreo')] <- as.factor(c(farms[j], years[i]))
        return(mode_calculated)
      } else {
        cat('Missing values exist\n')
      }
      mode_calculated <- mode_calculated[,c("Id_Finca","Año_muestreo","Arena_perc","Limo_perc","Arcilla_perc","Textura","pH",
                                            "Acidez_intercambiable_meq.100g","Soil_MO_perc","Soil_P_ppm","Soil_S_ppm",
                                            "Soil_K_meq.100g","Soil_Ca_meq.100g","Soil_Mg_meq.100g","Soil_Na_meq.100g",
                                            "Soil_Fe_meq.100g","Soil_Mn_meq.100g","Soil_Cu_meq.100g","Soil_Zn_meq.100g",
                                            "Soil_B_meq.100g","Soil_Perc_sat.K","Soil_Perc_sat.Ca","Soil_Perc_sat.Mg",
                                            "Soil_Perc_sat.Na","Soil_Perc_sat.Al")]
      mode_calculated[,c('Id_Finca','Año_muestreo')] <- as.factor(c(farms[j], years[i]))
      return(mode_calculated)
    } else {
      cat('Farm and year combination does not exists, NA values will be return\n')
      mode_calculated <- soil_data[1,]
      mode_calculated <- mode_calculated[,c("Id_Finca","Año_muestreo","Arena_perc","Limo_perc","Arcilla_perc","Textura","pH",
                                            "Acidez_intercambiable_meq.100g","Soil_MO_perc","Soil_P_ppm","Soil_S_ppm",
                                            "Soil_K_meq.100g","Soil_Ca_meq.100g","Soil_Mg_meq.100g","Soil_Na_meq.100g",
                                            "Soil_Fe_meq.100g","Soil_Mn_meq.100g","Soil_Cu_meq.100g","Soil_Zn_meq.100g",
                                            "Soil_B_meq.100g","Soil_Perc_sat.K","Soil_Perc_sat.Ca","Soil_Perc_sat.Mg",
                                            "Soil_Perc_sat.Na","Soil_Perc_sat.Al")]
      mode_calculated[,3:ncol(mode_calculated)] <- NA
      mode_calculated[,c('Id_Finca','Año_muestreo')] <- as.factor(c(farms[j], years[i]))
      return(mode_calculated)
    }
    
  })
  year_results <- Reduce(function(...) rbind(..., deparse.level=1), year_results)
  return(year_results)
})
mode_by_year_farm <- Reduce(function(...) rbind(..., deparse.level=1), mode_by_year_farm)
write.csv(mode_by_year_farm, './DATOS_PROCESADOS/_suelos/suelo_modal_finca_year_AEPS.csv', row.names=FALSE)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# LOAD Foliar data
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

foliar_data <- read_excel('./DATOS_PROCESADOS/Banasan_data.xlsx', sheet='Foliar')

varNames <- names(foliar_data)[7:length(names(foliar_data))]

# PLOT Distribution's variable
# Distributions of each variable by Farm and Year
lapply(1:length(varNames), function(i)
{
  library(ggplot2)
  eval(parse(text=paste('p <- ggplot(data=foliar_data, aes(x=',varNames[i],')) + geom_density(colour="darkgreen", fill="darkgreen")',sep='')))
  p <- p + facet_grid(Año_muestreo~Id_Finca)
  p <- p + theme_minimal() + theme_bw()
  p <- p + theme(axis.text.x = element_text(size=10),
                 axis.text.y = element_text(size=10),
                 axis.title.x = element_text(face="bold",size=11),
                 axis.title.y = element_text(face="bold",size=11),
                 legend.title = element_text(face="bold",size=11),
                 legend.text  = element_text(size=10))
  p <- p + ylab('Frecuencia')
  ggsave(filename=paste('./RESULTADOS/Identificacion_factores_limitantes/Foliar_analysis/_farm_year_distributions/',varNames[i],'.png', sep=''), plot=p, width=8, height=8, units='in', dpi=300)
  return(cat('Done!\n'))
})

g = gc(); rm(list=ls())

# CALCULATE Lote raw
foliar_data$Id_Lote_raw <- substr(foliar_data$Id_Lote, start=5, stop=nchar(foliar_data$Id_Lote))
foliar_data <- foliar_data[,c("Id_Lote", "Id_Finca", "Id_Lote_raw", 
                          "Año_muestreo", "Mes_muestreo", "Dia.semana_muestreo", "Codigo_muestra",
                          "Foliar_N_perc","Foliar_P_perc","Foliar_K_perc","Foliar_Ca_perc","Foliar_Mg_perc",
                          "Foliar_S_perc","Foliar_Cl_perc","Foliar_Fe_ug.g.1","Foliar_Mn_ug.g.1",
                          "Foliar_Cu_ug.g.1","Foliar_Zn_ug.g.1","Foliar_B_ug.g.1","Foliar_Na_ug.g.1",
                          "Foliar_Na_perc","Foliar_Perc_Sat.K","Foliar_Perc_Sat.Ca","Foliar_Perc_Sat.Mg")]
loteID <- strsplit(x=foliar_data$Id_Lote_raw, split='[[:punct:]]')
freq <- unlist(lapply(1:length(loteID), function(i){z <- length(loteID[[i]]); return(z)}))
foliar_data$Freq <- freq; rm(freq)
foliar_data <- foliar_data[rep(row.names(foliar_data), foliar_data$Freq), 1:(ncol(foliar_data)-1)]
foliar_data$Id_Lote_raw <- unlist(loteID); rm(loteID)
rownames(foliar_data) <- 1:nrow(foliar_data)
write.csv(foliar_data, './RESULTADOS/Identificacion_factores_limitantes/Foliar_analysis/foliar_data_by_plot.csv', row.names=F)

foliar_data <- read.csv('./RESULTADOS/Identificacion_factores_limitantes/Foliar_analysis/foliar_data_by_plot.csv')
varNames <- names(foliar_data)[8:length(names(foliar_data))]

fincasList <- sort(as.character(unique(foliar_data$Id_Finca)))

# PLOT Distribution's variable
# Distributions of each variable by Farm and Plot (PENDIENTE POR CORRER)
lapply(1:length(varNames), function(i)
{
  lapply(1:length(fincasList), function(j)
  {
    library(ggplot2)
    var.values <- as.vector(na.omit(foliar_data[foliar_data$Id_Finca==fincasList[j],varNames[i]]))
    if(length(var.values)>0)
    {
      eval(parse(text=paste('p <- ggplot(data=foliar_data[foliar_data$Id_Finca=="',fincasList[j],'",], aes(x=Id_Lote_raw, y=',varNames[i],', color=as.factor(Año_muestreo), shape=as.factor(Año_muestreo))) + geom_jitter(position=position_jitter(0.2))',sep='')))
      p <- p + theme_minimal() + theme_bw()
      p <- p + theme(axis.text.x = element_text(size=10),
                     axis.text.y = element_text(size=10),
                     axis.title.x = element_text(face="bold",size=11),
                     axis.title.y = element_text(face="bold",size=11),
                     legend.title = element_text(face="bold",size=11),
                     legend.text  = element_text(size=10))
      p <- p + labs(color='Year') + labs(shape='Year')
      ggsave(filename=paste('./RESULTADOS/Identificacion_factores_limitantes/Foliar_analysis/_plot_farm_year_distributions/',varNames[i],'_',fincasList[j],'.png', sep=''), plot=p, width=10, height=8, units='in', dpi=300)
    } else {
      cat(varNames[i],'does not have values for',fincasList[j],'year\n')
    }
  })
  return(cat('Done!\n'))
})

g = gc(); rm(list=ls())

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# CALCULATE Modal foliar data for AEPS (variable, farm and year)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

foliar_data <- read.csv('./RESULTADOS/Identificacion_factores_limitantes/Foliar_analysis/foliar_data_by_plot.csv')

# Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux <- as.vector(na.omit(ux))
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate modal foliar data by year and farm
years <- unique(foliar_data$Año_muestreo)
farms <- sort(as.character(unique(foliar_data$Id_Finca)))
mode_by_year_farm <- lapply(1:length(years), function(i)
{
  year_results <- lapply(1:length(farms), function(j)
  {
    sub_foliar <- subset(foliar_data, subset=(foliar_data$Año_muestreo==years[i] & foliar_data$Id_Finca==farms[j]))
    
    if(nrow(sub_foliar)>0){
      
      rownames(sub_foliar) <- 1:nrow(sub_foliar)
      mode_calculated <- apply(sub_foliar, 2, Mode)
      mode_calculated <- as.data.frame(t(mode_calculated))
      
      mode_calculated <- mode_calculated[,c("Id_Finca", "Año_muestreo",
                                            "Foliar_N_perc","Foliar_P_perc","Foliar_K_perc","Foliar_Ca_perc","Foliar_Mg_perc",
                                            "Foliar_S_perc","Foliar_Cl_perc","Foliar_Fe_ug.g.1","Foliar_Mn_ug.g.1",
                                            "Foliar_Cu_ug.g.1","Foliar_Zn_ug.g.1","Foliar_B_ug.g.1","Foliar_Na_ug.g.1",
                                            "Foliar_Na_perc","Foliar_Perc_Sat.K","Foliar_Perc_Sat.Ca","Foliar_Perc_Sat.Mg")]
      return(mode_calculated)
      
    } else {
      
      cat('Farm and year combination does not exists, NA values will be return\n')
      mode_calculated <- sub_foliar[1,]
      mode_calculated <- mode_calculated[,c("Id_Finca", "Año_muestreo",
                                            "Foliar_N_perc","Foliar_P_perc","Foliar_K_perc","Foliar_Ca_perc","Foliar_Mg_perc",
                                            "Foliar_S_perc","Foliar_Cl_perc","Foliar_Fe_ug.g.1","Foliar_Mn_ug.g.1",
                                            "Foliar_Cu_ug.g.1","Foliar_Zn_ug.g.1","Foliar_B_ug.g.1","Foliar_Na_ug.g.1",
                                            "Foliar_Na_perc","Foliar_Perc_Sat.K","Foliar_Perc_Sat.Ca","Foliar_Perc_Sat.Mg")]
      mode_calculated[,3:ncol(mode_calculated)] <- NA
      mode_calculated[,c('Id_Finca','Año_muestreo')] <- as.factor(c(farms[j], years[i]))
      return(mode_calculated)
      
    }
    
  })
  year_results <- Reduce(function(...) rbind(..., deparse.level=1), year_results)
  return(year_results)
})
mode_by_year_farm <- Reduce(function(...) rbind(..., deparse.level=1), mode_by_year_farm)
write.csv(mode_by_year_farm, './DATOS_PROCESADOS/_foliar/foliar_modal_finca_year_AEPS.csv', row.names=FALSE)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Unir cosechas, suelo y análisis foliar
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

g <- gc(); rm(list=ls())

options(warn=-1)
library(readxl)
library(raster)
library(rgdal)
library(maptools)
library(sp)

setwd('/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA')
# setwd('//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA')

cosechas <- read_excel('./DATOS_PROCESADOS/Banasan_data.xlsx', sheet='Cosechas')
suelo    <- read.csv('./DATOS_PROCESADOS/_suelos/suelo_modal_finca_year_AEPS.csv')
foliar   <- read.csv('./DATOS_PROCESADOS/_foliar/foliar_modal_finca_year_AEPS.csv')

grep2 <- function(pattern, x){grep(pattern, x)}
grep2 <- Vectorize(FUN=grep2, vectorize.args='pattern')

names(cosechas)[grep2(pattern=c('ID_finca','Year_cosecha'),names(cosechas))] <- c('IDFinca','Year_cosecha')
names(suelo)[grep2(pattern=c('Id_Finca','Year_muestreo'),names(suelo))] <- c('IDFinca','Year_cosecha')
names(foliar)[grep2(pattern=c('Id_Finca','Year_muestreo'),names(foliar))] <- c('IDFinca','Year_cosecha')

cosechasFinal <- base::merge(x=cosechas, y=suelo, by=c('IDFinca','Year_cosecha'), all.x=TRUE)
cosechasFinal <- base::merge(x=cosechasFinal, y=foliar, by=c('IDFinca','Year_cosecha'), all.x=TRUE)

cosechasFinal <- cosechasFinal[,c('IDFinca','Id_Lote','Longitud','Latitud','Finca','Area_finca', # Cosecha
                                  'Year_embolse','Semana_embolse',                               # Cosecha
                                  'Year_cosecha','Mes_cosecha','Semana_cosecha',                 # Cosecha
                                  'Recobro','Embolsados_lote','Racimos_cosechar',                # Cosecha
                                  'Perc_corta','Perc_premio','Ratio_premio','Ratio_total',       # Cosecha
                                  'Cajas_corta','Cajas_premio','Cajas_total',                    # Cosecha
                                  'Edad_cosecha','Perc_primera_B',                               # Cosecha
                                  'Arena_perc','Limo_perc','Arcilla_perc',                       # Suelo
                                  "pH","Acidez_intercambiable_meq.100g","Soil_MO_perc",          # Suelo
                                  "Soil_P_ppm","Soil_S_ppm","Soil_K_meq.100g",                   # Suelo
                                  "Soil_Ca_meq.100g","Soil_Mg_meq.100g","Soil_Na_meq.100g",      # Suelo
                                  "Soil_Fe_meq.100g","Soil_Mn_meq.100g","Soil_Cu_meq.100g",      # Suelo
                                  "Soil_Zn_meq.100g","Soil_B_meq.100g","Soil_Perc_sat.K",        # Suelo
                                  "Soil_Perc_sat.Ca","Soil_Perc_sat.Mg","Soil_Perc_sat.Na",      # Suelo
                                  "Soil_Perc_sat.Al",                                            # Suelo
                                  "Foliar_N_perc","Foliar_P_perc","Foliar_K_perc",               # Foliar
                                  "Foliar_Ca_perc","Foliar_Mg_perc","Foliar_S_perc",             # Foliar
                                  "Foliar_Cl_perc","Foliar_Fe_ug.g.1","Foliar_Mn_ug.g.1",        # Foliar
                                  "Foliar_Cu_ug.g.1","Foliar_Zn_ug.g.1","Foliar_B_ug.g.1",       # Foliar
                                  "Foliar_Na_ug.g.1","Foliar_Na_perc","Foliar_Perc_Sat.K",       # Foliar
                                  "Foliar_Perc_Sat.Ca","Foliar_Perc_Sat.Mg",                     # Foliar
                                  'Merma','Peso_racimo')]                                        # Cosecha
cosechasFinal$Racimos_cosechar_area <- cosechasFinal$Racimos_cosechar/cosechasFinal$Area_finca

write.csv(cosechasFinal, './DATOS_PROCESADOS/_cosecha/banasan_cosechas_suelo_foliar_AEPS.csv', row.names=FALSE)
