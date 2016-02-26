# Estadisticas descriptivas - Informe final MADR
# H. Achicanoy
# CIAT, 2016

sc1 <- read.csv('Z:/DATOS_PROCESADOS/_cosecha/_banasan/banasan_cosechas_suelo_foliar_clima_cycle_corrected.csv')
sc1 <- sc1[complete.cases(sc1),]; sc1 <- sc1[sc1$Peso_racimo>0,]; rownames(sc1) <- 1:nrow(sc1)
sc1 <- data.frame(scenario='Escenario 1', peso_racimo=sc1$Peso_racimo)

sc2 <- read.csv('Z:/DATOS_PROCESADOS/_cosecha/_cobana/cobana_cosechas_fertilizaciones_clima_cycle.csv')
sc2 <- sc2[complete.cases(sc2),]; sc2 <- sc2[sc2$Peso_racimo>0,]; rownames(sc2) <- 1:nrow(sc2)
sc2 <- data.frame(scenario='Escenario 2', peso_racimo=sc2$Peso_racimo)

sc3 <- read.csv('Z:/DATOS_PROCESADOS/_cosecha/all_clima_cycle.csv')
sc3 <- sc3[complete.cases(sc3),]; sc3 <- sc3[sc3$Peso_racimo>0,]; rownames(sc3) <- 1:nrow(sc3)
sc3 <- data.frame(scenario='Escenario 3', peso_racimo=sc3$Peso_racimo)

scenarios <- rbind(sc1, sc2, sc3)
rm(sc1, sc2, sc3)

library(ggplot2)

g <- ggplot(data=scenarios, aes(x=peso_racimo, colour=scenario)) + stat_ecdf(size=1.2)
g <- g + xlab('Peso del racimo (kg)') + ylab('Densidad acumulada')
g <- g + guides(colour=guide_legend(title=NULL))
g <- g + theme_bw()
g <- g + theme(axis.text  = element_text(size=18),
               axis.title = element_text(size=20, face="bold"),
               legend.text = element_text(size=18))
g
ggsave(filename='ecdf_scenario.png', plot=g, width=10, height=8, units='in')


# g <- ggplot(data=scenarios, aes(x=peso_racimo, colour=scenario)) + geom_density()
# g <- g + xlab('Peso del racimo (kg)') + ylab('Densidad acumulada')
# g <- g + guides(colour=guide_legend(title=NULL))
# g <- g + theme_bw()
# g <- g + theme(axis.text  = element_text(size=18),
#                axis.title = element_text(size=20, face="bold"),
#                legend.text = element_text(size=18))
# g
