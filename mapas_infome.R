library(leaflet)
library(raster)
library(RColorBrewer)
library(htmlwidgets)
library(webshot)

# Read cluster's raster
cluster_map <- raster('Z:/RESULTADOS/Clasificacion_edafoclimatica/_results/climate_soils/mag_gua/DBSCAN100-1_clusters_colombia_15_30cm.tif')

# Delete shorter clusters
grep2 <- Vectorize(FUN=grep, vectorize.args='pattern')
nullCells <- unlist(grep2(pattern=paste('^', c(0,4,5,7:11,13,14), '$', sep=''), x=cluster_map[])); names(nullCells) <- NULL
cluster_map[][nullCells] <- NA; rm(nullCells)

# Climate stations coordinates
clim_station <- read.csv('Z:/DATOS_PROCESADOS/_clima/climate_stations_cluster.csv')

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Make beautiful map CLUSTERS + CLIMATE STATIONS
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

pal <- colorNumeric(brewer.pal(4, 'Set2'), domain=as.numeric(names(table(values(cluster_map)))), na.color="transparent")
m <- leaflet(clim_station) %>% addTiles() %>% setView(lng=-73, lat=10.5, zoom=7) %>% addProviderTiles("Acetate.terrain") # Acetate.terrain, MapQuestOpen.Aerial, Esri.WorldImagery, HikeBike.HikeBike
m <- m %>% addRasterImage(cluster_map, colors=pal, opacity=0.60, project=FALSE) %>% addLegend(pal=pal, values=as.numeric(names(table(values(cluster_map)))), title="Zonas edafoclimáticas")
pal2 <- colorFactor(c("navy", "red"), domain = c("clim", "farm"))
m <- m %>% addCircleMarkers(~Lon, ~Lat, popup = ~Station, radius = ~ifelse(type=="farm", 6, 10), color = ~pal2(type), stroke = FALSE, fillOpacity = 0.5)
# m <- m %>% addMarkers(~Lon, ~Lat, popup = ~Station, markerOptions(clickable=FALSE))
m

saveWidget(m, "Z:/DOCUMENTOS/clusters_stations.html", selfcontained=FALSE)
# webshot("temp.html", file="Rplot.png", cliprect="viewport")

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Make beatiful maps FARMS BY COMERCIALIZADORA
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# pal <- colorNumeric(brewer.pal(4, 'Set2'), domain=as.numeric(names(table(values(cluster_map)))), na.color="transparent")
clim_station2 <- clim_station[clim_station$comercializadora!='clim',]
rownames(clim_station2) <- 1:nrow(clim_station2)
clim_station2$comercializadora <- as.character(clim_station2$comercializadora)

pal <- colorNumeric(brewer.pal(4, 'Set2'), domain=as.numeric(names(table(values(cluster_map)))), na.color="transparent")
m <- leaflet(clim_station2) %>% addTiles() %>% setView(lng=-73, lat=10.5, zoom=7) %>% addProviderTiles("Acetate.terrain") # Acetate.terrain, MapQuestOpen.Aerial, Esri.WorldImagery, HikeBike.HikeBike
m <- m %>% addRasterImage(cluster_map, colors=pal, opacity=0.60, project=FALSE) %>% addLegend(pal=pal, values=as.numeric(names(table(values(cluster_map)))), title="Zonas edafoclimáticas")
pal2 <- colorFactor(c("navy", "forestgreen", "red"), domain = c("Banasan", "Cobana", "La Samaria"))
m <- m %>% addCircleMarkers(~Lon, ~Lat, popup=~Station, radius=rep(4.5, 3), color=~pal2(comercializadora), stroke=FALSE, fillOpacity=0.8)
# m <- m %>% addLegend(pal=pal2, values=as.numeric(names(table(values(cluster_map)))), title="Zonas edafoclimáticas")
# m <- m %>% addMarkers(~Lon, ~Lat, popup = ~Station, markerOptions(clickable=FALSE))
m

saveWidget(m, "Z:/DOCUMENTOS/clusters_farms.html", selfcontained=FALSE)

# Colour palette
library(RColorBrewer)
f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
(cols <- f("Set2"))

col2rgb(cols, alpha=FALSE)

