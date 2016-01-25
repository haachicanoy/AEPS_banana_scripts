# Regression methods Banana case
# H. Achicanoy & G. Calberto
# CIAT, 2015

# ----------------------------------------------------------------------------------------------------------------- #
# Create short directory from Linux OS (linux code)
# ----------------------------------------------------------------------------------------------------------------- #

# Create a symbolic link command
## ln -s {/path/to/file-name} {link-name}
## ln -s /mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA Z

# I tried to create a shortcut on linux OS but it seems that doesn't work
# when you try to concatenate strings using paste function. So, in this case is better to use the complete directory

# ----------------------------------------------------------------------------------------------------------------- #
# Load packages
# ----------------------------------------------------------------------------------------------------------------- #

options(warn=-1)
if(!require(gtools)){install.packages('gtools');library(gtools)} else{library(gtools)}
if(!require(gridBase)){install.packages('gridBase');library(gridBase)} else{library(gridBase)}
if(!require(relaimpo)){install.packages('relaimpo');library(relaimpo)} else{library(relaimpo)}
if(!require(caret)){install.packages('caret');library(caret)} else{library(caret)}
if(!require(party)){install.packages('party');library(party)} else{library(party)}
if(!require(randomForest)){install.packages('randomForest');library(randomForest)} else{library(randomForest)}
if(!require(snowfall)){install.packages('snowfall');library(snowfall)} else{library(snowfall)}
if(!require(earth)){install.packages('earth');library(earth)} else{library(earth)}
if(!require(reshape)){install.packages('reshape');library(reshape)} else{library(reshape)}
if(!require(agricolae)){install.packages('agricolae');library(agricolae)} else{library(agricolae)}
if(!require(stringr)){install.packages('stringr');library(stringr)} else{library(stringr)}
if(!require(readxl)){install.packages('readxl');library(readxl)} else{library(readxl)}
if(!require(raster)){install.packages('raster');library(raster)} else{library(raster)}
if(!require(rgdal)){install.packages('rgdal');library(rgdal)} else{library(rgdal)}
if(!require(maptools)){install.packages('maptools');library(maptools)} else{library(maptools)}
if(!require(sp)){install.packages('sp');library(sp)} else{library(sp)}
if(!require(dismo)){install.packages('dismo');library(dismo)} else{library(dismo)}
if(!require(gbm)){install.packages('gbm');library(gbm)} else{library(gbm)}
if(!require(cowplot)){install.packages('cowplot');library(cowplot)} else{library(cowplot)}
if(!require(gridExtra)){install.packages('gridExtra');library(gridExtra)} else{library(gridExtra)}

# ----------------------------------------------------------------------------------------------------------------- #
# Set work directory
# ----------------------------------------------------------------------------------------------------------------- #

# Windows OS
dirFol  <- "//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA" # Long version
dirFol  <- "Z:" # Short version

# Linux OS
dirFol  <- "/mnt/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/ASBAMA" # Long version
# dirFol  <- "Z" # Short version. PLEASE DON'T USE

wkDir <- paste(dirFol, '/DATOS_PROCESADOS/_cosecha', sep=''); setwd(wkDir)

# ----------------------------------------------------------------------------------------------------------------- #
# Read database
# ----------------------------------------------------------------------------------------------------------------- #

dataSet <- read.csv('cobana_cosechas_fertilizaciones_clima_cycle.csv') # Change according database to analyse. It could be included climate, soils, foliar, etc. information
dataSet <- dataSet[complete.cases(dataSet),]; rownames(dataSet) <- 1:nrow(dataSet)

# ----------------------------------------------------------------------------------------------------------------- #
# Select variables to analyse from database
# ----------------------------------------------------------------------------------------------------------------- #

names(dataSet) # With this command you can see all variable names in the original dataset
# Following code shows the variables that are selected
dataSet <- dataSet[,c("Arena_perc","Limo_perc","Arcilla_perc",
                      "pH","Soil_MO_perc","Soil_P_ppm",
                      "Soil_S_ppm","Soil_K_meq.100g","Soil_Ca_meq.100g",
                      "Soil_Mg_meq.100g","Soil_Na_meq.100g","Soil_Fe_meq.100g",
                      "Soil_Mn_meq.100g","Soil_Cu_meq.100g","Soil_Zn_meq.100g",
                      "Soil_B_meq.100g","Soil_Perc_sat.K","Soil_Perc_sat.Ca",
                      "Soil_Perc_sat.Mg","Soil_Perc_sat.Na","Soil_Perc_sat.Al",
                      "Foliar_N_perc","Foliar_P_perc","Foliar_K_perc",
                      "Foliar_Ca_perc","Foliar_Mg_perc","Foliar_S_perc",
                      "Foliar_Cl_perc","Foliar_Fe_ug.g.1","Foliar_Mn_ug.g.1",
                      "Foliar_Cu_ug.g.1","Foliar_Zn_ug.g.1","Foliar_B_ug.g.1",
                      "Foliar_Na_ug.g.1","Foliar_Perc_Sat.K","Foliar_Perc_Sat.Ca",
                      "Foliar_Perc_Sat.Mg","Peso_racimo")]

dataSet <- dataSet[,c(24:35)]

dataSet$splitVar <- 'All' # In case of exists variety variable doesn't run this line and use that variable like segmentation variable
dataSet <- dataSet[,c(names(dataSet)[1:11],'splitVar','Peso_racimo')]

inputs  <- 1:11  # inputs columns
segme   <- 12    # split column; In case of exists variety variable USE IT HERE
output  <- 13    # output column

namsDataSet <- names(dataSet)

# ----------------------------------------------------------------------------------------------------------------- #
# Creating the split factors (in case of exists more than 1 variety run models for each variety)
# ----------------------------------------------------------------------------------------------------------------- #

wkDir <- paste(dirFol, '/RESULTADOS/Modelling/_scripts', sep=''); setwd(wkDir)

load('All-Functions-AEPS_BD.RData')
contVariety <- table(dataSet[,segme])
variety0    <- names(sort(contVariety[contVariety>=30]))
if(length(variety0)==1){variety = variety0 }else{variety = factor(c(variety0,"All"))}
variety <- 'All' # Omit this line in case of exists more than 1 variety

wkDir <- paste(dirFol, '/RESULTADOS/Modelling/_informe_final', sep='')
runID <- paste(wkDir, '/_run1', sep='')
if(!dir.exists(runID)){cat('Creating run directory\n'); dir.create(runID)} else {cat('Run directory exists\n')}
setwd(runID)

# ----------------------------------------------------------------------------------------------------------------- #
# Creating folders
# ----------------------------------------------------------------------------------------------------------------- #

createFolders(dirFol, variety)

# ----------------------------------------------------------------------------------------------------------------- #
# Descriptive analysis
# ----------------------------------------------------------------------------------------------------------------- #

descriptiveGraphics("All", dataSet, inputs=inputs, segme=segme, output=output, smooth=T, ylabel="Peso del racimo (kg)", smoothInd=NULL, ghrp="box")

# ----------------------------------------------------------------------------------------------------------------- #
# DataSets ProcesosF; parallelize processes usign caret R package
# ----------------------------------------------------------------------------------------------------------------- #

dataSetProces(variety, dataSet, segme, corRed="caret")

# ----------------------------------------------------------------------------------------------------------------- #
# Run Linear model regression; only when all inputs are cuantitative
# ----------------------------------------------------------------------------------------------------------------- #

lineaRegresionFun(variety, dirLocation=paste0(getwd(),"/"), ylabs="Peso del racimo (kg)")

# ----------------------------------------------------------------------------------------------------------------- #
# Run Multilayer perceptron
# ----------------------------------------------------------------------------------------------------------------- #

multilayerPerceptronFun(variety, dirLocation=paste0(getwd(),"/"), nb.it=30, ylabs="Peso del racimo (kg)", pertuRelevance=T, ncores=23)

# ----------------------------------------------------------------------------------------------------------------- #
# Run Random Forest
# ----------------------------------------------------------------------------------------------------------------- #

randomForestFun(variety, nb.it=100, ncores=23)

# ----------------------------------------------------------------------------------------------------------------- #
# Run Conditional Forest; especify if you have categorical variables
# ----------------------------------------------------------------------------------------------------------------- #

conditionalForestFun(variety, nb.it=30, ncores=23)

# ----------------------------------------------------------------------------------------------------------------- #
# Run Generalized Boosted Models
# ----------------------------------------------------------------------------------------------------------------- #

rest <- gbm.step(dataSet, gbm.x=1:64, gbm.y=65, tree.complexity=5, learning.rate=0.05, family="gaussian", n.trees=113); summary(rest)

# Find interactions
find.int <- gbm.interactions(rest); find.int$interactions; find.int$rank.list

# Plotting interactions
par(mar=c(1,1,0.2,0.2))
gbm.perspec(rest, y=4, x=38, y.range=range(dataSet[,4]), x.range=range(dataSet[,38]), z.range=c(44,55), y.label='Diurnal_Range_avg_LEAF', x.label='TX_freq_34_DEVL', z.label='Racimos cosechados por ha', col='forestgreen')
gbm.perspec(rest, y=4, x=11, y.range=range(dataSet[,4]), x.range=range(dataSet[,11]), z.range=c(44,55), y.label='Diurnal_Range_avg_LEAF', x.label='SR_accu_LEAF', z.label='Racimos cosechados por ha', col='forestgreen')
gbm.perspec(rest, y=4, x=10, y.range=range(dataSet[,4]), x.range=range(dataSet[,10]), z.range=c(44,55), y.label='Diurnal_Range_avg_LEAF', x.label='RH_avg_LEAF', z.label='Racimos cosechados por ha', col='forestgreen')
