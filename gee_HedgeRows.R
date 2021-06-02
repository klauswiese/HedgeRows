#Código para clasificación supervisada GEE
#HegdeRows Catacamas
#K. Wiese
#3 de Marzo 2021

# Definir directorio de trabajo
setwd("~/R/RGEE_HedgeRows/")

# Librerias 
library(rgee)
library(sf)
library(raster)
source("Funciones/RGEE.R")

# Iniciar conexión con GEE
ee_Initialize(email = 'klauswiesengine@gmail.com', drive = TRUE)

# Cargar colección de datos para clasificación
#Límites área de estudio
Catacamas <- "SHP/CatacamasAOI.shp" %>%
  st_read(quiet = TRUE) %>% 
  sf_as_ee()

# Visualizar extent Área de Interés
extCatacamas <- Catacamas$geometry()$bounds()
Map$centerObject(extCatacamas,zoom=12)
Map$addLayer(
  eeObject = extCatacamas,
  name = "Área de Trabajo"
)  

# Reducción Temporal de datos RADAR ----
# Reducción para percentiles 25, 50 y 75
reducer <- ee$Reducer$percentile(c(25,50,75))

# Cargar Sentinel-1 C-band SAR Ground Range collection (log scaling, VV co-polar)
vv <-  ee$ImageCollection('COPERNICUS/S1_GRD')$filterBounds(Catacamas)$
  filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VV'))$
  filter(ee$Filter$eq('instrumentMode', 'IW'))$
  filter(ee$Filter$eq('orbitProperties_pass', 'DESCENDING'))$
  select('VV')


# Reducción considerando la colección de imágenes VV para el rango de tiempo definido
S1VV.percs <- reduce.imagesS1(vv, "2020-01-01", "2020-12-31", extCatacamas, prefix='S1_', reducer=reducer)

# La salida de la reducción
S1VV.percs.bands <- S1VV.percs$bandNames()$getInfo()
S1VV.percs.bands

# Filtrados
S1VVp25filt <- S1VV.percs$select("S1_VV_p25")$focal_median()$rename("S1_VV_p25filt")
S1VVp50filt <- S1VV.percs$select("S1_VV_p50")$focal_median()$rename("S1_VV_p50filt")
S1VVp75filt <- S1VV.percs$select("S1_VV_p75")$focal_median()$rename("S1_VV_p75filt")

# Cargar Sentinel-1 C-band SAR Ground Range collection (log scaling, VV co-polar)
vh <-  ee$ImageCollection('COPERNICUS/S1_GRD')$filterBounds(Catacamas)$
  filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VH'))$
  filter(ee$Filter$eq('instrumentMode', 'IW'))$
  filter(ee$Filter$eq('orbitProperties_pass', 'DESCENDING')) $
  select('VH')

# Reducción considerando la colección de imágenes VH para el rango de tiempo definido
S1VH.percs <- reduce.imagesS1(vh, "2020-01-01", "2020-12-31", extCatacamas, prefix='S1_', reducer=reducer)

# La salida de la reducción
S1VH.percs.bands <- S1VH.percs$bandNames()$getInfo()
S1VH.percs.bands

#Filtrados
S1VHp25filt <- S1VH.percs$select("S1_VH_p25")$focal_median()$rename("S1_VH_p25filt")
S1VHp50filt <- S1VH.percs$select("S1_VH_p50")$focal_median()$rename("S1_VH_p50filt")
S1VHp75filt <- S1VH.percs$select("S1_VH_p75")$focal_median()$rename("S1_VH_p75filt")

# Cargar Sentinel-1 C-band SAR Ground Range collection (log scaling, VV co-polar)
RadarVV <-  ee$ImageCollection('COPERNICUS/S1_GRD')$filterBounds(Catacamas)$
  filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VV'))$
  filter(ee$Filter$eq('instrumentMode', 'IW'))$
  filter(ee$Filter$eq('orbitProperties_pass', 'DESCENDING'))$
  filterDate('2019-01-01','2019-02-01')$
  select('VV')$median()$rename("VV")$clip(extCatacamas)

#Filtrado
RadarVVfilt <- RadarVV$focal_median()$rename("VVfilt")

RadarVH <-  ee$ImageCollection('COPERNICUS/S1_GRD')$filterBounds(Catacamas)$
  filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VH'))$
  filter(ee$Filter$eq('instrumentMode', 'IW'))$
  filter(ee$Filter$eq('orbitProperties_pass', 'DESCENDING'))$
  filterDate('2019-01-01','2019-02-01')$
  select('VH')$median()$rename("VH")$clip(extCatacamas)

#Filtrado
RadarVHfilt <- RadarVH$focal_median()$rename("VHfilt")

# Sentinel 2

#Una imagen
Coleccion_sen2 <- ee$ImageCollection('COPERNICUS/S2_SR')$
  filterDate('2019-01-01','2021-03-01')$
  filterBounds(Catacamas)$
  filterMetadata('CLOUDY_PIXEL_PERCENTAGE','less_than',10)

#imágenes que cumplen con la condición de menos de 10% de nubes en la escena completa
#ee_get_date_ic(Coleccion_sen2)
#                                                        id          time_start
# 1 COPERNICUS/S2_SR/20190129T160521_20190129T161214_T16PFB 2019-01-29 16:19:17
# 2 COPERNICUS/S2_SR/20200104T160641_20200104T161448_T16PFB 2020-01-04 16:19:15
# 3 COPERNICUS/S2_SR/20200831T155911_20200831T161710_T16PFB 2020-08-31 16:19:25
# 4 COPERNICUS/S2_SR/20201109T160511_20201109T161049_T16PFB 2020-11-09 16:19:25

#Seleccionar imagen
#imagen de enero 2021
id <- "COPERNICUS/S2_SR/20190129T160521_20190129T161214_T16PFB"
sen2 <- ee$Image(id)$clip(Catacamas)$divide(10000)$select(c("B2","B3","B4","B5","B6","B7","B8","B9","B11","B12"))

#Una imagen
# Índices Espectrales
S2ndviene <- sen2$normalizedDifference(c('B8','B4'))$rename("S2ndviene")
S2ndbiene <- sen2$normalizedDifference(c('B11','B8'))$rename("S2ndbiene") #built-up index
S2ndbi2ene <- sen2$normalizedDifference(c('B12','B8'))$rename("S2ndbi2ene") #built-up index
S2eviene <- EVI(sen2)$rename("S2eviene")
S2gliene <- GLI(sen2)$rename("S2gliene")
S2saviene <- SAVI(sen2)$rename("S2saviene")
S2bsiene <- BSI(sen2)$rename("S2bsiene")
S2ndmiene <- NDMI(sen2)$rename("S2ndmiene")
S2ndyiene <- NDYI(sen2)$rename("S2ndyiene")

#Propuestos Julie una imagen
S2repene <- S2REP(sen2)$rename("S2repene")$clip(extCatacamas)
S2mcariene <- MCARI(sen2)$rename("S2mcariene")$clip(extCatacamas)
S2wdviene <- WDVI(sen2)$rename("S2wdviene")$clip(extCatacamas)

#Métricas de vecindad
#cambio gradual
ndviGradientene <- S2ndviene$gradient()$pow(2)$reduce("sum")$sqrt()$rename("NDVI_GRADene")

#Diversidad Espectral
#Shannon
Shannon <-  ee$Image('users/klauswiesengine/ShannonCatacamas')$rename('Shannon')

#RAO
RAO <- ee$Image('users/klauswiesengine/CatacamasRAO')$rename("RAO")

# Unir todas
Brick <- sen2$
  addBands(S2ndviene)$float()$
  addBands(S2eviene)$float()$
  addBands(S2bsiene)$float()$
  addBands(S2gliene)$float()$
  addBands(S2ndmiene)$float()$
  addBands(S2ndbiene)$float()$
  addBands(S2ndbi2ene)$float()$
  addBands(ndviGradientene)$float()$
  addBands(S2repene)$float()$
  addBands(S2mcariene)$float()$
  addBands(S2wdviene)$float()$
  addBands(S2ndyiene)$float()$
  addBands(RAO)$float()$
  addBands(Shannon)$float()$
  addBands(S1VHp25filt)$
  addBands(S1VHp50filt)$
  addBands(S1VHp75filt)$
  addBands(S1VVp25filt)$
  addBands(S1VVp50filt)$
  addBands(S1VVp75filt)$
  addBands(RadarVHfilt)$
  addBands(RadarVVfilt)

# Extraer nombres de cada variable
Brick.bands <- Brick$bandNames()$getInfo()
print(Brick.bands)

#Muestras
# Cargar colección de datos para clasificación ----
#Muestras
Muestras <- "SHP/Entrenamiento.shp" %>%
  st_read(quiet = TRUE) %>% 
  sf_as_ee()

#Número de Muestras
Muestras$size()$getInfo()

# Clases cobertura vegetal y usos del suelo (CVUS):
## 0-'Cerco Vivo', 1-'Bosque', 2-Matorral, 3-Pasto
Colores <- list('0'='darkgreen', 
                '1'='green', 
                '2' = "white", 
                '3' = 'gray')

#Visualizar muestras
Map$centerObject(extCatacamas,zoom=12)
layers <- list(Map$addLayer(extCatacamas, {}, "Área de Interés"))
plot.samples(Muestras, 'Clave', Colores, layers)

# Sobreposición de muestras ----
ini <- Sys.time()
# Función para extraer los valores de stack de imágenes por cada muestra
# exporta a un archivo CSV a Google Drive, lo descarga y carga a R
samples.Catacamas <- overlayer.samples(Muestras, Brick)
fin <- Sys.time() - ini
print(fin)

# Filtar Columnas de interés
samples.Catacamas <- samples.Catacamas[,c(Brick.bands, 'Clave')]

#dir.create("CSV")
write.csv(samples.Catacamas, "CSV/samplesCatacamas.csv", row.names = FALSE)

# variable predictora a FACTOR 
# para que concuerde con mlr3
samples.Catacamas$Clave <- as.factor(samples.Catacamas$Clave)

# Tunning modelo Random Forest----

# Cargar mlr3 packages
# documentación https://mlr3book.mlr-org.com/introduction.html
library("mlr3")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3filters")
library("mlr3misc")
library("paradox")
library("mlr3tuning")

# Semilla para obtener resultados reproducibles
set.seed(1, "L'Ecuyer")

#Eliminar NA en varibles de entrada
samples.Catacamas <- na.omit(samples.Catacamas)

# Crear una nueva tarea de clasificación
task <- TaskClassif$new("Brick", samples.Catacamas, target = "Clave")

# Clasificación usando random forest aunque mlr3 soporta varios algoritmos
lrn <- lrn("classif.ranger", importance = "impurity")

# Aquí especificamos los hipeparametros soportados por GEE (ee.Classifier.smileRandomForest)
mtry.upper <- as.integer((ncol(samples.Catacamas)-1) / 2)
prms <- ParamSet$new(list(
  ParamInt$new("min.node.size", lower = 1, upper = 4), ## minLeafPopulation in GEE
  ParamInt$new("num.trees", lower = 50, upper = 500), ## numberOfTrees in GEE
  ParamInt$new("mtry", lower = 1, upper = mtry.upper) ## variablesPerSplit in GEE
))

# Definir los valores de los hiperpametros
design = expand.grid(
  min.node.size = seq(1,4,1),
  mtry = seq(1, mtry.upper, 3),
  num.trees = seq(50, 501, 25)
)

# Crear instancia de tuning
tnr.config <- TuningInstanceSingleCrit$new(
  task = task,
  learner = lrn,
  resampling = rsmp("cv", folds = 5)$instantiate(task), # 5-Fold crossvalidation
  measure = msr(c("classif.acc")), # Maximize the overall accuracy
  search_space = prms,
  terminator = trm("none") # all design should be evaluated
)

# Cargar tuner
tuner <- tnr("design_points", design = as.data.table(design))

# Comenzar la optimización
inis <- Sys.time()
tuner$optimize(tnr.config)
fin <- Sys.time() - inis
print(fin)

# ¿Cuáles son las variables más importantes?
filter <- flt("importance", learner = lrn)
filter$calculate(task)

# Resultados de la optimización
best.bands <- as.data.table(filter)
print(best.bands)

#guardar importancia de bandas
write.csv(best.bands, "CSV/BestBands.csv")

#Mejores parámetros de modelo
best.hparams <- tnr.config$result_learner_param_vals
print(best.hparams)

#$importance
#[1] "impurity"

#$min.node.size
#[1] 3

#$num.trees
#[1] 125

#$mtry
#[1] 10

#min.node.size, num.trees, mtry

# Clasificación con GEE
# Sobreponer los puntos en el stack de imágenes para obtener los datos de entrenamiento (Objeto GEE).
training <- Brick$sampleRegions(
  collection = Muestras,
  properties = list("Clave"),
  scale = 10,
  tileScale=2  
)

# RF con hiperparametros optimizados por MLR3
rf1 <- ee$Classifier$smileRandomForest(
  numberOfTrees=best.hparams$num.trees, 
  variablesPerSplit=best.hparams$mtry, 
  minLeafPopulation=best.hparams$min.node.size
)

# Entrenamiento de modelos
modelorf <- rf1$train(training, 'Clave', Brick.bands)

# Predicción de modelos
clasificacionRF <- Brick$classify(modelorf)

# Visualizar clasificación y Datos
Map$centerObject(extCatacamas,zoom=11)
Map$addLayer(
  eeObject = Brick,
  visParams = list(
    bands = c("B8", "B4", "B3"),
    max = 0.4
  ),
  name = "Sentinel Falso Color Infrarojo"
) +
  Map$addLayer(
    eeObject = clasificacionRF,
    visParams = list(
      palette = toString(Colores),
      min = 0,
      max = 3
    ),
    name = "Clasificación RF"
  )


Clasi <- ee_as_raster(clasificacionRF$clip(extCatacamas), scale=10)
plot(Clasi)
writeRaster(Clasi, paste("ClasificacionRF_", Sys.Date(), sep=""))



