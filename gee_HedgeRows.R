#Código para clasificación supervisada GEE
#HegdeRows Catacamas
#K. Wiese
#3 de Marzo 2021

# Definir directorio de trabajo ----
setwd("~/R/RGEE_HedgeRows/")

# Librerias 
library(rgee)
library(sf)
source("Funciones/RGEE.R")

# Inicializar conexión con GEE
ee_Initialize(email = 'klauswiesengine@gmail.com', drive = TRUE)

# Cargar colección de datos para clasificación ----
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
select('VV')

# Reducción considerando la colección de imágenes VV para el rango de tiempo definido
S1VV.percs <- reduce.imagesS1(vv, "2020-01-01", "2020-12-31", extCatacamas, prefix='S1_', reducer=reducer)

# La salida de la reducción
S1VV.percs.bands <- S1VV.percs$bandNames()$getInfo()
S1VV.percs.bands

#paleta de colores
RADARParams <- list(palette = c(
  "#1a9850", "#66bd63", "#a6d96a", 
  "#d9ef8b", "#fee08b", "#fdae61",  
  "#f46d43", "#d73027" 
),
min = -20)


Map$centerObject(extCatacamas,zoom=12)
Map$addLayer(
  eeObject = extCatacamas,
  name = "Área de Trabajo"
)  +
Map$addLayer(
  eeObject = S1VV.percs$select("S1_VV_p25"),
  RADARParams,
  name = "RADAR VV Percentil 25"
) +
Map$addLayer(
  eeObject = S1VV.percs$select("S1_VV_p50"),
  RADARParams,
  name = "RADAR VV Percentil 50"
) +  
Map$addLayer(
  eeObject = S1VV.percs$select("S1_VV_p75"),
  RADARParams,
  name = "RADAR VV Percentil 75"
)   

# Cargar Sentinel-1 C-band SAR Ground Range collection (log scaling, VV co-polar)
vh <-  ee$ImageCollection('COPERNICUS/S1_GRD')$filterBounds(Catacamas)$
  filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VH'))$
  filter(ee$Filter$eq('instrumentMode', 'IW'))$
  select('VH')

# Reducción considerando la colección de imágenes VH para el rango de tiempo definido
S1VH.percs <- reduce.imagesS1(vh, "2020-01-01", "2020-12-31", extCatacamas, prefix='S1_', reducer=reducer)

# La salida de la reducción
S1VH.percs.bands <- S1VH.percs$bandNames()$getInfo()
S1VH.percs.bands

#Visualizar VH
Map$centerObject(extCatacamas,zoom=12)
Map$addLayer(
  eeObject = extCatacamas,
  name = "Área de Trabajo"
)  +
  Map$addLayer(
    eeObject = S1VH.percs$select("S1_VH_p25"),
    RADARParams,
    name = "RADAR VH Percentil 25"
  ) +
  Map$addLayer(
    eeObject = S1VH.percs$select("S1_VH_p50"),
    RADARParams,
    name = "RADAR VH Percentil 50"
  ) +  
  Map$addLayer(
    eeObject = S1VH.percs$select("S1_VH_p75"),
    RADARParams,
    name = "RADAR VH Percentil 75"
  )   

# Cargar Sentinel-1 C-band SAR Ground Range collection (log scaling, VV co-polar)
RadarVV <-  ee$ImageCollection('COPERNICUS/S1_GRD')$filterBounds(Catacamas)$
  filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VH'))$
  filter(ee$Filter$eq('instrumentMode', 'IW'))$
  filterDate('2019-01-01','2019-02-01')$
  select('VV')$median()$rename("VV")

RadarVH <-  ee$ImageCollection('COPERNICUS/S1_GRD')$filterBounds(Catacamas)$
  filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VH'))$
  filter(ee$Filter$eq('instrumentMode', 'IW'))$
  filterDate('2019-01-01','2019-02-01')$
  select('VH')$median()$rename("VH")


Map$centerObject(extCatacamas,zoom=12)
Map$addLayer(
  eeObject = extCatacamas,
  name = "Área de Trabajo"
)  +
  Map$addLayer(
    eeObject = RadarVH,
    RADARParams,
    name = "RADAR VH"
  ) +
  Map$addLayer(
    eeObject = RadarVV,
    RADARParams,
    name = "RADAR VV"
  ) 

# Sentinel 2
#Compuesto Sentinel 2
 Compuesto_sen2 <- ee$ImageCollection('COPERNICUS/S2_SR')$
  filterDate('2019-01-01','2021-03-01')$
  filterBounds(Catacamas)$
  filterMetadata('CLOUDY_PIXEL_PERCENTAGE','less_than',10)$median()$
  clip(Catacamas)$divide(10000)$select(c("B2","B3","B4","B5","B6","B7","B8","B9","B11","B12"))

#Una imagen
 Coleccion_sen2 <- ee$ImageCollection('COPERNICUS/S2_SR')$
   filterDate('2019-01-01','2021-03-01')$
   filterBounds(Catacamas)$
   filterMetadata('CLOUDY_PIXEL_PERCENTAGE','less_than',10)
 
#ee_get_date_ic(Coleccion_sen2)
#                                                        id          time_start
# 1 COPERNICUS/S2_SR/20190129T160521_20190129T161214_T16PFB 2019-01-29 16:19:17
# 2 COPERNICUS/S2_SR/20200104T160641_20200104T161448_T16PFB 2020-01-04 16:19:15
# 3 COPERNICUS/S2_SR/20200831T155911_20200831T161710_T16PFB 2020-08-31 16:19:25
# 4 COPERNICUS/S2_SR/20201109T160511_20201109T161049_T16PFB 2020-11-09 16:19:25

#Seleccionar imagen
id <- "COPERNICUS/S2_SR/20190129T160521_20190129T161214_T16PFB"
sen2 <- ee$Image(id)$clip(Catacamas)$divide(10000)$select(c("B2","B3","B4","B5","B6","B7","B8","B9","B11","B12"))

#Parámetros de visualización
S2.viz <- gen.vizParams(sen2, list('B8','B4','B3'), extCatacamas)

Map$centerObject(Catacamas,zoom=11)
Map$addLayer(
  eeObject = extCatacamas,
  name = "Área de Trabajo"
)  +
  Map$addLayer(
    eeObject = Compuesto_sen2,
    visParams = list(
      bands = c("B8", "B4", "B3"),
      max = 0.4
    ),
    name = "Sentinel Vegetación Infraroja Compuesto Temporal"
  ) +
  Map$addLayer(
    eeObject = sen2,
    visParams = list(
      bands = c("B8", "B4", "B3"),
      max = 0.4
    ),
    name = "Sentinel Vegetación Infraroja Imagen enero 2019"
  )  


# Índices Espectrales
S2ndvi <- Compuesto_sen2$normalizedDifference(c('B8','B4'))$rename("S2ndvi")
S2ndbi <- Compuesto_sen2$normalizedDifference(c('B11','B8'))$rename("S2ndbi") #built-up index
S2ndbi2 <- Compuesto_sen2$normalizedDifference(c('B12','B8'))$rename("S2ndbi2") #built-up index
S2evi <- EVI(Compuesto_sen2)$rename("S2evi")
S2gli <- GLI(Compuesto_sen2)$rename("S2gli")
S2savi <- SAVI(Compuesto_sen2)$rename("S2savi")
S2bsi <- BSI(Compuesto_sen2)$rename("S2bsi")
S2ndmi <- NDMI(Compuesto_sen2)$rename("S2ndmi")

#Métricas de vecindad
#cambio gradual
ndviGradient <- S2ndvi$gradient()$pow(2)$reduce("sum")$sqrt()$rename("NDVI_GRAD")
Map$addLayer(
  eeObject = ndviGradient,
  visParams = list(min = 0, max = 0.01, palette = c("darkgreen", "darkred")),
  name = "Gradiente NDVI"
)

#Diversidad Espectrla
#Shannon
Shannon <-  ee$Image('users/klauswiesengine/ShannonCatacamas')$rename('Shannon')

Map$addLayer(
  eeObject = Shannon,
  visParams = list(min = 0, max = 2.2, palette = c("darkred", "yellow", "orange", "darkgreen")),
  name = "Shannan Spatial Divesity"
)

#RAO
RAO <- ee$Image('users/klauswiesengine/CatacamasRAO')$rename("RAO")

Map$addLayer(
  eeObject = RAO,
  visParams = list(min = 0, max = 0.21, palette = c("darkred", "yellow", "orange", "darkgreen")),
  name = "Shannan Spatial Divesity"
)

# Unir todas
Brick <- Compuesto_sen2$addBands(S1VH.percs)$
              addBands(S1VV.percs)$
              addBands(RadarVH)$
              addBands(RadarVV)$
              addBands(S2ndvi)$
              addBands(S2evi)$
              addBands(S2bsi)$
              addBands(S2gli)$
              addBands(S2ndmi)$
              addBands(S2ndbi)$
              addBands(S2ndbi2)$
              addBands(RAO)$
              addBands(Shannon)$
              addBands(ndviGradient)

# Extraer nombres de cada variable
Brick.bands <- Brick$bandNames()$getInfo()
print(Brick.bands)


