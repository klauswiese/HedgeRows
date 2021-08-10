# Funciones hechas por Leandro Leal Parente
#https://github.com/leandroleal
####################################################################################################

plot.samples <- function(samples, property, colors, layers = list()) {
  
  for(class.value in names(colors)) {
    color <- toString(colors[class.value])
    layer.fc <- samples$filterMetadata(property, 'Equals', as.numeric(class.value))
    layer.name <- paste0('Class ', class.value, ' (' , layer.fc$size()$getInfo(), ' samples)')
    print(paste0('Preparing ', layer.name))
    layers[[class.value]] <- Map$addLayer(layer.fc$draw(color), {}, layer.name)
  }
  
  Map$centerObject(samples$geometry(),10)
  Reduce('+', layers)
  
}

gen.vizParams <- function(image, bands, geometry, p1=2, p2=98, scale = 1000) {
  
  percent.stats <- image$reduceRegion(
    reducer = ee$Reducer$percentile(c(p1, p2)), 
    geometry = geometry, 
    bestEffort = TRUE,
    scale = scale,
    maxPixels = 1E13,
    tileScale = 2
  )$getInfo()
  
  viz <- list(
    bands = bands, 
    min = as.numeric(unlist(percent.stats[sprintf('%s_p%s', bands, rep(p1,3))])),
    max = as.numeric(unlist(percent.stats[sprintf('%s_p%s', bands, rep(p2,3))]))
  )
  
  return(viz)
}

reduce.images <- function(imgCollectionId, startDt, endDt, bands, geometry, valid.expr, prefix='', 
                          reducer = ee$Reducer$median()) {
  cloud.remove <- function(image) {
    valid.mask <- image$expression(valid.expr)
    image <- image$mask(valid.mask)$select(bands)
    return(image)
  }
  
  result <- ee$ImageCollection(imgCollectionId)$
    filterDate(startDt, endDt)$
    filterBounds(geometry)$
    map(cloud.remove)$
    reduce(reducer)$
    clip(geometry)$
    regexpRename('^',paste0(prefix))
  
  return(result)
}



reduce.imagesS1 <- function(imgCollectionId, startDt, endDt, geometry, prefix='', 
                          reducer = ee$Reducer$median()) {
  result <- ee$ImageCollection(imgCollectionId)$
    filterDate(startDt, endDt)$
    filterBounds(geometry)$
    reduce(reducer)$
    clip(geometry)$
    regexpRename('^',paste0(prefix))
  
  return(result)
}



overlayer.samples <- function(samples, image) {
  
  samples.ov <- samples$map(function(feat) {
    geom <- feat$geometry()
    result <- feat$set(image$reduceRegion(ee$Reducer$first(), geom, 10))
    return(result)
  })
  
  task.name <- paste0('overlay_', format(Sys.time(), format='%Y%m%d_%H%M%S'), '_', toString(sample(1:10000, 1)))
  task.params <- list(driveFolder =  'RGEE', fileFormat = 'CSV')
  
  print(paste0("Exporting to ", task.name))
  
  task <- ee$batch$Export$table(samples.ov, task.name, task.params)
  task$start()
  
  ee_monitoring()
  
  result.file <- ee_drive_to_local(task)
  result <- read.csv(result.file)
  
  return(result)
}

overlayer.polygons <- function(samples, image) {
  
  # Get the values for all pixels in each polygon in the training.
  samples.ov  <- image$sampleRegions(
    # Get the sample from the polygons FeatureCollection.
    collection = samples,
    # Keep this list of properties from the polygons.
    properties = list('landcover'),
    # Set the scale to get Landsat pixels in the polygons.
    scale = 10
  )
  

  task.name <- paste0('overlay_', format(Sys.time(), format='%Y%m%d_%H%M%S'), '_', toString(sample(1:10000, 1)))
  task.params <- list(driveFolder =  'RGEE', fileFormat = 'CSV')
  
  print(paste0("Exporting to ", task.name))
  
  task <- ee$batch$Export$table(samples.ov, task.name, task.params)
  task$start()
  
  ee_monitoring()
  
  result.file <- ee_drive_to_local(task)
  result <- read.csv(result.file)
  
  return(result)
}

#Índices de Vegetación

#EVI
EVI <- function (img) {
  evi <- img$expression(
    expression = '2.5 * ((nir - red) / (nir + 2.4 * red + 1))', opt_map = list(
      nir = img$select("B8"),
      red = img$select("B4")))
  return (evi)
}

GLI <- function (img){
  gli <- img$expression(
    expression = '((GREEN - RED) + (GREEN - BLUE)) / ((2 * GREEN) + RED + BLUE)', opt_map = list(
      GREEN = img$select("B3"),
      BLUE = img$select("B2"),
      RED = img$select("B4")))
  return(gli)
}

#SAVI (Soil Adjusted Vegetation Index)
SAVI <- function (img){
  savi <- img$expression(
    expression = '((NIR - RED) / (NIR + RED + 0.5))* (1.5)', opt_map = list(
      #L = 0.5,
      NIR = img$select("B8"),
      RED = img$select("B4")))
  return(savi)
}

#Bare Soil Index
BSI <- function (img){
  bsi <- img$expression(
    expression = '((SWIR + RED) - (NIR + BLUE))/((SWIR + RED) + (NIR + BLUE))', opt_map = list(
      BLUE = img$select("B2"),
      RED = img$select("B4"),
      NIR = img$select("B8"),
      SWIR = img$select("B11")))
  return(bsi)
}

#Moisture Index
NDMI <- function (img){
  ndmi <- img$expression(
    expression = '(NIR-SWIR)/(NIR+SWIR)', opt_map = list(
      NIR = img$select("B8"),
      SWIR = img$select("B11")))
  return(ndmi)
}





####################################################################################################
######## Helper functions
####################################################################################################

plot.samples <- function(samples, property, colors, layers = list()) {
  
  for(class.value in names(colors)) {
    color <- toString(colors[class.value])
    layer.fc <- samples$filterMetadata(property, 'Equals', as.numeric(class.value))
    layer.name <- paste0('Class ', class.value, ' (' , layer.fc$size()$getInfo(), ' samples)')
    print(paste0('Preparing ', layer.name))
    layers[[class.value]] <- Map$addLayer(layer.fc$draw(color), {}, layer.name)
  }
  
  Map$centerObject(samples$geometry(),10)
  Reduce('+', layers)
  
}

gen.vizParams <- function(image, bands, geometry, p1=2, p2=98, scale = 1000) {
  
  percent.stats <- image$reduceRegion(
    reducer = ee$Reducer$percentile(c(p1, p2)), 
    geometry = geometry, 
    bestEffort = TRUE,
    scale = scale,
    maxPixels = 1E13,
    tileScale = 2
  )$getInfo()
  
  viz <- list(
    bands = bands, 
    min = as.numeric(unlist(percent.stats[sprintf('%s_p%s', bands, rep(p1,3))])),
    max = as.numeric(unlist(percent.stats[sprintf('%s_p%s', bands, rep(p2,3))]))
  )
  
  return(viz)
}

reduce.images <- function(imgCollectionId, startDt, endDt, bands, geometry, valid.expr, prefix='', 
                          reducer = ee$Reducer$median()) {
  cloud.remove <- function(image) {
    valid.mask <- image$expression(valid.expr)
    image <- image$mask(valid.mask)$select(bands)
    return(image)
  }
  
  result <- ee$ImageCollection(imgCollectionId)$
    filterDate(startDt, endDt)$
    filterBounds(geometry)$
    map(cloud.remove)$
    reduce(reducer)$
    clip(geometry)$
    regexpRename('^',paste0(prefix))
  
  return(result)
}



reduce.imagesS1 <- function(imgCollectionId, startDt, endDt, geometry, prefix='', 
                          reducer = ee$Reducer$median()) {
  result <- ee$ImageCollection(imgCollectionId)$
    filterDate(startDt, endDt)$
    filterBounds(geometry)$
    reduce(reducer)$
    clip(geometry)$
    regexpRename('^',paste0(prefix))
  
  return(result)
}



overlayer.samples <- function(samples, image) {
  
  samples.ov <- samples$map(function(feat) {
    geom <- feat$geometry()
    result <- feat$set(image$reduceRegion(ee$Reducer$first(), geom, 10))
    return(result)
  })
  
  task.name <- paste0('overlay_', format(Sys.time(), format='%Y%m%d_%H%M%S'), '_', toString(sample(1:10000, 1)))
  task.params <- list(driveFolder =  'RGEE', fileFormat = 'CSV')
  
  print(paste0("Exporting to ", task.name))
  
  task <- ee$batch$Export$table(samples.ov, task.name, task.params)
  task$start()
  
  ee_monitoring()
  
  result.file <- ee_drive_to_local(task)
  result <- read.csv(result.file)
  
  return(result)
}

overlayer.polygons <- function(samples, image) {
  
  # Get the values for all pixels in each polygon in the training.
  samples.ov  <- image$sampleRegions(
    # Get the sample from the polygons FeatureCollection.
    collection = samples,
    # Keep this list of properties from the polygons.
    properties = list('landcover'),
    # Set the scale to get Landsat pixels in the polygons.
    scale = 10
  )
  

  task.name <- paste0('overlay_', format(Sys.time(), format='%Y%m%d_%H%M%S'), '_', toString(sample(1:10000, 1)))
  task.params <- list(driveFolder =  'RGEE', fileFormat = 'CSV')
  
  print(paste0("Exporting to ", task.name))
  
  task <- ee$batch$Export$table(samples.ov, task.name, task.params)
  task$start()
  
  ee_monitoring()
  
  result.file <- ee_drive_to_local(task)
  result <- read.csv(result.file)
  
  return(result)
}

#Índices de Vegetación

#EVI
EVI <- function (img) {
  evi <- img$expression(
    expression = '2.5 * ((nir - red) / (nir + 2.4 * red + 1))', opt_map = list(
      nir = img$select("B8"),
      red = img$select("B4")))
  return (evi)
}

GLI <- function (img){
  gli <- img$expression(
    expression = '((GREEN - RED) + (GREEN - BLUE)) / ((2 * GREEN) + RED + BLUE)', opt_map = list(
      GREEN = img$select("B3"),
      BLUE = img$select("B2"),
      RED = img$select("B4")))
  return(gli)
}

#SAVI (Soil Adjusted Vegetation Index)
SAVI <- function (img){
  savi <- img$expression(
    expression = '((NIR - RED) / (NIR + RED + 0.5))* (1.5)', opt_map = list(
      #L = 0.5,
      NIR = img$select("B8"),
      RED = img$select("B4")))
  return(savi)
}

#Bare Soil Index
BSI <- function (img){
  bsi <- img$expression(
    expression = '((SWIR + RED) - (NIR + BLUE))/((SWIR + RED) + (NIR + BLUE))', opt_map = list(
      BLUE = img$select("B2"),
      RED = img$select("B4"),
      NIR = img$select("B8"),
      SWIR = img$select("B11")))
  return(bsi)
}

#Moisture Index
NDMI <- function (img){
  ndmi <- img$expression(
    expression = '(NIR-SWIR)/(NIR+SWIR)', opt_map = list(
      NIR = img$select("B8"),
      SWIR = img$select("B11")))
  return(ndmi)
}

#Extraidos de: 
#Evaluation of Sentinel-1 & 2 time series for predicting wheat and rapeseed
#phenological stages

#S2REP
#705 + 35 * ((((B7 + B4)/2)-B5)/(B6-B5))
S2REP <- function (img){
  s2rep <- img$expression(
    expression = '705 + 35 * ((((NIR + ROJO)/2)-RE1)/(RE2-RE1))', opt_map = list(
      NIR = img$select("B8"),
      ROJO = img$select("B4"),
      RE1 = img$select("B5"),
      RE2 = img$select("B6")
      ))
  return(s2rep)
}

#MCARI
#[(B5 - B4) − 0.2(B5 - B3)] * (B5 - B4)
MCARI <- function (img){
  mcari <- img$expression(
    expression = '((RE1 - ROJO) - 0.2 * (RE1 - VERDE)) * (RE1 - ROJO)', opt_map = list(
      VERDE = img$select("B3"),
      ROJO = img$select("B4"),
      RE1 = img$select("B5")
    ))
  return(mcari)
}

#WDVI
#(B8 – 0,5 * B4)
WDVI <- function (img){
  wdvi <- img$expression(
    expression = '(NIR - 0.5 * ROJO)', opt_map = list(
      ROJO = img$select("B4"),
      NIR = img$select("B8")
    ))
  return(wdvi)
}

#NDYI
#NDYI=(B4−B3)/(B4+B3)
NDYI <- function (img){
  ndyi <- img$expression(
    expression = '(ROJO-VERDE)/(ROJO+VERDE)', opt_map = list(
      ROJO = img$select("B4"),
      VERDE = img$select("B3")
    ))
  return(ndyi)
}
