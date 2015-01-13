mask.trainingdata <- function(x,y){
  trainingareas <- mask(x,y)
  y@data$Code <- as.numeric(y@data$Class)
  classes <- rasterize(trainingPoly, x, field = 'Class')
  names(classes) <- 'Class'
  trainingclass <- addLayer(trainingareas, classes)
    
  return(trainingclass)
}
