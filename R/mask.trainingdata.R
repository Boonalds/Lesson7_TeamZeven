mask.trainingdata <- function(x,y){
  # This function masks out everything except the trainingareas.
  # x is a VCF raster, y stands for the training areas
  trainingareas <- mask(x,y)
  y@data$Code <- as.numeric(y@data$Class) #select the variable code from the trainingareas
  classes <- rasterize(trainingPoly, x, field = 'Class') #convert the data to a raster
  names(classes) <- 'Class'
  trainingclass <- addLayer(trainingareas, classes)
    
  return(trainingclass)
}
