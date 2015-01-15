calculate.RMSE <- function (x,y){
  # This function calculates the RMSE
  # x is Original VCF layer, y is the Predicted VCF layer
  minus<-x-y
  square<-minus^2
  mean<-cellStats(square, 'mean')
  RMSE <- sqrt(mean)
  return(RMSE)
}