calculate.RMSE <- function (x,y){
  minus<-x-y
  square<-minus^2
  mean<-cellStats(square, 'mean')
  RMSE <- sqrt(mean)
  return(RMSE)
}