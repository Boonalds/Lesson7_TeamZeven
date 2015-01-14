calculate.RMSE.class <- function(r1,r2,class){
  r1sub <- r1$VCF
  r1sub[r1$Class != class] <- NA
  r2sub <- r2$VCF
  r2sub[r2$Class != class] <- NA

  RMSEsub <- calculate.RMSE(r1sub,r2sub)
  return(RMSEsub)
}

