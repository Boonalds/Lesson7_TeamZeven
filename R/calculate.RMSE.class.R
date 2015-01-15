calculate.RMSE.class <- function(r1,r2,class){
  # This function calculates the RMSE per class
  # r1 is Original VCF layer, r2 is the Predicted VCF layer.
  r1sub <- r1$VCF
  r1sub[r1$Class != class] <- NA
  r2sub <- r2$VCF
  r2sub[r2$Class != class] <- NA

  RMSEsub <- calculate.RMSE(r1sub,r2sub)
  return(RMSEsub)
}

