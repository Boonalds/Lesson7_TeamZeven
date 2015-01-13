subset.perclass <- function(x,y){
  vcf_perclass <- x$VCF
  vcf_perclass[x$Class != y] <- NA
  return(vcf_perclass)
}