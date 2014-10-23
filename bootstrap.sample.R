bootstrap.sample <- function(vector, samples){
  B <- matrix(sample(vector, length(vector)*samples, replace=T), nrow=samples, ncol=length(vector))
  return(B)
}