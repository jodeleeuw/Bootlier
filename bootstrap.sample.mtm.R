bootstrap.sample.mtm <- function(vector, samples=1000, trim_amount=2){
  B <- bootstrap.sample(vector,samples)
  M <- apply(B, 1, function(x) { return(stat.mtm(x,trim_amount))})
  return(M)
}