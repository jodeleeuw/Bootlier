find.h.crit <- function(vector, search_depth=25){
  
  require(KernSmooth)
  
  h.max = abs(max(vector)-min(vector))
  h.min = 0
  #H = c()
  for (i in 1:search_depth) {
    h = h.min + ((h.max - h.min) / 2)
    k <- bkde(vector, bandwidth = h)$y
    if(is.multimodal(k)){
      h.min = h
    } else {
      h.max = h
    }
    #H = c(H, h)
  }
  
  h.crit = h
  
  return(h.crit)
  
}