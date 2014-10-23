is.multimodal <- function(vector){
  
  last = vector[1]
  increasing = TRUE
  
  for(i in 2:length(vector)){
    if(vector[i] > last){
      if(increasing == FALSE){
        return(TRUE)
      } 
    } else {
      increasing = FALSE
    }
    last = vector[i]
  }
  
  return(FALSE)
  
}