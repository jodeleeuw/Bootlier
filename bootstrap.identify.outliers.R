bootstrap.identify.outliers <- function(data){
  
  if(!bootstrap.has.outliers(data)){
    return(list(
      data.truncated = data,
      data.outlier.set = NULL
    ))
  }
  
  data.sorted <- sort(data)
  
  # start with all removals with only 1 element, then 2 elements, then 3, and so on.
  for(remove.N in 1:(length(data.sorted)-1)){
    
    # compute all possible sets of extreme value removal for this N of removed items
    removal_sets = matrix(nrow=remove.N+1, ncol=2)
    for(low in 0:remove.N){
      removal_sets[low+1,] =  c(low, remove.N-low)
    }
    
    outcomes = c()
    any.false = FALSE
    for(i in 1:dim(removal_sets)[1]){
      truncated_data = data.sorted[ (1 + removal_sets[i,1]) : (length(data.sorted)-removal_sets[i,2]) ];
      has.outlier <- bootstrap.has.outliers(truncated_data)
      outcomes = c( outcomes, has.outlier )
      if(!has.outlier) { any.false = TRUE }
    }
    
    if(any.false){
      break;
    }
    
  }
  
  data.outlier.set = c()
  data.truncated = c()
  for(i in 1:length(outcomes)){
    if(outcomes[i] == FALSE){
      dt = data.sorted[ (1 + removal_sets[i,1]) : (length(data.sorted)-removal_sets[i,2]) ];
      do = c()
      if(removal_sets[i,1]>0){
        do = c( do, data.sorted[1:removal_sets[i,1]])
      }
      if(removal_sets[i,2]>0){
        do = c( do, data.sorted[(length(data.sorted)-removal_sets[i,2]+1):length(data.sorted)])
      }
      
      data.outlier.set = do
      data.truncated = dt
      
    }
  }
  
  return(list(
    data.truncated = data.truncated,
    data.outlier.set = data.outlier.set
  ))
}