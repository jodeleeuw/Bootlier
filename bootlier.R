stat.mtm <- function(values, trim_amount) {
  # untrimmed mean
  mean <- mean(values)
  
  # trim values by trim_amount
  sorted_values <- sort(values)
  trimmed_values <- sorted_values[ (trim_amount + 1) : (length(sorted_values) - trim_amount)]
  
  # mean of trimmed values
  mean.trim <- mean(trimmed_values)
  
  # return the untrimmed mean - trimmed mean
  return(mean - mean.trim)
}

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

bootstrap.sample <- function(vector, samples){
  B <- matrix(sample(vector, length(vector)*samples, replace=T), nrow=samples, ncol=length(vector))
  return(B)
}

bootstrap.sample.mtm <- function(vector, samples=1000, trim_amount=2){
  B <- bootstrap.sample(vector,samples)
  M <- apply(B, 1, function(x) { return(stat.mtm(x,trim_amount))})
  return(M)
}

bootstrap.has.outliers <- function(data, bootstrap.sample.size=1000, mtm.trim.amount=2, alpha=0.05, adjust=1, h.crit.search.depth=25) {
  # bootstrap the mean - trimmed mean stat
  M <- bootstrap.sample.mtm(data, bootstrap.sample.size, mtm.trim.amount)
  # find critical bandwidth of M distribution
  h.crit <- find.h.crit(M, h.crit.search.depth)
  # bootstrap the critical bandwidth by sampling from the kernel density
  # estimate of M with bandwidth of h.crit
  M.samples <- bootstrap.sample(M, bootstrap.sample.size)
  M.kernel.samples <- matrix(rnorm(M.samples, mean=M.samples, sd = h.crit), nrow=dim(M.samples)[1], ncol=dim(M.samples)[2])
  h.crit.bootstrap <- apply(M.kernel.samples, 1, function(x){ return(find.h.crit(x, h.crit.search.depth)) })
  # check the significance value of the measured critical bandwidth
  p.inv <- sum(h.crit.bootstrap <= h.crit*adjust)/length(h.crit.bootstrap)
  has.outliers <- p.inv >= 1 - alpha
  return(has.outliers)
}

bootstrap.identify.outliers <- function(data, bootstrap.sample.size=1000, mtm.trim.amount=2, alpha=0.05, adjust=1, h.crit.search.depth=25){
  
  if(!bootstrap.has.outliers(data, bootstrap.sample.size, mtm.trim.amount, alpha, adjust, h.crit.search.depth)){
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
      has.outlier <- bootstrap.has.outliers(truncated_data, bootstrap.sample.size, mtm.trim.amount, alpha, adjust, h.crit.search.depth)
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