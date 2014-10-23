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