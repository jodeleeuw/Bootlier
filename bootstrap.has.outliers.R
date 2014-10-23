bootstrap.has.outliers <- function(data, alpha=0.05, adjust=1) {
  # bootstrap the mean - trimmed mean stat
  M <- bootstrap.sample.mtm(data)
  # find critical bandwidth of M distribution
  h.crit <- find.h.crit(M)
  # bootstrap the critical bandwidth by sampling from the kernel density
  # estimate of M with bandwidth of h.crit
  M.samples <- bootstrap.sample(M, 1000)
  M.kernel.samples <- matrix(rnorm(M.samples, mean=M.samples, sd = h.crit), nrow=dim(M.samples)[1], ncol=dim(M.samples)[2])
  h.crit.bootstrap <- apply(M.kernel.samples, 1, find.h.crit)
  # check the significance value of the measured critical bandwidth
  p.inv <- sum(h.crit.bootstrap <= h.crit*adjust)/length(h.crit.bootstrap)
  has.outliers <- p.inv >= 1 - alpha
  return(has.outliers)
}