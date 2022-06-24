## Function to optimize jittering parameter 
find_sigma <- function(d0, d1, dd = 1 / 3600, rel.tol = 1e-3) {
  
  f <- function(sigma, d0, dd = 1 / 3600, rel.tol = 1e-3) {
    x1 <- seq(from = 0, to = 45, by = d0)
    xx <- seq(from = 0, to = 45, by = dd)
    nxx <- length(xx)
    ss <- numeric(nxx)
    for (i in 1:nxx) {
      ss[i] <- sum(dnorm(xx[i], mean = x1, sd = sigma))
    }
    mask <- seq(from = 15 / dd + 1, to = 30 / dd, by = 1)
    diff(range(ss[mask])) / mean(ss[mask]) - rel.tol
  }
  
  uniroot(f, c(dd, max(d0, d1)), d0 = d0, rel.tol = rel.tol)$root

}
