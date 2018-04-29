##################################
# Reference cases for covariance #
##################################

# Normal case, test 1
test.cov <- function() {
  x <- c(1.,2.,1.,2.,1.)
  y <- c(2.,1.,2.,1.,2.)
  return(cov(x,y))
}

# Normal case, test 2
test.cov.2 <- function() {
  x <- c(1.,2.,NA,2.,1.)
  y <- c(2.,1.,2.,1.,2.)
  return(cov(x,y,use="pairwise.complete.obs"))
}

#####################################
# Reference case for autocovariance #
#####################################

# Normal case, test 1
test.acov <- function() {
  x <- c(1.0, 1.9, 3.1, 3.9, 5.2)
  a <- acf(x, lag.max=4, type="covariance")
  return(a)
}


# Normal case, autcorrelation, test 1
test.acor <- function() {
  x <- c(1.0, 1.9, 3.1, 3.9, 5.2)
  a <- acf(x, lag.max=4, type="correlation")
  return(a)
}


# Normal case, partial autcorrelation, test 1
test.pacf <- function() {
  x <- c(1.0, 1.9, 3.1, 3.9, 5.2)
  d <- acf(x, lag.max=4, type="correlation")
  print(d$acf[2:5])
  print(durbin.levinson(d$acf[2:5]))
  a <- acf(x, lag.max=4, type="partial")
  return(a)
}

durbin.levinson <-
  function(rho)
  {
    n = length(rho)
    pacf = phi = numeric(n)
    pacf[1] = phi[1] = rho[1]
    for(k in 2:n) {
      km1 = k - 1
      numer = rho[k] - sum(phi[1:km1]*rho[km1:1])
      denom = 1 - sum(phi[1:km1]*rho[1:km1])
      phi[k] = numer / denom
      phi[1:km1] = phi[1:km1] - phi[k] * phi[km1:1]
      pacf[k] = phi[k]
    }
    pacf
  }

