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

#######################################
# Reference case for cross-covariance #
#######################################

test.ccov <- function() {
  x <- c(1.,2.,3.,4.,5.,4.,3.,2.,1.)
  y <- c(3.,4.,5.,4.,3.,2.,1.,1.,1.)
  d <- ccf(x,y,lag.max=4,type="covariance")
  return(d)
}


test.ccor <- function() {
  x <- c(1.,2.,3.,4.,5.,4.,3.,2.,1.)
  y <- c(3.,4.,5.,4.,3.,2.,1.,1.,1.)
  d <- ccf(x,y,lag.max=4,type="correlation")
  return(d)
}

#############################################
# Test case for Eulerian decorrelation time #
#############################################

# Generate a negative-exponentially correlated Gaussian process
# using the algorithm described in [Deserno, web1]
neg.exp.proc <- function(n, tau) {
  f <- exp(-1/tau)
  g <- rnorm(n,0,1)
  r <- numeric(n)
  r[1] <- g[1]
  for(i in 2:n) {
    r[i] <- f*r[i-1] + sqrt(1.0-f^2)*g[i]
  }
  return(r)
}

# Predispose and write the test set
test.eulerian.time <- function() {
  v <- neg.exp.proc(1024, 7.0)
  write.csv(v, file="euler.dat", row.names=FALSE)
  ac <- acf(v, lag.max = 50, type="cor")$acf
  write.csv(ac, file="euler.ref", row.names=FALSE)
  idx <- 0:50
  pos <- which(ac > 0)
  ac.p <- log(ac[pos])
  id.p <- idx[pos]
  plot(id.p, ac.p)
  l <- lm(ac.p~id.p)
  print(summary(l))
  return(l)
}
