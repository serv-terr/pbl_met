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
