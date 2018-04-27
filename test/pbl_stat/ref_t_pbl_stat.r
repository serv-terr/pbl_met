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

