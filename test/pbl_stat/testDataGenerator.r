# Build and return a data series composed by a sine-like deterministic part
# overplapped by a normal noise.
#
# This is useful to compose tables having known statistical properties.
#
noisy.series <- function(num.points, noise.std.dev, amplitude=0, delay=0) {
  
  # Compute the deterministic part
  t.0 <- 0
  t.1 <- 2*pi
  t <- seq(from=t.0, to=t.1, length.out=num.points)
  s <- amplitude*sin(t-delay)
  
  # Compute noise, as normally distributed perturbation
  n <- rnorm(num.points, 0, noise.std.dev)
  
  # Compose the two parts, and yield them to users
  y <- s + n
  return(y)
  
}


pepper.with.gaps <- function(series, percent.gaps=1.0) {
  n <- length(series)
  gaps <- floor(n * percent.gaps/100)
  if(gaps <= 0) {
    
    print("Warning: gap fraction is too small, leaving the data series unchanged")
    
  }
  else {
    
    # Compute the indices of elementsto change into gaps
    indices <- 1:n
    gap.positions <- sample(indices, size=gaps, replace=FALSE)
    
    # Actual change
    series[gap.positions] <- NA
    
  }
  
  # Yield result to users
  return(series)
  
}


prepare.gapless <- function() {
  num.points <- 16384
  noise.sd   <- 0.3
  amplitude  <- 4
  delay      <- pi/3
  x          <- noisy.series(num.points, noise.std.dev = noise.sd, amplitude = amplitude, delay = 0.0)
  y          <- noisy.series(num.points, noise.std.dev = noise.sd, amplitude = amplitude, delay = delay)
  d <- data.frame(x=x,y=y)
  return(d)
}


prepare.with.gaps <- function() {
  num.points <- 16384
  noise.sd   <- 2.0
  amplitude  <- 4
  delay      <- pi/3
  x          <- noisy.series(num.points, noise.std.dev = noise.sd, amplitude = amplitude, delay = 0.0)
  x          <- pepper.with.gaps(x, 10)
  y          <- noisy.series(num.points, noise.std.dev = noise.sd, amplitude = amplitude, delay = delay)
  y          <- pepper.with.gaps(y, 20)
  d <- data.frame(x=x,y=y)
  return(d)
}


prepare.test.material <- function() {
  
  # Test block 1 on gapless series
  
  d <- prepare.gapless()
  write.csv(d, "gapless.csv", row.names=FALSE)
  
  fields <- c(
    "mean",
    "min",
    "max",
    "median",
    "p25",
    "p75",
    "sd",
    "var"
  )
  x <- c(
    mean(d$x, na.rm=TRUE),
    min(d$x, na.rm=TRUE),
    max(d$x, na.rm=TRUE),
    median(d$x, na.rm=TRUE),
    quantile(d$x, 0.25, na.rm=TRUE),
    quantile(d$x, 0.75, na.rm=TRUE),
    sd(d$x, na.rm=TRUE),
    var(d$x, na.rm=TRUE)
  )
  y <- c(
    mean(d$y, na.rm=TRUE),
    min(d$y, na.rm=TRUE),
    max(d$y, na.rm=TRUE),
    median(d$y, na.rm=TRUE),
    quantile(d$y, 0.25, na.rm=TRUE),
    quantile(d$y, 0.75, na.rm=TRUE),
    sd(d$y, na.rm=TRUE),
    var(d$y, na.rm=TRUE)
  )
  e <- data.frame(quantity=fields, x=x, y=y)
  write.csv(e, file="gapless_stat.csv", row.names=FALSE)
  
  png("gapless_acf_x.png", width=800, height=600)
  f <- acf(d$x, lag.max=15000, main="", xlab="Lag", ylab="acf")
  dev.off()
  write.csv(f$acf, file="gapless_acf_x.csv", row.names=FALSE)
  
  png("gapless_acf_y.png", width=800, height=600)
  f <- acf(d$y, lag.max=15000, main="", xlab="Lag", ylab="acf")
  dev.off()
  write.csv(f$acf, file="gapless_acf_y.csv", row.names=FALSE)
  
  png("gapless_ccf_xy.png", width=800, height=600)
  f <- ccf(d$x, d$y, lag.max=15000, main="", xlab="Lag", ylab="ccf")
  dev.off()
  write.csv(data.frame(lag=f$lag,ccf=f$acf), file="gapless_ccf_xy.csv", row.names=FALSE)
  
  png("gapless_time_x.png", width=800, height=600)
  plot(d$x, type="l", xlab="Index", ylab="value")
  dev.off()
  
  png("gapless_time_y.png", width=800, height=600)
  plot(d$y, type="l", xlab="Index", ylab="value")
  dev.off()
  
  # Test block 2 on with-gaps series
  
  d <- prepare.with.gaps()
  write.csv(d, "withgaps.csv", row.names=FALSE)
  
  fields <- c(
    "mean",
    "min",
    "max",
    "median",
    "p25",
    "p75",
    "sd",
    "var"
  )
  x <- c(
    mean(d$x, na.rm=TRUE),
    min(d$x, na.rm=TRUE),
    max(d$x, na.rm=TRUE),
    median(d$x, na.rm=TRUE),
    quantile(d$x, 0.25, na.rm=TRUE),
    quantile(d$x, 0.75, na.rm=TRUE),
    sd(d$x, na.rm=TRUE),
    var(d$x, na.rm=TRUE)
  )
  y <- c(
    mean(d$y, na.rm=TRUE),
    min(d$y, na.rm=TRUE),
    max(d$y, na.rm=TRUE),
    median(d$y, na.rm=TRUE),
    quantile(d$y, 0.25, na.rm=TRUE),
    quantile(d$y, 0.75, na.rm=TRUE),
    sd(d$y, na.rm=TRUE),
    var(d$y, na.rm=TRUE)
  )
  e <- data.frame(quantity=fields, x=x, y=y)
  write.csv(e, file="withgaps_stat.csv", row.names=FALSE)
  
  png("withgaps_acf_x.png", width=800, height=600)
  f <- acf(d$x, lag.max=15000, main="", xlab="Lag", ylab="acf", na.action = na.pass)
  dev.off()
  write.csv(f$acf, file="gapless_acf_x.csv", row.names=FALSE)
  
  png("withgaps_acf_y.png", width=800, height=600)
  f <- acf(d$y, lag.max=15000, main="", xlab="Lag", ylab="acf", na.action = na.pass)
  dev.off()
  write.csv(f$acf, file="gapless_acf_y.csv", row.names=FALSE)
  
  png("withgaps_ccf_xy.png", width=800, height=600)
  f <- ccf(d$x, d$y, lag.max=15000, main="", xlab="Lag", ylab="ccf", na.action = na.pass)
  dev.off()
  write.csv(data.frame(lag=f$lag,ccf=f$acf), file="withgaps_ccf_xy.csv", row.names=FALSE)
  
  png("withgaps_time_x.png", width=800, height=600)
  plot(d$x, type="l", xlab="Index", ylab="value")
  dev.off()
  
  png("withgaps_time_y.png", width=800, height=600)
  plot(d$y, type="l", xlab="Index", ylab="value")
  dev.off()
  
}


prepare.test.material()

