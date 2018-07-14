generate.peaky <- function(n,m,s, n.peaks=1, level=5) {
  
  # Signal background
  x <- rlnorm(n,m,s)
  
  # Generate peaks, by first assigning their location, then their level
  peak.pos <- sample.int(n,n.peaks)
  x.std <- sd(x)
  peak.lev <- level*x.std
  x[peak.pos] <- peak.lev
  
  # Write and yield back data
  write.csv(x,file="peaky.csv",row.names=FALSE)
  return(x)
  
}


exceedances <- function(n, l.mu, l.sigma) {
  
  # Build the log-normal sample using the parameters specified
  x <- rlnorm(n, l.mu, l.sigma)
  
  # Build a normal sample having same size, and parameters as the series 'x'
  y <- rnorm(n, mean(x), sd(x))

  # Count exceedances in both cases, using a same increasing threshold
  x.exc <- numeric(10)
  y.exc <- numeric(10)
  for(s in 1:10) {
    x.exc[s] <- sum(x-mean(x) > s*sd(x))
    y.exc[s] <- sum(y-mean(x) > s*sd(x))
  }
  out <- data.frame(x.exc = x.exc, y.exc = y.exc)
  return(out)
  
}

show.peaky <- function() {
  d <- read.csv("peaky.csv")
  png(file="peaky.png", width=800, height=600)
  plot(d$x, type="l", xlab="Time (s)", ylab="Simulated C (mmol/mol)")
  dev.off()
}

get.peaks <- function(level) {
  f.name <- sprintf("peaky.%2.2d.csv", level)
  d <- read.csv(f.name)
  peaks <- which(d$PeakType > 0)
  e <- d[peaks,]
  fig.name <- sprintf("peaky.%2.2d.png", level)
  png(file=fig.name, width=800, height=600)
  plot(d$Index, d$Value, type="l", xlab="Time (s)", ylab="Simulated C")
  lines(d$Index, d$Avg.Filter+d$Std.Filter, lwd=2, col="red")
  lines(d$Index, d$Avg.Filter+d$Std.Filter*level, lwd=1, col="pink")
  points(e$Index, e$Value, col="red", pch=20, cex=1.5)
  dev.off()
}

get.peaks.0 <- function(level) {
  f.name <- sprintf("peaky.beta0.%2.2d.csv", level)
  d <- read.csv(f.name)
  peaks <- which(d$PeakType > 0)
  e <- d[peaks,]
  fig.name <- sprintf("peaky.beta0.%2.2d.png", level)
  png(file=fig.name, width=800, height=600)
  plot(d$Index, d$Value, type="l", xlab="Time (s)", ylab="Simulated C")
  lines(d$Index, d$Avg.Filter+d$Std.Filter, lwd=2, col="red")
  lines(d$Index, d$Avg.Filter+d$Std.Filter*level, lwd=1, col="pink")
  points(e$Index, e$Value, col="red", pch=20, cex=1.5)
  dev.off()
}

get.peaks.1 <- function(level) {
  f.name <- sprintf("peaky.beta1.%2.2d.csv", level)
  d <- read.csv(f.name)
  peaks <- which(d$PeakType > 0)
  e <- d[peaks,]
  fig.name <- sprintf("peaky.beta1.%2.2d.png", level)
  png(file=fig.name, width=800, height=600)
  plot(d$Index, d$Value, type="l", xlab="Time (s)", ylab="Simulated C")
  lines(d$Index, d$Avg.Filter+d$Std.Filter, lwd=2, col="red")
  lines(d$Index, d$Avg.Filter+d$Std.Filter*level, lwd=1, col="pink")
  points(e$Index, e$Value, col="red", pch=20, cex=1.5)
  dev.off()
}

make.plots <- function() {
  get.peaks(3)
  get.peaks(10)
  get.peaks(20)
  get.peaks(30)
  get.peaks(40)
  get.peaks(50)
  get.peaks.0(3)
  get.peaks.0(10)
  get.peaks.0(20)
  get.peaks.0(30)
  get.peaks.0(40)
  get.peaks.0(50)
  get.peaks.1(3)
  get.peaks.1(10)
  get.peaks.1(20)
  get.peaks.1(30)
  get.peaks.1(40)
  get.peaks.1(50)
}
