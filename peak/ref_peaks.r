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
  plot(d$Index, d$Value, type="l", xlab="Time (s)", ylab="Simulated C (mmol/mol)")
  points(e$Index, e$Value, col="red", pch=20, cex=1.5)
  dev.off()
}
