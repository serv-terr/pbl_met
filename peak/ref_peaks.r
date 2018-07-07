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