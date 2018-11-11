get.data <- function() {
  d <- read.csv("rgSimple.csv", stringsAsFactors = FALSE)
  d$Time.Stamp <- as.POSIXct(d$Time.Stamp)
  return(d)
}

day.sums <- function(d) {
  tm <- as.integer(d$Time.Stamp) %/% (24*3600)
  date   <- aggregate(d$Time.Stamp, by=list(tm), FUN=min)$x
  rg.min <- aggregate(d$Rg.Min, by=list(tm), FUN=sum)$x
  rg.max <- aggregate(d$Rg.Max, by=list(tm), FUN=sum)$x
  out <- data.frame(date=date, rg.min=rg.min, rg.max=rg.max)
  return(out)
}
