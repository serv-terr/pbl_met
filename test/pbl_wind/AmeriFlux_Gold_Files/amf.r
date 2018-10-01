get.amf <- function() {
  d <- read.csv("amf.csv", stringsAsFactors = FALSE)
  d$t.stamp <- as.POSIXct(d$t.stamp, tz="UTC")
  return(d)
}

get.pbm <- function() {
  d <- read.csv("test/AmerifluxGoldFiles/AF_20150414.csv", stringsAsFactors = FALSE)
  e <- read.csv("test/AmerifluxGoldFiles/AF_20150630.csv", stringsAsFactors = FALSE)
  d <- rbind(d,e)
  d$date <- as.POSIXct(d$date, tz="UTC")
  return(d)
}

phi <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$phi
  new  <- e$phi
  plot(test,new,xlab="SonicLib",ylab="pbl_met",main="phi (rad)")
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}

u.star <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$u.star
  new  <- e$u.star
  plot(test,new,xlab="SonicLib",ylab="pbl_met",main="u* (m/s)")
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}
