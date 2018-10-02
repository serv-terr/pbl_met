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

uu <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$uu
  new  <- e$uu
  plot(test,new,xlab="SonicLib",ylab="pbl_met",cex=0.2,main="UU (m2/s2)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}

vv <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$vv
  new  <- e$vv
  plot(test,new,xlab="SonicLib",ylab="pbl_met",cex=0.2,main="VV (m2/s2)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}

ww <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$ww
  new  <- e$ww
  plot(test,new,xlab="SonicLib",ylab="pbl_met",cex=0.2,main="WW (m2/s2)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}

uv <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$uv
  new  <- e$uv
  plot(test,new,xlab="SonicLib",ylab="pbl_met",cex=0.2,main="UV (m2/s2)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}

uw <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$uw
  new  <- e$uw
  plot(test,new,xlab="SonicLib",ylab="pbl_met",cex=0.2,main="UW (m2/s2)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}

vw <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$vw
  new  <- e$vw
  plot(test,new,xlab="SonicLib",ylab="pbl_met",cex=0.2,main="VW (m2/s2)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}

phi <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$phi
  new  <- e$phi
  plot(test,new,xlab="SonicLib",ylab="pbl_met,cex=0.2",main="phi (rad)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}

theta <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$theta
  new  <- e$theta
  plot(test,new,xlab="SonicLib",ylab="pbl_met",cex=0.2,main="theta (rad)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}

u.star <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$u.star
  new  <- e$u.star
  plot(test,new,xlab="SonicLib",ylab="pbl_met",cex=0.2,main="u* (m/s)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}
