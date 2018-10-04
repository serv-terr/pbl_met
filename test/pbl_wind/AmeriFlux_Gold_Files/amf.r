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

dir <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$Dir
  new  <- e$dir
  plot(test,new,xlab="SonicLib",ylab="pbl_met",cex=0.2,main="Dir (° from N)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}

vel.chk <- function() {
  e <- get.pbm()
  test <- sqrt(e$u^2+e$v^2)
  new  <- e$vel
  plot(test,new,xlab="sqrt(u^2+v^2)",ylab="pbl_met",cex=0.2,main="Vel (m/s)")
  abline(0,1)
  out <- data.frame(component.vel=test, pbl_met=new)
  return(out)
}

vel.chk.soniclib.2 <- function() {
  e <- get.amf()
  test <- sqrt(e$u.avg^2+e$v.avg^2)
  new  <- e$resultant.vel
  plot(test,new,xlab="sqrt(u^2+v^2)",ylab="SonicLib",cex=0.2,main="Vel (m/s)")
  abline(0,1)
  out <- data.frame(component.vel=test, pbl_met=new)
  return(out)
}

vel.chk.soniclib <- function() {
  e <- get.amf()
  test <- sqrt(e$u.avg^2+e$v.avg^2)
  new  <- e$vel
  plot(test,new,xlab="sqrt(u^2+v^2)",ylab="SonicLib",cex=0.2,main="Vel (m/s)")
  abline(0,1)
  out <- data.frame(component.vel=test, soniclib=new)
  return(out)
}

vel <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$resultant.vel
  new  <- e$vel
  plot(test,new,xlab="SonicLib",ylab="pbl_met",cex=0.2,main="Vel (m/s)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}

u <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$u.avg
  new  <- e$u
  plot(test,new,xlab="SonicLib",ylab="pbl_met",cex=0.2,main="U (m/s)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}

v <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$v.avg
  new  <- e$v
  plot(test,new,xlab="SonicLib",ylab="pbl_met",cex=0.2,main="V (m/s)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
}

w <- function() {
  d <- get.amf()
  e <- get.pbm()
  test <- d$w.avg
  new  <- e$w
  plot(test,new,xlab="SonicLib",ylab="pbl_met",cex=0.2,main="W (m/s)")
  abline(0,1)
  out <- data.frame(soniclib=test, pbl_met=new)
  return(out)
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
