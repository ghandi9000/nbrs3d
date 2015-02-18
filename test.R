### test.R --- 
## Filename: test.R
## Description: 
## Author: Noah Peart
## Created: Mon Feb 16 20:21:32 2015 (-0500)
## Last-Updated: Wed Feb 18 13:51:08 2015 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/neighborhoods/surround/rewrite/create_test.R")
source("~/work/gis/read-sky.R")  # read GIS skyline data
source("~/work/nbrs3d/vis.R")  # visualization function

## Using uniform x,y
ps <- as.matrix(expand.grid(x = -1.5:1.5, y = -1.5:1.5, z = 0, h = 1))
theta_s <- 45
theta_a <- 90
ps[,3] <- zvals(ps, theta_a, theta_s, x_offset = 315)
ps <- data.frame(ps)
ps$dbh <- runif(nrow(ps), 5, 15)
ps$shape <- sample(c("cone", "ellipse"), nrow(ps), replace=T)
ps$ht <- runif(nrow(ps), 2, 15)
ps$crdepth <- runif(1, 0.3, 0.95) * (ps$ht)
ps$crarea <- pi*(ps$crdepth/2)^2 + runif(1)

plot3d(xyz.coords(ps[,c("x","y","z")]), type = "s")
grid3d(side = "z")
planes3d(c(0,0,1), col="light green", alpha = 0.3)
abclines3d(0, a = diag(3))

library(plotrix)
poles <- cart2pol(ps[,1], ps[,2])
par(mfrow = c(1,2))
polar.plot(poles[,1], poles[,2] * 180/pi, main = "x-y distance")
polar.plot(ps[3,] + max(ps[,3]), poles[,2] * 180/pi, main = "z-values")

## Test plot
p <- 5  # plot number
yr <- 86
dat <- read.csv("~/work/data/moose/moose-long.csv")
tst <- dat[dat$pplot == p & dat$time == yr & dat$stat == "ALIVE", ] 
tst <- tst[complete.cases(tst[,c("x","y","z","dbh","ht","crdepth","crarea")]), ]
theta_a <- unique(tst$asp)
theta_s <- unique(tst$slope)
sky <- skylines[[as.character(p)]]

## Shapes for different species
soft <- c("ABBA", "PIRU")
tst$shape <- ifelse(tst$spec %in% soft, "cone", "ellipse")

library(plotrix)
polar.plot(sky$ZENITH_ANG, sky$HORIZ_ANG, main="Horizontal angle, radius is zenith angle")

poles <- cart2pol(x = tst$x, y = tst$y)
par(mfrow = c(1,2))
polar.plot(poles[,1], poles[,2] * 180/pi, main = "x-y distance")
polar.plot(tst$z, poles[,2] * 180/pi, main = "z-values")

## tst <- matrix(c(1,0,0,0,0.5,0,0,0,0.5),3,3)
## ell <- ellipse3d(tst, centre = c(1,1,1))
## shade3d(ell, col = "green")
## decorate3d()

## writeWebGL()
