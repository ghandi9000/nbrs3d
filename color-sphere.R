### color-sphere.R --- 
## Filename: color-sphere.R
## Description: Coloring pixels of sphere
## Author: Noah Peart
## Created: Wed Feb 18 13:48:56 2015 (-0500)
## Last-Updated: Wed Feb 18 13:49:17 2015 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/gis/read-sky.R")

dat <- read.csv("~/work/data/moose/moose-long.csv")
p5 <- dat[dat$pplot == 5 & dat$time == 86 & dat$stat == "ALIVE", ]
unique(p5$asp)  # 180 aspect
unique(p5$slope)  # ~ 7 degree slope

sky <- skylines[["5"]]
size <- 50  # dimensions of matrices
inds <- floor(seq(1, nrow(sky), length =size))
theta_zen <- sky[inds, "ZENITH_ANG"]*pi/180
theta_az <- sky[inds, "HORIZ_ANG"]*pi/180

library(rgl)

## coloring sphere
zen <- matrix(seq(0, pi, length = size), size, size, byrow = TRUE)
az <- matrix(seq(0, 2*pi, length = size), size, size)
r <- 1
z <- r*cos(zen)
x <- r*sin(zen)*cos(az)
y <- r*sin(zen)*sin(az)

open3d()
persp3d(x, y, z, alpha = 0.3)
grid3d(side = "z")
abclines3d(0, a = diag(3))
## Horizon line
r <- 1
z2 <- r*cos(theta_zen)
x2 <- r*sin(theta_zen) * cos(theta_az)
y2 <- r*sin(theta_zen) * sin(theta_az)
xyz <- cbind(x2, y2, z2)
lines3d(xyz.coords(x2, y2, z2), lwd = 4, col = "wheat2")


library(plotrix)
r <- 90
z2 <- r*cos(sky$ZENITH_ANG*pi/180)
x2 <- r*sin(sky$ZENITH_ANG*pi/180) * cos(sky$HORIZ_ANG*pi/180)
y2 <- r*sin(sky$ZENITH_ANG*pi/180) * sin(sky$HORIZ_ANG*pi/180)
xyz <- cbind(x2, y2, z2)

polar.plot(sky$ZENITH_ANG, polar.pos = sky$HORIZ_ANG, radial.lim = c(0, 90),
           rp.type = "l")
abline(h = 0, v = 0)
