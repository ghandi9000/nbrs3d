### vis.R --- 
## Filename: vis.R
## Description: Visualize sample neighborhood
## Author: Noah Peart
## Created: Wed Feb 11 16:45:43 2015 (-0500)
## Last-Updated: Fri Feb 13 16:03:45 2015 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/functions/functions-coordinates.R")
source("~/work/neighborhoods/surround/rewrite/create_test.R")

## Using uniform x,y
ps <- as.matrix(expand.grid(x = -1.5:1.5, y = -1.5:1.5, z = 0, h = 1))
theta_s <- 45
theta_a <- 135
ps[,3] <- zvals(ps, theta_a, theta_s)
ps <- data.frame(ps)
plot3d(xyz.coords(ps), type = "s")
grid3d(side = "z")
planes3d(c(0,0,1), col="light green", alpha = 0.3)
abclines3d(0, a = diag(3))
m <- matrix(c(cos(theta_a * pi/180 - pi/4), sin(theta_a * pi/180 - pi/4),
              tan(theta_s * pi/180)), ncol = 3)
abclines3d(0, a = m, col = "red", lwd = 4)

ps$dbh <- runif(nrow(ps), 5, 15)
ps$shape <- sample(c("cone", "ellipse"), nrow(ps), replace=T)
ps$ht <- runif(nrow(ps), 2, 15)
ps$crdepth <- runif(1, 0.3, 0.95) * (ps$ht)
ps$crarea <- pi*(ps$crdepth/2)^2 + runif(1)
    
## library(plotrix)
## poles <- cart2pol(ps[,1], ps[,2])
## par(mfrow = c(1,2))
## polar.plot(poles[,1], poles[,2] * 180/pi, main = "x-y distance")
## polar.plot(ps[3,] + max(ps[,3]), poles[,2] * 180/pi, main = "z-values")

## Test on plot 22, aspect of 270 (directly west)
dat <- read.csv("~/work/data/moose/moose-wide.csv")
tst <- dat[dat$pplot == 22, ]  # plot 22
tst <- tst[complete.cases(tst[,c("x","y")]), ]
theta_a <- unique(tst$asp)
theta_s <- unique(tst$slope)
poles <- cart2pol(x = tst$x, y = tst$y)
par(mfrow = c(1,2))
polar.plot(poles[,1], poles[,2] * 180/pi, main = "x-y distance")
polar.plot(tst$z, poles[,2] * 180/pi, main = "z-values")

library(rgl)
ellipse <- function(a, b, c, center, crwn_col = "darkgreen", crwn_trans = 0.8, ...) {
    mat <- (1/16) * diag(3)
    plot3d(ellipse3d(mat, level = 0.999, centre = center, scale = c(a,b,c), col = crwn_col,
                     alpha = crwn_trans), add = T, ...)
}

cylinder <- function(center, radius, height, ...) {
    def <- 10  # definition of rendering
    center <- cbind(center[1], center[2], c(center[3], center[3] + height))
    e2 <- cbind(1, 0, 0)
    cyl <- cylinder3d(center, sides = 8, radius = radius, e2=e2, ...)
    shade3d(cyl, col = "brown", alpha = 0.8, add = T,)
}

p3d <- function(plt) {
    ## Make 3D plot
    require(rgl)
    x <- plt[["x"]]
    y <- plt[["y"]]
    z <- plt[["z"]]
    t_rad <- sqrt(plt[["dbh"]]/pi/100)  # trunk radius
    tree_ht <- plt[["ht"]]  # height to top of tree
    cr_ht <- tree_ht - plt[["crdepth"]]  # height to base of crown
    crdepth <- plt[["crdepth"]]  # crown depth
    cr_rad <- sqrt(plt[["crarea"]]/pi)  # horiz. crown radius
    
    plot3d(x, y, z+tree_ht, type="n", box = F, xlab="", ylab = "", zlab = "")
    for (i in 1:nrow(plt)) {
        center <- c(x[i], y[i], z[i])
        cylinder(center, t_rad[i], cr_ht[i])  # draw trunk
        ellipse(cr_rad[i], cr_rad[i], crdepth[i]/2, center + c(0,0,tree_ht[i]-crdepth[i]/2))
    }
    planes3d(c(0,0,1), col = "lightgray", alpha = 0.5)  # plane through center of plot
    grid3d(side = "z")                              # grid on base
    ## Compass for orientation
    abclines3d(0, 0, min(z)+.01, a = matrix(c(1,-1,0,-1,-1,0,0,0,0), 3, 3), col = "lightblue", lwd=4)
    text3d(max(x)-0.1, c(min(y)+0.1, max(y)-0.1), min(z)+0.1,
           texts = c("North", "East"))
    title3d(main = "Sample Plot", xlab = "X", ylab = "Y", zlab = "Z", nticks = 10)

}

tst <- matrix(c(1,0,0,0,0.5,0,0,0,0.5),3,3)
ell <- ellipse3d(tst, centre = c(1,1,1))
shade3d(ell, col = "green")
decorate3d()

