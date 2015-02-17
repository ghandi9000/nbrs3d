### vis.R --- 
## Filename: vis.R
## Description: Visualize sample neighborhood
## Author: Noah Peart
## Created: Wed Feb 11 16:45:43 2015 (-0500)
## Last-Updated: Mon Feb 16 21:20:43 2015 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/functions/functions-coordinates.R")
source("~/work/neighborhoods/surround/rewrite/create_test.R")
source("~/work/nbrs3d/shapes.R")  # rgl shapes for trees
source("~/work/gis/read-sky.R")  # read GIS skyline data

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

## library(plotrix)
## poles <- cart2pol(ps[,1], ps[,2])
## par(mfrow = c(1,2))
## polar.plot(poles[,1], poles[,2] * 180/pi, main = "x-y distance")
## polar.plot(ps[3,] + max(ps[,3]), poles[,2] * 180/pi, main = "z-values")

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

## poles <- cart2pol(x = tst$x, y = tst$y)
## par(mfrow = c(1,2))
## polar.plot(poles[,1], poles[,2] * 180/pi, main = "x-y distance")
## polar.plot(tst$z, poles[,2] * 180/pi, main = "z-values")

p3d <- function(plt, crwn_col="darkgreen", crwn_wire=TRUE, trnk_col = "brown",
                trnk_wire=TRUE, alpha = 0.5, ...) {
    ## Make 3D plot
    soft <- c("ABBA", "PIRU")
    plt$shape <- ifelse(plt$spec %in% soft, "cone", "ellipse")
    plt <- plt[complete.cases(plt[,c("x","y","z","dbh","ht","crdepth","crarea")]), ]
    require(rgl)
    x <- plt[["x"]]
    y <- plt[["y"]]
    z <- plt[["z"]]
    t_rad <- sqrt(plt[["dbh"]]/pi/100)  # trunk radius
    tree_ht <- plt[["ht"]]  # height to top of tree
    cr_ht <- tree_ht - plt[["crdepth"]]  # height to base of crown
    crdepth <- plt[["crdepth"]]  # crown depth
    cr_rad <- sqrt(plt[["crarea"]]/pi)  # horiz. crown radius
    
#    plot3d(x, y, z+tree_ht, type="n", box = F, xlab="", ylab = "", zlab = "")
    for (i in 1:nrow(plt)) {
        center <- c(x[i], y[i], z[i])
        cylinder(center, t_rad[i], cr_ht[i], wire=trnk_wire, col = "brown")  # draw trunk
        if (plt[i, "shape"] == "cone")
            crwn <- cone(x[i], y[i], z[i]+cr_ht[i], height = crdepth[i], rad = cr_rad[i])
        else
            crwn <- ellipsoid(x[i], y[i], z[i]+cr_ht[i]+crdepth[i]/2, a=cr_rad[i], b=cr_rad[i], c=crdepth[i]/2)
        if (!crwn_wire)
            shade3d(crwn, col = crwn_col, alpha = alpha, ...)
        else
            wire3d(crwn, col = crwn_col, alpha = alpha, ...)
    }
    planes3d(c(0,0,1), col = "lightgray", alpha = 0.5)  # plane through center of plot
    grid3d(side = "z")                              # grid on base
    ## Compass for orientation
    abclines3d(0, 0, min(z)+.01, a = matrix(c(1,-1,0,-1,-1,0,0,0,0), 3, 3), col = "lightblue", lwd=4)
    text3d(max(x)-0.1, c(min(y)+0.1, max(y)-0.1), min(z)+0.1,
           texts = c("North", "East"))
    tt <- ifelse(!is.na(plt$pplot), unique(plt$pplot), "Sample")
    title3d(main = paste("Plot:", tt), xlab = "X", ylab = "Y", zlab = "Z", nticks = 10)
    axes3d()
    play3d(spin3d(), duration = 2.5)
}

## tst <- matrix(c(1,0,0,0,0.5,0,0,0,0.5),3,3)
## ell <- ellipse3d(tst, centre = c(1,1,1))
## shade3d(ell, col = "green")
## decorate3d()

