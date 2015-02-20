### normal-ellipses.R --- 
## Filename: normal-ellipses.R
## Description: Ellipse formed on normal plane to point of view intersection with ellipsoid
## Author: Noah Peart
## Created: Thu Feb 19 17:51:22 2015 (-0500)
## Last-Updated: Thu Feb 19 20:20:38 2015 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/nbrs3d/vis.R")  # 3d visualization stuff (p3d, rgl shapes)
source("~/work/neighborhoods/surround/rewrite/pixel_matrix.R")

## draw_hood_full(nbrs = nbrs)  # 2D representation
## p3d(nbrs)  # 3D
## abclines3d(0, a = diag(4), col = "slategray")
## add_tree(targ, wire=TRUE)

## Draw 3D tangent lines
## add_tangents <- function(targ, nbr, ...) {
##     r <- min(c(sqrt(nbr[["crarea"]]/pi), nbr[["dist"]]))  # if crown of nbr obscures target, set to dist
##     d <- nbr$dist
##     theta1 <- asin(r/d)
##     theta <- theta1*2
## }

## Oblate ellipsoid centered (1, 1, 1), point of view from origin
## normal plane is simply <1,1,1>
## axes: a = b = 1, c = 2
open3d()
plot3d(0, xlab = "X", ylab = "Y", zlab = "Z",
       xlim = c(-3, 5), ylim = c(-3, 5), zlim = c(-3, 5))
grid3d(side = c("x-","y+","z-"))
abclines3d(0, a=diag(3), lwd=2)
wire3d(ellipsoid(1,1,1,1,1,2), alpha = 0.3, col = "darkgreen")

## Add normal line and plane
lines3d(x=c(0,1), y=c(0,1), z=c(0,1), col = "green", lwd=2)
planes3d(a=1, b=1, c=1, d = -3, alpha = 0.5)

## Plane parallel to position vector
open3d()
plot3d(0, xlab = "X", ylab = "Y", zlab = "Z",
       xlim = c(-3, 5), ylim = c(-3, 5), zlim = c(-3, 5))
grid3d(side = c("x-","y+","z-"))
abclines3d(0, a=diag(3), lwd=2)
wire3d(ellipsoid(1,2,1,1,1,2), alpha = 0.3, col = "darkgreen")

## Add normal line and plane
lines3d(x=c(0,1), y=c(0,2), z=c(0,1), col = "green", lwd=2)
planes3d(a=-2, b=1, c=0, alpha = 0.5)
