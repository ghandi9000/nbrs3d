### normal-ellipses.R --- 
## Filename: normal-ellipses.R
## Description: Ellipse formed on normal plane to point of view intersection with ellipsoid
## Author: Noah Peart
## Created: Thu Feb 19 17:51:22 2015 (-0500)
## Last-Updated: Thu Mar 12 15:59:57 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/nbrs3d/vis.R")  # 3d visualization stuff (p3d, rgl shapes)
source("~/work/neighborhoods/surround/rewrite/pixel_matrix.R")

## draw_hood_full(nbrs = nbrs)  # 2D representation
## p3d(nbrs)  # 3D
## abclines3d(0, a = diag(4), col = "slategray")
## add_tree(targ, wire=TRUE)

################################################################################
##
##                                2D ellipses
##
################################################################################
## Go over some basic linear transformations
a <- 3
b <- 1
xr <- c(-2, 10)
yr <- c(-3, 6)
len = 500

## On origin
xs <- a*cos(seq(0, 2*pi, length = len))
ys <- b*sin(seq(0, 2*pi, length = len))
ps <- cbind(xs=xs, ys=ys)
plot(xy.coords(ps), xlim = xr, ylim = yr, type = "l", main="2D ellipse transforms",
     xlab = "x", ylab = "y")
abline(h=0, v=0)
grid()

## Translated
center = matrix(c(6, 3), len, 2, byrow = T)
pt <- ps + center
points(xy.coords(pt), type = "l", xlim = xr, ylim = yr, col = "red")
arrows(x0 = a, y0 = 0, x1 = center[1,1]+a, y1 = center[1,2], col = "red")

## increase a-dim
inc <- matrix(c(1.5, 0, 0, 1), 2, 2)
pinc <- t( inc %*% t(pt - center) ) + center
points(xy.coords(pinc), type = "l", xlim = xr, ylim = yr, col = "turquoise")
arrows(x0=pt[1,1], y0=pt[1,2], x1=pinc[1,1], y1=pinc[1,2], col = "turquoise")

## Rotate
theta <- pi/4
rmat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
pr <- t( rmat %*% t(pinc-center) ) + center
points(xy.coords(pr), type = "l", xlim = xr, ylim = yr, main = "Rotated", col = "blue")
arrows(x0=pinc[1,1], y0=pinc[1,2], x1=pr[1,1], y1=pr[1,2], col = "blue")

## Translate back to y-axis
center2 <- matrix(c(0, 3), len, 2, byrow = T)
pt2 <- pr + center2 - center
points(xy.coords(pt2), type = "l", xlim = xr, ylim = yr, col = "purple")
arrows(x0=pr[1,1], y0=pr[1,2], x1=pt2[1,1], y1=pt2[1,2], col = "purple")

## Rotate back to starting orientation
rmat2 <- t(rmat)  # inverse rotation same as transpose
pr2 <- t( rmat2 %*% t(pt2 - center2) ) + center2
points(xy.coords(pr2), type = "l", xlim = xr, ylim = yr, col = "green")
arrows(x0=pt2[1,1], y0=pt2[1,2], x1=pr2[1,1], y1=pr2[1,2], col = "green")

## shrink a-axis
shrink <- matrix(c(0.5, 0, 0, 1), 2, 2)
psh <- t( shrink %*% t(pr2) )
points(xy.coords(psh), type = "l", xlim = xr, ylim = yr, col = "turquoise")
arrows(x0=pr2[1,1], y0=pr2[1,2], x1=psh[1,1], y1=psh[1,2], col = "turquoise")

## center new shape
pt3 <- psh - center2
points(xy.coords(pt3), type = "l", xlim = xr, ylim = yr, col = "orange")
arrows(x0=psh[1,1], y0=psh[1,2], x1=pt3[1,1], y1=pt3[1,2], col = "orange")

################################################################################
##
##                             2D Tangent lines
##
################################################################################
## Translated rotated ellipse
a <- 3
b <- 1
theta <- 0  # rotation from +x axis
xc <- 3  # center of ellipse
yc <- 0
len = 500

ps <- cbind(xs=a*cos(seq(0, 2*pi, length=len)), ys=b*sin(seq(0, 2*pi, length=len)))
rmat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
ps <- t( rmat %*% t(ps) ) + matrix(c(xc, yc), len, 2, byrow = TRUE)
plot(xy.coords(ps), type = "l", main="2D ellipse tangents", xlab = "x", ylab = "y",
     ylim = c(min(0, min(ps[,2])), max(1, ps[,2])),
     xlim = c(min(0, min(ps[,1])), max(1, ps[,1])))
abline(h=0, v=0)
grid()

ms <- ell_tang2d(a, b, xc, yc, theta)  # tangent slopes
abline(0, ms[1], lty=2, col = "red")
abline(0, b=ms[2], lty=2, col = "red")

## https://surreyspacecentre.files.wordpress.com/2013/01/perspective-projection-of-a-spheroid-onto-an-image-plane.pdf
ell_grad2d <- function(a, b, xc, yc, theta) {
    return( 1/2*(xc**2+yc**2)*(a**2+b**2) - 1/2*(xc**2-yc**2)*(a**2-b**2)*cos(2*theta) -
           xc*yc*(a**2-b**2)*sin(2*theta)-a**2*b**2 )
}

ell_tang2d <- function(a, b, xc, yc, theta) {
    grads <-  c(-1,1)*2*sqrt(ell_grad2d(a,b,xc,yc,theta))
    return( (2*xc*yc - (a**2 - b**2)*sin(2*theta) + grads)/
               (2*xc**2-(a**2 + b**2) - (a**2 - b**2)*cos(2*theta)) )
}

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
a <- 1
b <- 1
c <- 3
xc <- 4
yc <- 3
zc <- 1

open3d()
plot3d(0, xlab = "X", ylab = "Y", zlab = "Z",
       xlim = c(-3, 5), ylim = c(-3, 5), zlim = c(-3, 5))
grid3d(side = c("x-","y+","z-"))
abclines3d(0, a=diag(3), lwd=2)
wire3d(ellipsoid(xc, yc, zc, a, b, c), alpha = 1, col = "darkred")

## Add unit sphere
.sphere <- ellipsoid3d(qmesh = T, trans = diag(4))
wire3d(.sphere, col='blue', alpha=0.6)

## Add normal line and plane
lines3d(x=c(0,1), y=c(0,1), z=c(0,1), col = "green", lwd=2)
planes3d(a=1, b=1, c=1, d = -3, alpha = 0.6)


## Plane parallel to position vector
open3d()
plot3d(0, xlab = "X", ylab = "Y", zlab = "Z",
       xlim = c(-3, 5), ylim = c(-3, 5), zlim = c(-3, 5))
grid3d(side = c("x-","y+","z-"))
abclines3d(0, a=diag(3), lwd=2)
wire3d(ellipsoid(1,2,1,1,1,2), alpha = 0.3, col = "darkgreen")

## Add normal line and parallel plane
lines3d(x=c(0,1), y=c(0,2), z=c(0,1), col = "green", lwd=2)
planes3d(a=-2, b=1, c=0, alpha = 0.5)

planes3d()

## Tangent Points
a <- 3
b <- 1
theta <- 0  # rotation from +x axis
xc <- 3  # center of ellipse
yc <- 0
len = 500

ps <- cbind(xs=a*cos(seq(0, 2*pi, length=len)), ys=b*sin(seq(0, 2*pi, length=len)))
rmat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
ps <- t( rmat %*% t(ps) ) + matrix(c(xc, yc), len, 2, byrow = TRUE)
plot(xy.coords(ps), type = "l", main="2D ellipse tangents", xlab = "x", ylab = "y",
     ylim = c(min(0, min(ps[,2])), max(1, ps[,2])),
     xlim = c(min(0, min(ps[,1])), max(1, ps[,1])))
abline(h=0, v=0)
grid()

ms <- ell_tang2d(a, b, xc, yc, theta)  # tangent slopes
abline(0, ms[1], lty=2, col = "red")
abline(0, b=ms[2], lty=2, col = "red")

