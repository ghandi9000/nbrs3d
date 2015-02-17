### shapes.R --- 
## Filename: shapes.R
## Description: Some shapes for rgl plotting stuff
## Author: Noah Peart
## Created: Fri Feb 13 16:03:55 2015 (-0500)
## Last-Updated: Mon Feb 16 17:33:20 2015 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/functions/functions-coordinates.R")
require(rgl)

cylinder <- function(center, radius, height, col = "brown", wire=TRUE, ...) {
    def <- 10  # definition of rendering
    center <- cbind(center[1], center[2], c(center[3], center[3] + height))
    e2 <- cbind(1, 0, 0)
    cyl <- cylinder3d(center, sides = 8, radius = radius, e2=e2, ...)
    if (wire) wire3d(cyl, add=T, ...)
    else shade3d(cyl, add = T, ...)
}

## from rgl demo
ellipsoid3d <- function(rx=1, ry=1, rz=1, n=30, ctr=c(0,0,0),
                        qmesh=FALSE,
                        trans = par3d("userMatrix"), ...) {
    if (missing(trans) && !rgl.cur()) trans <- diag(4)
    degvec <- seq(0, 2*pi, length=n)
    ecoord2 <- function(p) {
        c(rx*cos(p[1])*sin(p[2]), ry*sin(p[1])*sin(p[2]), rz*cos(p[2])) }
    v <- apply(expand.grid(degvec, degvec), 1, ecoord2)
    if (qmesh) v <- rbind(v, rep(1, ncol(v))) ## homogeneous
    e <- expand.grid(1:(n-1), 1:n)
    i1 <- apply(e, 1, function(z) z[1]+n*(z[2]-1))
    i2 <- i1+1
    i3 <- (i1+n-1) %% n^2 + 1
    i4 <- (i2+n-1) %% n^2 + 1
    i <- rbind(i1, i2, i4, i3)
    if (!qmesh)
        quads3d(v[1,i], v[2,i], v[3,i], ...)
    else return(rotate3d(qmesh3d(v, i, material=...), matrix=trans))
}

cone3d <- function(base=c(0,0,0), tip=c(0,0,1), rad=1, n=30, draw.base=TRUE, qmesh=FALSE,
                   trans = par3d("userMatrix"), ...) {
    ax <- tip-base
    if (missing(trans) && !rgl.cur()) trans <- diag(4)
    ## is there a better way?
    if (ax[1] != 0) {
        p1 <- c(-ax[2]/ax[1],1,0)
        p1 <- p1/sqrt(sum(p1^2))
        if (p1[1]!=0) {
            p2 <- c(-p1[2]/p1[1],1,0)
            p2[3] <- -sum(p2*ax)
            p2 <- p2/sqrt(sum(p2^2))
        } else {
            p2 <- c(0,0,1)
        }
    } else if (ax[2]!=0) {
        p1 <- c(0,-ax[3]/ax[2],1)
        p1 <- p1/sqrt(sum(p1^2))
        if (p1[1]!=0) {
            p2 <- c(0,-p1[3]/p1[2],1)
            p2[3] <- -sum(p2*ax)
            p2 <- p2/sqrt(sum(p2^2))
        } else {
            p2 <- c(1,0,0)
        }
    } else {
        p1 <- c(0,1,0); p2 <- c(1,0,0)
    }
    degvec <- seq(0,2*pi,length=n+1)[-1]
    ecoord2 <- function(theta) {
        base+rad*(cos(theta)*p1+sin(theta)*p2)
    }
    i <- rbind(1:n, c(2:n,1), rep(n+1,n))
    v <- cbind(sapply(degvec, ecoord2), tip)
    if (qmesh) 
        ## minor kluge for quads -- draw tip twice
        i <- rbind(i, rep(n+1,n))
    if (draw.base) {
        v <- cbind(v,base)
        i.x <- rbind(c(2:n,1),1:n,rep(n+2,n))
        if (qmesh)  ## add base twice
            i.x <-  rbind(i.x,rep(n+2,n))
        i <- cbind(i,i.x)
    }
    if (qmesh) v <- rbind(v,rep(1,ncol(v))) ## homogeneous
    if (!qmesh)
        triangles3d(v[1,i],v[2,i],v[3,i],...)
    else
        return(rotate3d(qmesh3d(v,i,material=...), matrix=trans))
}

## Unit cone
.cone <- cone3d(qmesh = T, trans = diag(4))

cone <- function(x=0, y=0, z=0, height=1, rad=1, ...) {
    res <- translate3d(scale3d(.cone, rad, rad, height), x, y, z)
    return( res )
}

## unit sphere
.sphere <- ellipsoid3d(qmesh = T, trans = diag(4))

ellipsoid <- function(x=0, y=0, z=0, a=1, b=1, c=1, ...) {
    res <- translate3d(scale3d(.sphere, a, b, c), x, y, z)
    return( res )
}

## http://r.789695.n4.nabble.com/rgl-draw-multiple-ellipsoids-td3249157.html
## apply transformations to the unit sphere
rgl.ellipsoid2 <- function(x=0, y=0, z=0, a = 1, b=1, c=1, phi=0, theta=0, psi=0,
                            subdivide = 3, smooth = TRUE, ...) {
    result <- scale3d(.sphere, a, b, c)
    rotM <- euler(phi, theta, psi)
    result <- rotate3d(result, matrix=rotM)
    result <- translate3d(result, x, y, z)
    invisible(result)
}

## loop over the specification of a cluster (matrices nx3)
rgl.ellipsoids2 <- function(positions, sizes, angles,...) {
    N <- NROW(positions)
    ll <- lapply(seq(1, N), function(ii)
        rgl.ellipsoid2(positions[ii,1],positions[ii,2],positions[ii,3],
                  sizes[ii,1],sizes[ii,2],sizes[ii,3],
                  angles[ii,1],angles[ii,2],angles[ii,3], ...))
    shapelist3d(ll,...)
}

## positions <- matrix(c(1,1,1), ncol = 3)
## sizes <- matrix(c(1,1,2), ncol = 3)
## angles <- matrix(c(0, 0, 0), ncol = 3)
## ellipsoids(positions, sizes, angles)
## axes3d()

