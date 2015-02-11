### vis.R --- 
## Filename: vis.R
## Description: Visualize sample neighborhood
## Author: Noah Peart
## Created: Wed Feb 11 16:45:43 2015 (-0500)
## Last-Updated: Wed Feb 11 17:57:51 2015 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/functions/functions-coordinates.R")
source("~/work/neighborhoods/surround/rewrite/create_test.R")

## 3d visualization of plot
library(rgl)
plot3d(nbrs$x, nbrs$y, nbrs$z, type="s", col=as.numeric(nbrs$spec))

open3d()
layout3d(matrix(1:16, 4,4), heights = c(1,3,1,3))
text3d(0,0,0,"tetrahedron3d"); next3d()


library(plotrix)
coords <- cart2pol(nbrs$x, nbrs$y)
polar.plot(nbrs$z, coords[,2] * 180/pi, start = -45)
