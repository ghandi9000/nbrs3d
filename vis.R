### vis.R --- 
## Filename: vis.R
## Description: Visualize sample neighborhood
## Author: Noah Peart
## Created: Wed Feb 11 16:45:43 2015 (-0500)
## Last-Updated: Wed Feb 18 20:09:07 2015 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/functions/functions-coordinates.R")
source("~/work/nbrs3d/shapes.R")                      # rgl shapes for trees

################################################################################
##
##                          Visualize 3D Trees/Plot
##
################################################################################
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
##     play3d(spin3d(), duration = 2.5)
}

## Add tree crown to 3D plot
add_tree <- function(tree, wire = FALSE, crwn_col = "darkred", cones = c("ABBA", "PIRU"),
                     alpha = 0.5, ...) {
    require(rgl)
    if (all(sapply(c(tree$x, tree$y, tree$z), is.null)))
        tree[, c("x", "y", "z")] <- c(0,0,0)  # target centered in plot
    cr_rad <- sqrt(tree$crarea/pi)
    if (tree$spec %in% cones)
        crwn <- with(tree, cone(x, y, z+ht-crdepth, height = crdepth, rad = cr_rad))
    else
        crwn <- with(tree, ellipsoid(x, y, z+ht-crdepth/2,
                                     a=cr_rad, b=cr_rad, c=crdepth/2))
    if (!wire)
        shade3d(crwn, col = crwn_col, alpha = alpha, ...)
    else
        wire3d(crwn, col = crwn_col, alpha = alpha, ...)
}

