### 3d/2d R script for forest modeling
                                        # 3dforest script by Jan Hrcek
                                        # janhrcek@gmail.com
                                        # functions #

disc.1 <- function(render.det) {
    angle <- 2*pi/render.det
    first.x<- 1*cos(angle) - 0*sin(angle)
    first.y<- 1*sin(angle) + 0*cos(angle)
    x<-c(0,1, first.x)
    y<- c(0,0, first.y)
    xx<-c(0)
    yy<-c(0)
    xx.s<-c(0)
    yy.s<-c(0)
    for(i in 1:render.det) {
        x2<- x*cos(angle) - y*sin(angle)
        y2<- x*sin(angle) + y*cos(angle)
        x<-x2
        y<-y2
        j=i*3
        xx[[j-2]]<- x2[[1]]
        xx[[j-1]]<- x2[[2]]
        xx[[j]]<- x2[[3]]
        yy[[j-2]]<- y2[[1]]
        yy[[j-1]]<- y2[[2]]
        yy[[j]]<- y2[[3]]
                                        # and the short versions prepared for cylinder.1
        xx.s[[i]]<- x2[[2]]
        yy.s[[i]]<- y2[[2]]
    }
    out<-list(x=xx, y=yy, x.s=xx.s, y.s=yy.s)
    out
}
                                        # getting 3rd coord for triangle
                                        # angle <- pi/4.5
                                        # xx2<- 1*cos(angle) - 0*sin(angle)
                                        # yy2<- 1*sin(angle) + 0*cos(angle)

disc <- function(baseX, baseY, baseZ, d, h, c.color, c.transparency, disc.11, render.det, ...) {
    zz<- rep(h,render.det*3)
    triangles3d(disc.11$x*(d/2)+baseX, disc.11$y*(d/2)+baseY, zz+baseZ, add=T, col= c.color, alpha= c.transparency, ...)
}

cylinder.1 <- function(render.det, disc.11) {
                                        # compiles x and y coordinates for unit cylinder
                                        # order of taking numbers from disc.11 in both x and y
    a<-c(0, 1, 1, 0)
    aa<-c(0)
    for(i in 1:render.det) {
        a2<-a+i
        j=i*4
        aa[[j-3]]<- a2[[1]]
        aa[[j-2]]<- a2[[2]]
        aa[[j-1]]<- a2[[3]]
        aa[[j]]<- a2[[4]]
    }
                                        # we move round a circle, the last rectangle hast to end at 1
    aa[length(aa)-2]<-1
    aa[length(aa)-1]<-1
                                        # now take numbers from disc.11$x.s and disc.11$y.s according to [aa]
    xx<- disc.11$x.s[aa]
    yy<- disc.11$y.s[aa]
    out<-list(x=xx, y=yy)
    out
}

cylinder <- function(baseX, baseY, baseZ, d, h0, h, c.color, c.transparency, cylinder.11, render.det) {
    zz<- rep(c(h0,h0, h, h), render.det)
    quads3d(cylinder.11$x*(d/2)+baseX, cylinder.11$y*(d/2)+baseY, zz+baseZ, add=T, col= c.color, alpha= c.transparency)
}

cone <- function(baseX, baseY, baseZ, cd, th, ch, c.color, c.transparency, disc.11, render.det) {
    zz<- rep(c(th+ch,th,th),render.det)
    triangles3d(disc.11$x*(cd/2)+baseX, disc.11$y*(cd/2)+baseY, zz+baseZ, add=T, col= c.color, alpha= c.transparency)
}

revcone <- function(baseX, baseY, baseZ, cd, td, th, ch, c.color, c.transparency, disc.11, render.det) {
    zz<- rep(c(th,th+ch,th+ch),render.det)
    triangles3d(disc.11$x*(cd/2)+baseX, disc.11$y*(cd/2)+baseY, zz+baseZ, add=T, col= c.color, alpha= c.transparency)
}


ellipsoid <- function(baseX, baseY, baseZ, cd, th, ch, c.color, c.transparency) {
                                        # the ellipsoid size is not exact, just approximate
    x<-(1/16)*((cd/2)^2)
    y<-(1/16)*((cd/2)^2)
    z<-(1/16)*((ch/2)^2)
    matrix.a<- matrix(c(x,0,0,0,y,0,0,0,z), 3,3)
    plot3d(ellipse3d(matrix.a, level=0.999, centre = c(baseX, baseY, baseZ+th+ch/2)), col=c.color, alpha= c.transparency, add=T)
}

trunk <- function(baseX, baseY, baseZ, td, th, cylinder.11, render.det) {
    t.color<-"grey30"
                                        # trunk as a cylinder
    cylinder(baseX, baseY, baseZ, td, 0, th, t.color, 1, cylinder.11, render.det)
}

crown <- function(baseX, baseY, baseZ, td, th, cd, ch, c.shape, c.color, c.transparency, disc.11, cylinder.11, render.det) {

                                        # c.shape switch (cylinder, cone, ellipsoid, revcone)

    if (c.shape == "ellipsoid")
        ellipsoid(baseX, baseY, baseZ, cd, th, ch, c.color, c.transparency)

    else if(c.shape == "cone") {
        cone(baseX, baseY, baseZ, cd, th, ch, c.color, c.transparency, disc.11, render.det);
        disc(baseX, baseY, baseZ, cd, th, c.color, c.transparency, disc.11, render.det)
    }

    else if(c.shape == "revcone") {
        revcone(baseX, baseY, baseZ, cd, td, th, ch, c.color, c.transparency, disc.11, render.det);
        disc(baseX, baseY, baseZ, cd, ch+th, c.color, c.transparency, disc.11, render.det)
    }

    else {
        cylinder(baseX, baseY, baseZ, cd, th, th+ch, c.color, c.transparency, cylinder.11, render.det);
                                        # the top disc
        disc(baseX, baseY, baseZ, cd, ch+th, c.color, c.transparency, disc.11, render.det);
                                        # the bottom disc
        disc(baseX, baseY, baseZ, cd, th, c.color, c.transparency, disc.11, render.det)
    }
}

plot.tree <- function(x, disc.11, cylinder.11, render.det) {
    ## baseX, baseY, baseZ, td, th, cd, ch, c.shape, c.color, c.transparency
    ## the parameters have to be divided to numeric and character (they come as character into this function)
    n<-0
    ch<-0
    n[1:7]<-as.numeric(x[1:7])
    ch[1:3]<-as.character(x[8:10])
    trunk(n[1], n[2], n[3], n[4], n[5], cylinder.11, render.det)
    crown(n[1], n[2], n[3], n[4], n[5], n[6], n[7], ch[1], ch[2], ch[3], disc.11, cylinder.11, render.det)
}

plot.trees <- function(df) {
                                        # render.det is the detail used when rendering circular shapes, as they are apriximated
                                        # by triangles. 72 means that a circle is composed from 72 triangles.
                                        # It influences only cylinders (incl. trunks) and cones, not ellipsoids.
                                        # render.det has to be whole number. now it is global, can make it separately for trunk and crown
                                        # no need to be more than 360, that is really smooth. not much need to be less than 72 - very slight speedup
    render.det <- 72
    disc.11 <- disc.1(render.det)
    cylinder.11 <- cylinder.1(render.det, disc.11)
    apply(df, 1, plot.tree, disc.11, cylinder.11, render.det)
    dim(df)[1]
}

sface  <- function(baseX, baseY, baseZ, ...) {
                                        # function sface plots a surface triangulation from point coordinates.
                                        # the points can be at the base of trees, or anywhere else
    require(tripack)
    tri1<-tri.mesh(baseX, baseY)
    tri1.t<-triangles(tri1)
                                        #triangle node indexes: tri1.t[,1:3]
    x<-0
    for(i in 1:length(tri1.t[,1])) {
        j=i*3
        x[[j-2]]<- tri1.t[i,1]
        x[[j-1]]<- tri1.t[i,2]
        x[[j]]<- tri1.t[i,3]
    }
                                        #node coordinates (ordered by node index): tri1$x, tri1$y
    triangles3d(tri1$x[x] , tri1$y[x], baseZ[x], add=T, ...)
}

plot.discs <- function(df) {
                                        # can't use apply here - would have to make a bit different disc function (numeric vs. character)
                                        # disc in the height of top of the tree's crown
                                        # color function is dependent on c.color being character class
    render.det <- 72
    df$c.color<-as.character(df$c.color)
    df$c.shape<-as.character(df$c.shape)
    disc.11 <- disc.1(render.det)
    for (i in 1:dim(df)[1]) {
                                        # baseX, baseY, baseZ, td, th, cd, ch, c.shape, c.color, c.transparency, disc.11, render.det
                                        #df[i,1], df[i,2], df[i,3], df[i,4], df[i,5], df[i,6], df[i,7], df[i,8], df[i,9], df[i,10], disc.11, render.det
        disc(df[i,1], df[i,2], df[i,3], df[i,6], df[i,5]+ df[i,7], df[i,9], df[i,10], disc.11, render.det, lit=F)
    }
    view3d( theta = 0, phi = 0, fov=1)
}

                                        # end of functions #
