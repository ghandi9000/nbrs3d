## Run 3d forest model
laptop.directory <- "C:/Users/Noah/Dropbox/Shared/Noah/Datasets"
setwd(laptop.directory)
pp <- read.csv("canmod.csv")
setwd("C:/Users/Noah/Dropbox/Noah/3dForestModel")
source("ForestModel3dScript.R")


# test with plot 4/data from 1986
p4 <- subset(pp, PLOT==4 & !is.na(DBH86) & !is.na(BQUDX) & STAT86=="ALIVE")
	
p4$CH86 <- p4$HT86-p4$CRHT86
p4$CD86 <- sqrt(p4$CRAREA86/pi)
p4$C.SHAPE86 <- rep(NA,nrow(p4))
p4[p4$SPEC=="ABBA"|p4$SPEC=="PIRU",]$C.SHAPE86 <-"cone"
p4[p4$SPEC!="ABBA"&p4$SPEC!="PIRU",]$C.SHAPE86 <-"ellipsoid"
p4$C.COLOR86 <- rep(NA,nrow(p4))
p4[p4$SPEC=="ABBA"|p4$SPEC=="PIRU",]$C.COLOR86 <-"darkgreen"
p4[p4$SPEC!="ABBA"&p4$SPEC!="PIRU",]$C.COLOR86 <-"palegreen4"
p4$C.TRANS <- rep(NA,nrow(p4))
p4[p4$SPEC=="ABBA"|p4$SPEC=="PIRU",]$C.TRANS <-.5
p4[p4$SPEC!="ABBA"&p4$SPEC!="PIRU",]$C.TRANS <-.5
p4$baseZ <- rep(0,nrow(p4))

test <- p4[,c("BPCRDX","BPCRDY","baseZ","DBH86","HT86","CD86","CH86","C.SHAPE86","C.COLOR86",
	"C.TRANS")]
names(test)[1:10] <- c("baseX","baseY","baseZ","td","th","cd","ch","c.shape","c.color",
	"c.transparency")
forest.1 <- test
forest.1$td <- forest.1$td/100
# plot data
library(rgl)
open3d()
aspect3d("iso")
plot.trees(forest.1)
axes3d()

ticks.at<- c(seq(0,20,by=1)) # where the tickmarks should be
#pos means where the axis should start c(x,y,z)
axis3d('x--', at = ticks.at, pos=c(0,0,0), tick=T, labels=T)
axis3d('y--', at = ticks.at, pos=c(0,0,0), tick=T, labels=T)
axis3d('x+-', at = ticks.at, pos=c(0,20,0), tick=F, labels=F)
axis3d('y+-', at = ticks.at, pos=c(20,0,0), tick=F, labels=F)

## requires unique coordinate points
##sface(forest.1$baseX, forest.1$baseY, forest.1$baseZ, col= "darkkhaki", alpha= 0.4)


#### set up for all plots in 1986
p2 <- subset(pp, PLOT>3 & !is.na(DBH86) & !is.na(BQUDX) & STAT86=="ALIVE")	
p2$CH86 <- p2$HT86-p2$CRHT86
p2$CD86 <- sqrt(p2$CRAREA86/pi)
p2$C.SHAPE86 <- rep(NA,nrow(p2))
p2[p2$SPEC=="ABBA"|p2$SPEC=="PIRU",]$C.SHAPE86 <-"cone"
p2[p2$SPEC!="ABBA"&p2$SPEC!="PIRU",]$C.SHAPE86 <-"ellipsoid"
p2$C.COLOR86 <- rep(NA,nrow(p2))
p2[p2$SPEC=="ABBA"|p2$SPEC=="PIRU",]$C.COLOR86 <-"darkgreen"
p2[p2$SPEC!="ABBA"&p2$SPEC!="PIRU",]$C.COLOR86 <-"palegreen4"
p2$C.TRANS <- rep(NA,nrow(p2))
p2[p2$SPEC=="ABBA"|p2$SPEC=="PIRU",]$C.TRANS <-.5
p2[p2$SPEC!="ABBA"&p2$SPEC!="PIRU",]$C.TRANS <-.5
p2$baseZ <- rep(0,nrow(p2))
# Add slope column



###################################################################
#####			Choose Target Plot (subset p2)		#######
###################################################################
test <- subset(p2, PLOT==5)
yp <- rotate.loci(test["XCOORD"],test["YCOORD"],mean(pp[pp$PLOT==5,]$ASPRAD,na.rm=T))[[2]]
test$baseZ <- yp*tan(mean(test$SLOPERAD,na.rm=T))

test <- test[,c("BPCRDX","BPCRDY","baseZ","DBH86","HT86","CD86","CH86","C.SHAPE86","C.COLOR86",
	"C.TRANS")]
names(test)[1:10] <- c("baseX","baseY","baseZ","td","th","cd","ch","c.shape","c.color",
	"c.transparency")
test <- test[complete.cases(test),]

forest.1 <- test
forest.1$td <- forest.1$td/100
# plot data
library(rgl)
open3d()
aspect3d("iso")
plot.trees(forest.1)
axes3d()

ticks.at<- c(seq(0,20,by=1)) # where the tickmarks should be
#pos means where the axis should start c(x,y,z)
axis3d('x--', at = ticks.at, pos=c(0,0,0), tick=T, labels=T)
axis3d('y--', at = ticks.at, pos=c(0,0,0), tick=T, labels=T)
axis3d('x+-', at = ticks.at, pos=c(0,20,0), tick=F, labels=F)
axis3d('y+-', at = ticks.at, pos=c(20,0,0), tick=F, labels=F)

## requires unique coordinate points
##sface(forest.1$baseX, forest.1$baseY, forest.1$baseZ, col= "darkkhaki", alpha= 0.4)


























