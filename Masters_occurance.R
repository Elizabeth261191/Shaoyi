#convert .txt files in .csv files
#convert any blank cells with na values

#load data
library(readr)
Acacia_shirleyi <- read_csv("C:/Users/etelford.IC.001/Dropbox/Shaoyi/Acacia_shirleyi.csv")
AS<-Acacia_shirleyi
names(AS)
AS<-AS[, c(133, 134)]


#creating an occurance map
install.packages("maptools")
#only use install.packages function once
library(maptools)
data(wrld_simpl)
p1 <- plot(wrld_simpl, axes=TRUE)
p1 <- box()
p1 <- points(AS$decimalLongitude, AS$decimalLatitude, col='red', pch=20, cex=0.75)



######potentially works

library(sp)
coordinates(VS) <- ~Long+Lat
crs(VS) <- crs(wrld_simpl)
class(VS)

class(wrld_simpl)
## [1] "SpatialPolygonsDataFrame"
## attr(,"package")
## [1] "sp"
ovr <- over(VS, wrld_simpl)

class(wrld_simpl)
head(ovr)
cntr <- ovr$NAME

plot(VS)
plot(wrld_simpl, add=T, border='blue', lwd=2)
points(acg[j, ], col='red', pch=20, cex=2)

library(raster)
r <- raster(VS)
res(r) <- 1
r <- extend(r, extent(r)+1)
acsel <- gridSample(VS, r, n=1)
p <- rasterToPolygons(r)
plot(p, border='gray')
points(VS)
# selected points in red
points(acsel, cex=1, col='red', pch='x')

file <- paste(system.file(package="dismo"), '/ex/vs_clean.csv', sep='')
VS_raster <- read.csv(file)




#using the climate data
library(dismo)
path <- file.path(system.file(package="dismo"), 'ex')
files <- list.files(path, pattern='grd$', full.names=TRUE )
files
predictors <- stack(files)
predictors

names(predictors)

plot(predictors)


library(maptools)
data(wrld_simpl)
file <- paste(system.file(package="dismo"), "/ex/VS_clean.csv", sep="")
VS <- read.table(file,  header=TRUE,  sep=',')
library(maptools)
data(wrld_simpl)
file <- paste(system.file(package="dismo"), "/ex/VS_clean.csv", sep="")
bradypus <- read.table(file,  header=TRUE,  sep=',')
# first layer of the RasterStack
plot(predictors, 12)
# note the "add=TRUE" argument with plot
plot(wrld_simpl, add=TRUE)
# with the points function, "add" is implicit
points(VS_clean, col='blue')

presvals <- extract(predictors, VS_clean)
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata[,'biome'] = as.factor(sdmdata[,'biome'])
head(sdmdata)









#######this is the paart that actually works!!!!!!



plot(AS[,2:1], cex=0.5,col='red')
library(maptools)
## Checking rgeos availability: TRUE
data(wrld_simpl)
plot(wrld_simpl, add=TRUE)

library(raster)
wc <- raster::getData('worldclim', res=10, var='bio')
plot(wc[[c(1, 12)]], nr=2)
ASbfc <- extract(wc, AS[,2:1])
head(ASbfc)

plot(VS[,2:1], cex=0.5, col='blue')
plot(wrld_simpl, add=TRUE)
i <- which(is.na(VSbfc[,1]))
i
points(VS[i, ], pch=20, cex=3, col='red')

plot(VSbfc[ ,'bio1'] / 10, bfc[, 'bio12'], xlab='Annual mean temperature (C)',ylab='Annual precipitation (mm)')



VE <- read_csv("C:/Users/etelford.IC.000/Dropbox/Phd/Vachellia_distribution/VE.csv")
plot(VE[,2:1], cex=0.5,col='red')
library(maptools)
## Checking rgeos availability: TRUE
data(wrld_simpl)
plot(wrld_simpl, add=TRUE)

library(raster)
wc <- raster::getData('worldclim', res=10, var='bio')
plot(wc[[c(1, 12)]], nr=2)
bfc <- extract(wc, VE[,2:1])
head(bfc)

plot(VE[,2:1], cex=0.5, col='blue')
plot(wrld_simpl, add=TRUE)
i <- which(is.na(bfc[,1]))
i
points(VE[i, ], pch=20, cex=3, col='red')

plot(bfc[ ,'bio1'] / 10, bfc[, 'bio12'], xlab='Annual mean temperature (C)',ylab='Annual precipitation (mm)')

bfc[, 'bio12']<- as.numeric(bfc[, 'bio12'])
VEhist<-hist(bfc[, 'bio12'],
     main = "Vachellia erioloba",
     xlab = "Annual precipitation (mm per year)", ylab = "Species occurrence",
     col = "blue", xlim=c(0,2000), breaks = 10)

x <- VSbfc[, 'bio12']
na.omit(x)
h<-hist(VSbfc[, 'bio12'],
        main = "Vachellia sieberiana",
        xlab = "Annual precipitation (mm per year)", ylab = "Species occurrence",
        col = "red", xlim=c(0,2000), breaks = 20)


VEhist<- curve(dnorm(bfc, col="darkred", lwd=2, add=TRUE, yaxt="n")
