library(readr)

VEX <- read_csv("C:/Users/etelford.IC.000/Dropbox/Phd/Vachellia_distribution/VEX.csv")
VS_clean <- read_csv("C:/Users/etelford.IC.000/Dropbox/Phd/Vachellia_distribution/VS_clean.csv")
VS<- VS_clean
SN <- read_csv("C:/Users/etelford.IC.000/Dropbox/Phd/Vachellia_distribution/SN.csv")

library(maptools)
data(wrld_simpl)
afr=wrld_simpl[wrld_simpl$REGION==2,]
plot(afr)
box()
points(VS$Long, VS$Lat, col='red', pch=20, cex=0.75)
points(VEX$Long, VEX$Lat, col='blue', pch=20, cex=0.75)
points(SN$Long, SN$Lat, col='green', pch=20, cex=0.75)

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

library(ggplot2)
theme_set(theme_bw())
ggplot(data = world) +
        geom_sf() +
        xlab("Longitude") + ylab("Latitude") +
        ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))


library(raster)



wc <- raster::getData('worldclim', res=10, var='bio')
plot(wc[[c(1, 12)]], nr=2)


VSbfc <- extract(wc, VS[,2:1])
head(VSbfc)
plot(VS[,2:1], cex=0.5, col='blue')
plot(wrld_simpl, add=TRUE)
points(VS[i, ], pch=20, cex=3, col='red')
plot(VSbfc[ ,'bio1'] / 10, bfc[, 'bio12'], xlab='Annual mean temperature (C)',ylab='Annual precipitation (mm)')


VEXbfc <- extract(wc, VEX[,2:1])
head(VEXbfc)
plot(VEX[,2:1], cex=0.5, col='blue')
plot(wrld_simpl, add=TRUE)
points(VEX[i, ], pch=20, cex=3, col='red')
plot(VEXbfc[ ,'bio1'] / 10, VEXbfc[, 'bio12'], xlab='Annual mean temperature (C)',ylab='Annual precipitation (mm)')


SNbfc <- extract(wc, SN[,2:1])
head(SNbfc)
plot(SN[,2:1], cex=0.5, col='blue')
plot(wrld_simpl, add=TRUE)
points(SN[i, ], pch=20, cex=3, col='red')
plot(SNbfc[ ,'bio1'] / 10, SNbfc[, 'bio12'], xlab='Annual mean temperature (C)',ylab='Annual precipitation (mm)')



VEXhist<-hist(VEXbfc[, 'bio12'],
             main = "Vachellia exuvialis",
             xlab = "Annual precipitation (mm per year)", ylab = "Species occurrence",
             col = "blue", xlim=c(0,2000), breaks = 7)


h<-hist(VSbfc[, 'bio12'],
        main = "Vachellia sieberiana",
        xlab = "Annual precipitation (mm per year)", ylab = "Species occurrence",
        col = "red", xlim=c(0,2000), breaks = 20)

SNhiat<-hist(SNbfc[, 'bio12'],
        main = "Senegalia nigrescens",
        xlab = "Annual precipitation (mm per year)", ylab = "Species occurrence",
        col = "green", xlim=c(0,2000), breaks = 20)


