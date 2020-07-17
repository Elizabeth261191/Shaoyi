
library("rgbif")
library(ggplot2)
library(maps)
library(ggthemes)

ZA_code <- isocodes[grep("South Africa", isocodes$name), "code"]
occur <- occ_search(scientificName = "Vachellia karroo", country = ZA_code, hasCoordinate = TRUE, limit = 3000, year = '2006,2016', return = "data")
str(occur)


(map <- ggplot(occur, aes(x = decimalLongitude, y = decimalLatitude)) + 
        borders(database = "world", regions = "South Africa", colour = "gray40", size = 0.3) +  
    theme_map() + geom_point(alpha = 0.4, colour = "red")) 




library(ggplot2)
library(rWBclimate)
library(rgbif)
## Grab some occurrence data

splist <- c("Vachellia sieberiana", "Vachellia erioloba")
out <- occurrencelist_many(splist, coordinatestatus = TRUE, maxresults = 1000)

## Set the map data path
options(kmlpath = "/Users/edmundhart/kmltemp")
sp.map.df <- create_map_df(c("ZA"))

## create map plot
sp.map <- ggplot(sp.map.df,aes(x=long,y=lat,group=group))+geom_polygon(fill="white",colour="black")+xlim(-130,-65)+ylim(12,50)

## Overlay occurrence data
sp.map + geom_point(data=gbifdata(out),aes(y=decimalLatitude,x=decimalLongitude,group=taxonName,colour=taxonName))

library(spocc)
library(raster)

data(package="ecospat")
dim(ecospat.testData)
names(ecospat.testData)
dim(ecospat.testNiche)
names(ecospat.testNiche)
data(ecospat.testNiche.inv)
names(ecospat.testNiche.inv)
data(ecospat.testNiche.nat)
names(ecospat.testNiche.nat)

fpath <-system.file("extdata", "ecospat.testTree.tre", package="ecospat")
fpath
tree<-read.tree(fpath)
tree$tip.label

plot(tree, cex=0.6)

ecospat.mantel.correlogram(dfvar=ecospat.testData[c(2:16)],colxy=1:2, n=100,colvar=3:7, max=1000, nclass=10, nperm=100)

colvar <- ecospat.testData[c(4:8)]
x <-cor(colvar, method="pearson")
ecospat.npred(x, th=0.75)
x <-cor(colvar, method="spearman")
ecospat.npred(x, th=0.75)



x <- ecospat.testData[c(4:8)]
p<- x[1:90,]
ref<- x[91:300,]
ecospat.exdet(ref,p)

x <- ecospat.testData[c(2,3,4:8)]
proj<- x[1:90,]
cal<- x[91:300,]

mess.object<-ecospat.mess(proj, cal, w="default")
ecospat.plot.mess(xy=proj[c(1:2)], mess.object, cex=1, pch=15)



fpath <-system.file("extdata", "ecospat.testTree.tre", package="ecospat")
tree <-read.tree(fpath)
data <- ecospat.testData[9:52]

pd<-ecospat.calculate.pd(tree, data, method = "spanning", type = "species", root = TRUE, average = FALSE)
pd
plot(pd)

grid.clim.nat <-ecospat.grid.clim.dyn(glob=scores.globclim,glob1=scores.clim.nat,sp=scores.sp.nat, R=100,th.sp=0)

inv <- ecospat.testNiche.inv
nat <- ecospat.testNiche.nat

pca.env <-dudi.pca(rbind(nat,inv)[,3:10],scannf=F,nf=2)
ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)

scores.globclim <- pca.env$li
scores.sp.nat <-suprow(pca.env,nat[which(nat[,11]==1),3:10])$li
# PCA scores for the species invasive distribution
scores.sp.inv <-suprow(pca.env,inv[which(inv[,11]==1),3:10])$li
# PCA scores for the whole native study area
scores.clim.nat <-suprow(pca.env,nat[,3:10])$li
# PCA scores for the whole invaded study area
scores.clim.inv <-suprow(pca.env,inv[,3:10])$l



# gridding the native niche
grid.clim.nat <-ecospat.grid.clim.dyn(glob=scores.globclim,glob1=scores.clim.nat,sp=scores.sp.nat, R=100,th.sp=0)
# gridding the invasive niche
grid.clim.inv <-ecospat.grid.clim.dyn(glob=scores.globclim,glob1=scores.clim.inv,sp=scores.sp.inv, R=100,th.sp=0)


# Compute Schoener's D, index of niche overlap
D.overlap <-ecospat.niche.overlap(grid.clim.nat, grid.clim.inv, cor=T)$D
D.overlap

eq.test <-ecospat.niche.equivalency.test(grid.clim.nat, grid.clim.inv,rep=1000, alternative = "greater")
sim.test <-ecospat.niche.similarity.test(grid.clim.nat, grid.clim.inv,rep=1000, alternative = "greater",rand.type=2)
ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test, "D", "Similarity")
niche.dyn <-ecospat.niche.dyn.index(grid.clim.nat, grid.clim.inv, intersection = 0.1)
ecospat.plot.niche.dyn(grid.clim.nat, grid.clim.inv, quant=0.25, interest=2,title= "Niche Overlap", name.axis1="PC1",name.axis2="PC2")
ecospat.shift.centroids(scores.sp.nat, scores.sp.inv, scores.clim.nat, scores.clim.inv)

# gridding the native niche
grid.clim.t.nat <-ecospat.grid.clim.dyn(glob=as.data.frame(rbind(nat,inv)[,10]),glob1=as.data.frame(nat[,10]),sp=as.data.frame(nat[which(nat[,11]==1),10]),R=1000, th.sp=0)
# gridding the invaded niche
grid.clim.t.inv <-ecospat.grid.clim.dyn(glob=as.data.frame(rbind(nat,inv)[,10]),glob1=as.data.frame(inv[,10]),sp=as.data.frame(inv[which(inv[,11]==1),10]),R=1000, th.sp=0)
t.dyn<-ecospat.niche.dyn.index(grid.clim.t.nat, grid.clim.t.inv,intersection=0.1)

ecospat.plot.niche.dyn(grid.clim.t.nat, grid.clim.t.inv, quant=0, interest=2, title= "Niche Overlap",name.axis1="Average temperature")



data <- ecospat.testData[c(9:16,54:57)]

ecospat.co_occurrences(data)



data<- ecospat.testData[c(53,62,58,70,61,66,65,71,69,43,63,56,68,57,55,60,54,67,59,64)]
nperm <- 10000
outpath <-getwd()
ecospat.Cscore(data, nperm, outpath)


data <- ecospat.testData[,4:8]
ecospat.cor.plot(data)

data <- ecospat.testData
caleval <-ecospat.caleval(data = ecospat.testData[53], xy = data[2:3],row.num = 1:nrow(data), nrep = 2, ratio = 0.7,disaggregate = 0.2, pseudoabs = 100, npres = 10,replace = FALSE)

fit <- ecospat.testData$glm_Saxifraga_oppositifolia

obs<-ecospat.testData$glm_Saxifraga_oppositifolia[which(ecospat.testData$Saxifraga_oppositifolia==1)
                                              
ecospat.boyce(fit, obs, nclass = 0, window.w = "default", res = 100,PEplot = TRUE)$Spearman.cor

eval<-ecospat.testData[c(53,62,58,70,61,66,65,71,69,43,63,56,68,57,55,60,54,67,59,64)]
pred<-ecospat.testData[c(73:92)]
ecospat.CommunityEval(eval, pred, proba=T, ntir=5)




library(biomod2)
path.wd<-getwd()
xy <- inv[,1:2]
head(xy)
current <- inv[3:7]
head(current)
setwd(path.wd)
t1 <-Sys.time()
sp<-1

myBiomodData <-BIOMOD_FormatingData( resp.var =as.numeric(sp_occ[,sp]),expl.var = current,resp.xy = xy,resp.name =colnames(sp_occ)[sp])

myBiomodOption <-Print_Default_ModelingOptions()


myBiomodOption@GLM$test ='none'
myBiomodOption@GBM$interaction.depth = 2
### Calibration of simple bivariate models
my.ESM <-ecospat.ESM.Modeling( data=myBiomodData,models=c('GLM','RF'),models.options=myBiomodOption,NbRunEval=1,DataSplit=70,weighting.score=c("AUC"),parallel=F)                                                  

### Evaluation and average of simple bivariate models to ESMs
my.ESM_EF <-ecospat.ESM.EnsembleModeling(my.ESM,weighting.score=c("SomersD"),threshold=0)
### Projection of simple bivariate models into new space
my.ESM_proj_current <-ecospat.ESM.Projection(ESM.modeling.output=my.ESM,new.env=current)

my.ESM_EFproj_current <-ecospat.ESM.EnsembleProjection(ESM.prediction.output=my.ESM_proj_current,ESM.EnsembleModeling.output=my.ESM_EF)





ecospat.SESAM.prr(proba, sr)
presence<-ecospat.testData[c(53,62,58,70,61,66,65,71,69,43,63,56,68,57,55,60,54,67,59,64)]
pred<-ecospat.testData[c(73:92)]

nbpermut <- 10000
outpath <-getwd()
ecospat.cons_Cscore(presence, pred, nbpermut, outpath)




