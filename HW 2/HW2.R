######################
##  HW2
#####################   


rm(list = ls())
graphics.off()

#### Q1 #####

load("./SwissBankNotes.RData")
head(SwissBankNotes)

par(mfrow=c(2,3))
for(i in 1:6){
boxplot(SwissBankNotes[1:100,i], SwissBankNotes[101:200,i], names=c("Genuine", "Counterfeit"), 
        col=c("#5ab4ac" , "#d8b365"), main=colnames(SwissBankNotes)[i])
}

apply(SwissBankNotes, 2, var)


bnotes <- SwissBankNotes
bnotes[[7]] <- as.factor(c(rep("genuine", 100), rep("counterfeit", 100)))
colnames(bnotes) <- c("lenght", "height.L", "height.R", "inner.L", "inner.U", "diagonal","type")
head(bnotes)

#genuine pca
gen.pca.csF <- prcomp(bnotes[1:100,1:6], center = FALSE, scale = FALSE)
gen.pca.cTsF <- prcomp(bnotes[1:100,1:6], center = TRUE, scale = FALSE)
# gen.pca.cFsT <- prcomp(bnotes[1:100,1:6], center = FALSE, scale = TRUE)
gen.pca.csT <- prcomp(bnotes[1:100,1:6], center = TRUE, scale = TRUE)


summary(gen.pca.csF)
summary(gen.pca.cTsF)
summary(gen.pca.csT)


par(mfrow=c(1,3))
screeplot(gen.pca.csF,  type="lines", main="Genuine Only \n center = FALSE, scale = FALSE")
# screeplot(gen.pca.cFsT, type="lines", main="Genuine Only \n center = FALSE, scale = TRUE")
screeplot(gen.pca.cTsF, type="lines", main="Genuine Only \n center = TRUE, scale = FALSE")
screeplot(gen.pca.csT, type="lines", main="Genuine Only \n center = TRUE, scale = TRUE")


#counterfeit pca
count.pca.csF <- prcomp(bnotes[101:200,1:6], center = FALSE, scale = FALSE)
count.pca.cTsF <- prcomp(bnotes[101:200,1:6], center = TRUE, scale = FALSE)
# gen.pca.cFsT <- prcomp(bnotes[101:200,1:6], center = FALSE, scale = TRUE)
count.pca.csT <- prcomp(bnotes[101:200,1:6], center = TRUE, scale = TRUE)

summary(count.pca.csF)
summary(count.pca.cTsF)
summary(count.pca.csT)

par(mfrow=c(1,3))
screeplot(count.pca.csF,  type="lines", main="Counterfeit Only \n center = FALSE, scale = FALSE")
# screeplot(gen.pca.cFsT, type="lines", main="Genuine Only \n center = FALSE, scale = TRUE")
screeplot(count.pca.cTsF, type="lines", main="Counterfeit Only \n center = TRUE, scale = FALSE")
screeplot(count.pca.csT, type="lines", main="Counterfeit Only \n center = TRUE, scale = TRUE")


#all-data pca
all.pca.csF <- prcomp(bnotes[,1:6], center = FALSE, scale = FALSE)
all.pca.cTsF <- prcomp(bnotes[,1:6], center = TRUE, scale = FALSE)
# gen.pca.cFsT <- prcomp(bnotes[101:200,1:6], center = FALSE, scale = TRUE)
all.pca.csT <- prcomp(bnotes[,1:6], center = TRUE, scale = TRUE)

summary(all.pca.csF)
summary(all.pca.cTsF)
summary(all.pca.csT)

par(mfrow=c(1,3))
screeplot(all.pca.csF,  type="lines", main="Counterfeit & Genuine \n center = FALSE, scale = FALSE")
# screeplot(gen.pca.cFsT, type="lines", main="Genuine Only \n center = FALSE, scale = TRUE")
screeplot(all.pca.cTsF, type="lines", main="Counterfeit & Genuine \n center = TRUE, scale = FALSE")
screeplot(all.pca.csT, type="lines", main="Counterfeit & Genuine \n center = TRUE, scale = TRUE")

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
mybiplot <- function(x, comps, grps, leg.pos){
g<-ggbiplot(x, choices = comps, scale = 1, obs.scale = 1, var.scale = 1, 
         groups = grps, ellipse = TRUE,
         circle = F, varname.size = 8,
         varname.adjust = 1.5)
g <- g + scale_color_discrete(name = '') 
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = leg.pos, legend.text = element_text(size = 20),
               axis.title=element_text(size=16,face="bold"))
g
}


mybiplot(gen.pca.csT, comps=c(1,2), grps=F, leg.pos = "none")
mybiplot(count.pca.csT, comps=c(1,2), grps=F, leg.pos = "none")
layout(matrix(c(1,2),1,2))
mybiplot(all.pca.csT, comps=c(1,2), grps=bnotes$type, leg.pos = "top")
mybiplot(all.pca.cTsF, comps=c(1,2), grps=bnotes$type, leg.pos = "top")


#### Q2 #####

rm(list = ls())
graphics.off()


# (a) Generate a simulated dataset

cl.1 <- matrix(data=rnorm(50*20, mean = 3, sd = 1), nrow=20, ncol=50)
cl.2 <- matrix(data=rnorm(50*20, mean = 6, sd = 0.5), nrow=20, ncol=50)
cl.3 <- matrix(data=rnorm(50*20, mean = 1, sd = 3), nrow=20, ncol=50)

sim.data <- rbind(cl.1, cl.2, cl.3)
sim.data <- data.frame(sim.data, as.factor(c(rep("Class1", 20), 
                                             rep("Class2", 20), 
                                             rep("Class3", 20))) )

colnames(sim.data) <- c(paste("V", 1:50, sep=""), "Class")

head(sim.data)
tail(sim.data)

save(sim.data, file="simdata.RData")
load("simdata.RData")

# (b) PCA on simulated data

pca.sim.data <- prcomp(sim.data[,1:50], center = TRUE, scale = T)
mybiplot(x=pca.sim.data, comps = c(1, 2), grps = sim.data$Class, leg.pos = "top")

# (c) k-means clustering

library("multtest")
library("fpc")
library("cluster")

#km3 <- kmeans(sim.data[,1:50], 3) #we want to have 3 groups

#save(km3, file="km3.RData")
load("km3.RData")

#tabulate the results 98.33333% was correctly classified
table(km3$cluster, sim.data$Class)
tail( cbind(km3$cluster, as.character(sim.data$Class)) )

clusters <- cbind(km3$cluster, as.character(sim.data$Class))
cl1.3 <- which(clusters[,1] == 3 & clusters[,2] == "Class1")
cl2.1 <- which(clusters[,1] == 1 & clusters[,2] == "Class2")
cl3.2 <- which(clusters[,1] == 2 & clusters[,2] == "Class3")
cl3.3 <- which(clusters[,1] == 3 & clusters[,2] == "Class3")

#plot the groups 
palette(c('#7fc97f','#beaed4','#fdc086'))
plot(sim.data[c("V1", "V2")], type="n", main="k=3")
text(sim.data[cl2.1, c("V1", "V2")], label=2, col='#beaed4', cex=2)
text(sim.data[cl3.2, c("V1", "V2")], label=3, col='#7fc97f', cex=2)
text(sim.data[cl3.3, c("V1", "V2")], label=3, col='#fdc086', cex=2)
text(sim.data[cl1.3, c("V1", "V2")], label=1, col='#fdc086', cex=2)

points(km3$centers[ , c("V1", "V2")], col = c('#7570b3','#1b9e77', '#d95f02'), pch = "*", cex = 5) #center points of the cluster
legend("bottomright", legend = paste("Cluster", 1:3), col=c('#beaed4','#7fc97f','#fdc086'), pch=20, pt.cex=2, cex=1.2)
legend("topright", legend = paste("Center", 1:3), col=c('#7570b3','#1b9e77', '#d95f02'), pch="*", pt.cex=3, cex=1.2)
# see which one was mis-clustered
points(sim.data[59, c("V1", "V2")]+0.1, col = '#e7298a', pch = "O", cex = 3)
text(sim.data[59, "V1"]+0.5, sim.data[59, "V2"]+1.9, col='#e7298a', 
     labels="Misclassified \n Data Point", adj=c(0.6, 0.6), cex=1)

palette(c('#7fc97f','#beaed4','#fdc086'))
plot(pca.sim.data$x[,1:2], type="n", main="k=3")
text(pca.sim.data$x[cl2.1, 1:2], label=2, col='#beaed4', cex=2)
text(pca.sim.data$x[cl3.2, 1:2], label=3, col='#7fc97f', cex=2)
#text(pca.sim.data$x[cl3.3, 1:2], label=3, col='#fdc086', cex=2)
text(pca.sim.data$x[cl1.3, 1:2], label=1, col='#fdc086', cex=2)
points(km3$centers[ , 1:2], col = c('#7570b3','#1b9e77', '#d95f02'), pch = "*", cex = 5) #center points of the cluster
legend("bottomright", legend = paste("Cluster", 1:3), col=c('#beaed4','#7fc97f','#fdc086'), pch=20, pt.cex=2, cex=1.2)
legend("topright", legend = paste("Center", 1:3), col=c('#7570b3','#1b9e77', '#d95f02'), pch="*", pt.cex=3, cex=1.2)
# see which one was mis-clustered
points(pca.sim.data$x[59, 1:2]+0.1, col = '#e7298a', pch = "O", cex = 3)
text(pca.sim.data$x[59, 1]+0.5, pca.sim.data$x[59, 1]+1.9, col='#e7298a', 
     labels="Misclassified \n Data Point", adj=c(0.6, 0.6), cex=1)


## (d) k =2 and k=4

# k = 2
# km2 <- kmeans(sim.data[, -51], 2) 
table(km2$cluster, sim.data$Class)

# save(km2, file="km2.RData")
load("km2.RData")
plot(silhouette(pam(sim.data, 2)) )
pam.k2 <- pam(sim.data,2)
names(pam.k2)
table(pam.k2$cluster, sim.data$Class)

clusters <- cbind(km2$cluster, as.character(sim.data$Class))
cl1 <- which(clusters[,1] == 1 & clusters[,2] == "Class1")
cl3 <- which(clusters[,1] == 1 & clusters[,2] == "Class3")
cl2 <- which(clusters[,1] == 2 & clusters[,2] == "Class2")

#plot k=2
plot(sim.data[c("V1", "V2")], type='n', main="k=2")
text(sim.data[cl1, c("V1", "V2")], label=1, col='#7fc97f', cex=2)
text(sim.data[cl3, c("V1", "V2")], label=3, col='#7fc97f', cex=2)
text(sim.data[cl2, c("V1", "V2")], label=2, col='#beaed4', cex=2)
points(km2$centers[ , c("V1", "V2")], col = c('#1b9e77','#7570b3'), pch = "*", cex = 5) #center points of the cluster
legend("bottomright", legend = paste("Cluster", 1:2), col=c('#7fc97f','#beaed4'), pch=20, pt.cex=2, cex=1.2)
legend("topright", legend = paste("Center", 1:2), col=c('#1b9e77','#7570b3'), pch="*", pt.cex=3, cex=1.2)

# k = 4
# km4 <- kmeans(sim.data[, -51], 4) 
table(km4$cluster, sim.data$Class)
# 
# save(km2, file="km4.RData")
load("km4.RData")


pam.k4 <- pam(sim.data,4)
names(pam.k4)
table(pam.k4$cluster, sim.data$Class)
plot(silhouette(pam.k4) , main="Silhouette plot of Simulated Data, k=4")

clusters <- cbind(km4$cluster, as.character(sim.data$Class))
cl1.4 <- which(clusters[,1] == 4 & clusters[,2] == "Class1")
cl2.1 <- which(clusters[,1] == 1 & clusters[,2] == "Class2")
cl3.2 <- which(clusters[,1] == 2 & clusters[,2] == "Class3")
cl3.3 <- which(clusters[,1] == 3 & clusters[,2] == "Class3")
cl3.4 <- which(clusters[,1] == 4 & clusters[,2] == "Class3")

# ['#1b9e77', '#7570b3','#d95f02','#386cb0']
# ['#7fc97f','#beaed4','#fdc086','#80b1d3']

plot(sim.data[c("V1", "V2")], type='n', main="k=4")
text(sim.data[cl1.4, c("V1", "V2")], label=1, col='#80b1d3', cex=2) #cl4
text(sim.data[cl2.1, c("V1", "V2")], label=2, col='#beaed4', cex=2) #cl1
text(sim.data[cl3.2, c("V1", "V2")], label=3, col='#7fc97f', cex=2) #cl2
text(sim.data[cl3.3, c("V1", "V2")], label=3, col='#fdc086', cex=2) #cl3
text(sim.data[cl3.4, c("V1", "V2")], label=3, col='#80b1d3', cex=2) #cl4

points(km4$centers[ , c("V1", "V2")], col = c('#7570b3','#1b9e77', '#d95f02','#386cb0'), pch = "*", cex = 5) #center points of the cluster
legend("bottomright", legend = paste("Cluster", 1:4), col=c('#beaed4','#7fc97f','#fdc086','#80b1d3'), pch=20, pt.cex=2, cex=1.2)
legend("topright", legend = paste("Center", 1:4), col=c('#7570b3','#1b9e77', '#d95f02','#386cb0'), pch="*", pt.cex=3, cex=1.2)

## (e) Kmeans with comp1 & 2
# names(pca.sim.data)
# pca.km3 <- kmeans(pca.sim.data$x[, 1:2], 3)

save(pca.km3, file="pcakm3.RData")
load("pcakm3.RData")
table(pca.km3$cluster, sim.data$Class)

pca.km3 <- kmeans(pca.sim.data$x[, 1:2], 3)

pca.km4 <- kmeans(pca.sim.data$x[, 1:2], 4)
save(pca.km4, file="pcakm4.RData")
load("pcakm4.RData")
table(pca.km4$cluster, sim.data$Class)


## (f) scale and cluster
# scl.data <- scale(sim.data[,-51])
# scl.km3 <- kmeans(scl.data, 3)
scl.km4 <- kmeans(scl.data, 4)
table(scl.km3$cluster, sim.data$Class)
table(scl.km4$cluster, sim.data$Class)


clusters <- cbind(km4$cluster, as.character(sim.data$Class))
cl1.4 <- which(clusters[,1] == 4 & clusters[,2] == "Class1")
cl2.1 <- which(clusters[,1] == 1 & clusters[,2] == "Class2")
cl3.2 <- which(clusters[,1] == 2 & clusters[,2] == "Class3")
cl3.3 <- which(clusters[,1] == 3 & clusters[,2] == "Class3")
cl3.4 <- which(clusters[,1] == 4 & clusters[,2] == "Class3")

# ['#1b9e77', '#7570b3','#d95f02','#386cb0']
# ['#7fc97f','#beaed4','#fdc086','#80b1d3']

plot(sim.data[c("V1", "V2")], type='n', main="k=4")
text(sim.data[cl1.4, c("V1", "V2")], label=1, col='#80b1d3', cex=2) #cl4
text(sim.data[cl2.1, c("V1", "V2")], label=2, col='#beaed4', cex=2) #cl1
text(sim.data[cl3.2, c("V1", "V2")], label=3, col='#7fc97f', cex=2) #cl2
text(sim.data[cl3.3, c("V1", "V2")], label=3, col='#fdc086', cex=2) #cl3
text(sim.data[cl3.4, c("V1", "V2")], label=3, col='#80b1d3', cex=2) #cl4



# save(scl.km3, file="sclkm3.RData")
load("sclkm3.RData")


##### Q3 #######

genedata <- read.csv("Ch10Ex11.csv", header=F)
colnames(genedata) <- c(paste("H", 1:20, sep=""), paste("D", 1:20, sep=""))
rownames(genedata) <- paste("Gene", 1:1000, sep="")
### clustering - dendogram 

library("multtest")
library("fpc")
library("cluster")


# vector of colors
labelColors = c("#af8dc3", '#7fbf7b')            #"#CDB380", "#036564", "#EB6841", "#EDC951", "cornflowerblue", "firebrick")

install.packages('dendextend')
library(dendextend)

groupCodes <- c(rep("H",20), rep("D",20))
colorCodes <- c(H="#d7191c", D='#2c7bb6')
linkage <- c( "single", "complete", "average", "centroid")
par(mfrow=c(2,2), mar=c(3,2,3,2))
for(i in 1:length(linkage)){
  d <- as.dist((1 - cor(genedata))/2) #correlation based distance as suggested in ?dist help file
  hc <- hclust(d, method=linkage[i])            # apply hirarchical clustering with different linkages
  hcd<-as.dendrogram(hc, dLeaf=0.1, h=0.3)
  
  
  # Assigning the labels of dendrogram object with new colors:
  labels_colors(hcd) <- colorCodes[groupCodes][order.dendrogram(hcd)]
  # Plotting the new dendrogram
  plot(hcd, main=paste(linkage[i], "linkage", sep=" "), ylim=c(0.1 , 0.6))
  
}

# Genes that has the most difference
DEttest <- function(x) t.test(x[1:20], x[21:40])$p.value
p.val <- apply(genedata, 1, DEttest)

euc.dist <- function(x) sqrt( sum( (x[1:20] - x[21:40]) ^ 2 ) )
distance <- as.numeric(apply(genedata, 1, euc.dist))

gene.pval <- data.frame( p.val, distance)

attach(gene.pval)
gene.pd <- gene.pval[order(p.val, distance), ]
head(gene.pd)

DEgenes <- gene.pd[which(gene.pd$p.val <= 0.05), ]
head(DEgenes, 20)
summary(distance)
nrow(DEgenes)

load("./DMII_HW2_env.RData")
