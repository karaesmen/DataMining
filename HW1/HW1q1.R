rm(list=ls())

org_bodyfat <- bodyfat
bodyfat <- org_bodyfat
## Turn height from inch into cm
# 1 inch = 2.54 cm
bodyfat$height <- bodyfat$height*2.54

## Turn weight from lbs into grams
# 1 pound = 453.592 gr

bodyfat$weight <- bodyfat$weight*0.453592



#### boxplots anf histograms ####
par(mfrow = c(3,5), mar=(c(2,2,2,2)))
for(i in 1:ncol(bodyfat)){boxplot(bodyfat[,i], main=colnames(bodyfat)[i])}
## histograms
den=NULL
for(i in 1:ncol(bodyfat)){
  den <- density(bodyfat[,i])
  hist(bodyfat[,i], probability = T, xlim= range(den$x), ylim=range(den$y),
       main=colnames(bodyfat)[i])
  lines(den)}

#we can see that the some variables show a skewed density 
# but it's likely that it is due to outliers

###### Setting outliers to NAs ######

bodyfat_o2na <- bodyfat

for(i in 1:ncol(bodyfat)){
  outliers <- which(bodyfat[,i] %in% boxplot.stats(bodyfat[,i])$out == TRUE)
  bodyfat_o2na[outliers,i] <- NA
}

#### boxplots anf histograms for nonNA ####
par(mfrow = c(3,5), mar=(c(2,2,2,2)))
for(i in 1:ncol(bodyfat)){boxplot(bodyfat[,i], main=colnames(bodyfat)[i])}
## histograms
den=NULL
for(i in 1:ncol(bodyfat)){
  den <- density(bodyfat[,i])
  hist(bodyfat[,i], probability = T, xlim= range(den$x), ylim=range(den$y),
       main=colnames(bodyfat)[i])
  lines(den)}

#we can see that the some variables show a skewed density 
# but it's likely that it is due to outliers
for(i in 1:ncol(bodyfat_o2na)){boxplot(bodyfat_o2na[,i],main=colnames(bodyfat)[i])}

den=NULL
for(i in 1:ncol(bodyfat)){
  den <- density(bodyfat_o2na[,i], na.rm=T)
  hist(bodyfat_o2na[,i], probability = T, xlim= range(den$x), ylim=range(den$y),
       main=colnames(bodyfat)[i])
  lines(den)}

#### Scatterplot ####
par(mfrow = c(1,2), pty = "s") #pty = "s" is square plotting region
plot(weight ~ height, data = bodyfat_o2na)
plot(log(weight) ~ log(height), data = bodyfat_o2na)

plot(weight ~ density, data = bodyfat_o2na)
plot(log(weight) ~ log(density), data = bodyfat_o2na)

plot(height ~ density, data = bodyfat_o2na)
plot(log(height) ~ log(density), data = bodyfat_o2na)

plot(weight ~ height, data = bodyfat_o2na)
plot(log(weight) ~ log(height), data = bodyfat_o2na)

#### ggpairs #### 

splom(bodyfat_o2na[,c(1:5)], data = bodyfat_o2na)

library(ggplot2)
library(GGally)
bodyfat_o2na[["age"]] <- ordered(cut(bodyfat_o2na[["age"]], c(15 , 25, 45 , 65, 100)), 
                             labels=c("Young", "Adult", "Senior", "Elderly"))
bodyfat_o2na <- na.omit(bodyfat_o2na)
bdf[["age"]] <- ordered(cut(bdf[["age"]], c(15 , 25, 45 , 65, 100)), 
                                 labels=c("-25", "Middle-aged", "Senior", "Elderly"))
quartz()
ggpairs(data=bdf, colour="age", columns=c(1:6))
ggpairs(data=iris, colour="Species")


dens <- function(i){
  j = seq(1:15)[-3]
t = j[i]
 ggplot(bodyfat_o2na, aes(x = bodyfat_o2na[,t], group=age, colour=age)) +
        geom_density( size=1, na.rm=T) +
        ggtitle(colnames(bodyfat_o2na)[t]) + 
        theme(plot.title = element_text(size=15,lineheight=5, face="bold"),
              axis.title.x = element_blank()) +
    guides(fill=FALSE, colour=FALSE) }

p1<-dens(1)
p2<-dens(2)
p3<-dens(3)
p4<-dens(4)
p5<-dens(5)
p6<-dens(6)
p7<-dens(7)
p8<-dens(8)
p9<-dens(9)
p10<-dens(10)
p11<-dens(11)
p12<-dens(12)
p13<-dens(13)
p14<-dens(14)
p15<-dens(15)

age <- qplot(age, data=bodyfat_o2na ,fill = age, xlab="", ylab="")
age <- age + ggtitle("age") + guides(fill=FALSE)
age<- age+theme(axis.text=element_text(size=12, face="bold"),
                plot.title = element_text(size=15,lineheight=5, face="bold")) #, axis.title=element_text(size=20,face="bold"))
age

library(gridExtra)
grid.arrange(age, p1,p2,p3,p4,p5,p6,p7,p8,p9,
             p10,p11,p12, p13, p14, ncol=5)



#### lattice bullshit ####
library(lattice)
mtheme <- standard.theme("pdf", color=TRUE)
mtheme$plot.line$lwd <- 5
mtheme$superpose.line$lwd <- 5
j = seq(1:15)[-3]
dens = list()
for(i in 1:14){
  t=j[i]
  dens[[i]] <- densityplot(~bodyfat_o2na[,t], groups = age, data = bodyfat_o2na, 
              xlab=colnames(bodyfat_o2na)[t],
              main=list(label=colnames(bodyfat_o2na)[t], cex=2),
#               auto.key = list(space = "top", columns=4, cex=1.2, 
#                               lines = T, title="Age Group", cex.title=1.5),
              par.settings=mtheme)
}


# bdf <- densityplot(~bodyfat_o2na[,2], groups = age, data = bodyfat_o2na, 
#                  xlab=colnames(bodyfat_o2na)[2],
#                  main=list(label=colnames(bodyfat_o2na)[2], cex=2),
#                  auto.key = list(space = "top", columns=4, cex=1.2, 
#                                  lines = T, title="Age Group", cex.title=1.5),
#                  par.settings=mtheme)

print(dens[[1]], split = c(1, 1, 3, 2), more = TRUE)
print(dens[[2]], split = c(2, 1, 3, 2), more = TRUE)
print(dens[[3]], split = c(3, 1, 3, 2), more = TRUE)
print(dens[[4]], split = c(1, 2, 3, 2), more = TRUE)
print(dens[[5]], split = c(2, 2, 3, 2), more = TRUE) 
print(dens[[6]], split = c(3, 2, 3, 2), more = FALSE) 


#### Corrlation plots #####
library(corrplot)
cor_bf <- cor(as.matrix(bodyfat_o2na), use="complete.obs", method = "pearson")
corrplot(cor_bf, method = "circle")

