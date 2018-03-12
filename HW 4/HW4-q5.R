###############################
### Homework 4 - Question 5 ###
######### Abbas Rizvi #########
###############################
rm(list=ls())

### Part (a) ###

## Data released from the US Department of Commerece, Beureau of the Census is available in R
data(state)
?state
## First need to pre-process data
states <- data.frame(state.x77)
pc <- prcomp(states, center=T, scale=T)
#summary(pc)
screeplot(pc, type="lines")

region <- state.region
state.abb <- state.abb
division <- state.division
st.names <- state.name

library(ggbiplot)
mybiplot <- function(groups=region, group.title="Legend"){
g <- ggbiplot(pc, obs.scale = 2, var.scale = 1, groups=groups, labels =state.abb, 
              labels.size = 6, varname.size = 7, varname.adjust = 1.2)
g <- g + theme(legend.text = element_text(size = 16), legend.key.size=unit(1, "cm"),
               legend.title= element_text(size = 16, face="bold" ),
               axis.title=element_text(size=16,face="bold"))
g <- g +  labs(colour = group.title)

g
}         
pdf("PCA_regions.pdf", paper="USr", width=12, height=10)
mybiplot(region, "Regions")
dev.off()

pdf("PCA_regions", paper="USr", , width=12, height=10)
mybiplot(division, "Divisions")
dev.off

sc.states <- scale(states)
km.states <- kmeans(sc.states, 4)
clus.reg <- data.frame(Clusters = km.states$cluster, Regions = region)
tab.clus.reg1 <- table(clus.reg$Clusters, clus.reg$Regions)
save(tab.clus.reg1, clus.reg, file="clusTble.RData")
pdf("Mosaic_regClus.pdf",  width=8, height=6)
mosaicplot(tab.clus.reg1, color=c('#7fc97f','#beaed4','#fdc086','#ffff99'), cex.axis=1, 
           main=NULL, xlab="Clusters", ylab="Regions")
dev.off()

##################
##### PART B #####
##################
## Build a Gaussian Graphical Model using Graphical Lasso for the 8 predictors mentioned in Part A.
## What do you find for different penalities, and how does it complement (and/or contradict) your results in part A?
library(gRbase)
library(gRim)
library(gRain)
library(glasso)
library(pheatmap)
library(ggm)
library(igraph)
state.x77 <- data.frame(state.x77)
# Look at partial correlation
S.body <- cov.wt(sc.states, method = "ML")
PC.body <- cov2pcor(S.body$cov)
pheatmap(PC.body, cex=1.5, display_numbers=T) 

S <- S.body$cov 

# Estimate a single graph
m0.lasso <- glasso(S, rho = 0.1) 
names(m0.lasso)
#we are interested in 'wi'
#'wi' is the estimated inverse covariance matrix
my.edges <- m0.lasso$wi != 0 #if edge its TRUE if theres not an edge its FALSE
diag(my.edges) <- FALSE #edges pointed to own vertices need to be 0. we don't want to see these self-loops
g.lasso <- as(my.edges, "graphNEL") # convert for plotting, adjacency matrix
nodes(g.lasso) <- colnames(state.x77) #label graphNEL object with column names
glasso.net <- cmod(g.lasso, data = state.x77) #cmod ... specify GGM, c stands for continuous
plot(glasso.net)

?nodes
# Estimate over a range of rho's
rhos <- seq(0.05, 0.6, 0.10) #model selection aspect ... try range of rhos (complexity parameter?)
#rho -- vector of non-negative regularization parameters for the lasso
#should be increasing from smallest to largest value of rho
m0.lasso <- glassopath(S, rho = rhos)
par(mfrow=c(2,3))
for (i in 1:length(rhos)){
  my.edges <- m0.lasso$wi[, , i] != 0 #stack of matrices
  diag(my.edges) <- FALSE #adjacency matrix
  g.lasso <- as(my.edges, "graphNEL") # convert for plotting
  nodes(g.lasso) <- names(state.x77)
  glasso.net <- cmod(g.lasso, data = state.x77)
  plot(glasso.net)
  title(main=paste("rho = ", rhos[i], sep=""))
}


dev.off()


