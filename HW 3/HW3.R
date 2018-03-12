rm(list=ls())

###########################################
###  Q1
###########################################

#### 1.a) ####

library(igraph)
set.seed(1)

# Undirected social network of frequent associations between 62 dolphins 
#in a community living off Doubtful Sound, New Zealand.
dolphins <- nexus.get("dolphins")

# Two characters are connected if they appear in the same scene. 
# The weight of the connection is the number of common appearances.
miserables <- nexus.get("miserables")

?igraphdemo

igraph_demo("hrg")

### Optimalize modularity

# cluster_optimal: This function calculates the optimal community structure of a graph, 
# by maximizing the modularity measure over all possible partitions.
optcom.d <- cluster_optimal(dolphins)
optcom.m <- cluster_optimal(miserables)

# membership: Numeric vector, one value for each vertex, 
# assigns which cluster the vertex belongs to
V(dolphins)$comm <- membership(optcom.d)
V(miserables)$comm <- membership(optcom.m)

dol_p1 <- plot(optcom.d, dolphins)
mis_p1 <- plot(optcom.m, miserables)

hrg.dol <- fit_hrg(dolphins)
hrg.mis <- fit_hrg(miserables)


### Plot the full hierarchy, as an igraph graph
ihrg.d <- as.igraph(hrg.dol)
ihrg.d$layout <- layout.reingold.tilford
colors <- rainbow(max(membership(optcom.d)))
plot(ihrg.d, vertex.size=10, edge.arrow.size=0.5, 
     margin=c(-0.1,-0.1,-0.1,-0.1), vertex.color=colors[membership(optcom.d)])

ihrg.m <- as.igraph(hrg.mis)
ihrg.m$layout <- layout.reingold.tilford
colors <- rainbow(max(membership(optcom.m)))
plot(ihrg.m, vertex.size=10, edge.arrow.size=0.5, 
     margin=c(-0.3,-0.3,-0.1,-0.3), vertex.color=colors[membership(optcom.m)])


### Plot it as a dendrogram
dendPlot(hrg.dol, edge.color="black", cex=0.5)
dendPlot(hrg.mis, edge.color="black", cex=0.5)

#### 1.b-c) ####
set.seed(1)

edge.pred <- function ( ig_obj, pct) {
  #ig_obj = igraph class R obj, pct=percent
  

  #number of edges will be deleted
  num.del <- round(ecount(ig_obj)*(pct/100))
  
  #pick random edges to delete
  delete<-sample(1:ecount(ig_obj),num.del)
  
  #delete the randomly picked edges
  igraph.del<-delete.edges(ig_obj, delete)
  
  #predict the missing edges
  #actual edges in the graph aren't repredicted
  pred<-predict_edges(igraph.del)
  
  #indices of predicted edges
  pred.edge <-pred$edges
  
  #indices of deleted edges
  del.edge<-get.edges(ig_obj, delete)
  
  #match deleted indices with predicted indices
  temp = NULL
  foo <- function(x){ 
    for(i in 1:nrow(del.edge)){ temp[i] <- all(x == del.edge[i,])}
    any(temp)}
  
  cor.pred <-apply(pred.edge, 1, foo)
  # pred.edge[which(cor.pred),]
  num.del <- nrow(del.edge)
  n.cor.pred <- sum(1*cor.pred)
  tot.pred <- nrow(pred.edge)
  
  #probabilities of predicted edges
  d.prob <-pred$prob[which(cor.pred)]
  p.prob <- pred$prob
  num.del.75Q <- sum(1*(d.prob>quantile(p.prob)[4])) 
  
  cat(
" Number of deleted edges : ", num.del, "\n",
"Number of correctly predicted edges : ", n.cor.pred, "\n",
"Total number predicted edges : ", tot.pred, "\n",
"% of deleted edges found in the upper inter quartile range of all predicted edges : ", 100*num.del.75Q/num.del, "%")
 
  list(num.del=num.del,
       delete=delete,
       igraph.del=igraph.del,
       pred=pred,
       pred.edge=pred.edge,
       del.edge=del.edge,
       cor.pred=cor.pred,
       n.cor.pred=n.cor.pred,
       tot.pred=tot.pred,
       d.prob=d.prob,
       p.prob=p.prob,
       num.del.75Q=num.del.75Q
  )
  
}

# third quartile (designated Q3) also called the upper quartile or the 75th percentile 
#(splits off the highest 25% of data from the lowest 75%)

del5 <- edge.pred(dolphins, 5)  
del15 <- edge.pred(dolphins, 15) 
del40 <- edge.pred(dolphins, 40)

which(del5$cor.pred)
  
mymes <- function(x) 
  {cat(
  " Number of deleted edges : ", x$num.del, "\n",
  "Number of correctly predicted edges : ", x$n.cor.pred, "\n",
  "Total number predicted edges : ", x$tot.pred, "\n",
  "% of deleted edges that pass 3rd Q threshold : ", 100*x$num.del.75Q/x$num.del, "%\n",
  "Summary of predicted probabilities - all predicted edges : \n",
  "-- Min :", round(summary(x$p.prob)[[1]],4), "\n",
  "-- 1st Q :", round(summary(x$p.prob)[[2]],4), "\n",
  "-- Median :", round(summary(x$p.prob)[[3]],4), "\n",
  "-- Mean :", round(summary(x$p.prob)[[4]],4), "\n",
  "-- 3rd Q :", round(summary(x$p.prob)[[5]],4), "\n",
  "-- Max :", round(summary(x$p.prob)[[6]],4), "\n",
  "Summary predicted probabilities - deleted edges : \n",
  "-- Min :", round(summary(x$d.prob)[[1]],4), "\n",
  "-- 1st Q :", round(summary(x$d.prob)[[2]],4), "\n",
  "-- Median :", round(summary(x$d.prob)[[3]],4), "\n",
  "-- Mean :", round(summary(x$d.prob)[[4]],4), "\n",
  "-- 3rd Q :", round(summary(x$d.prob)[[5]],4), "\n",
  "-- Max :", round(summary(x$d.prob)[[6]],4))}
save(file="mymes.Rdata", mymes)

mymes(del5)
mymes(del15)

sum(del40$p.prob)
###########################################
#### Q2 ##################################
###########################################

library(RBGL)
library(ggm)
library(gRbase)
library(gRain)
library(Rgraphviz)

#writing format: ~parent (w/o parent) ~child|parent, for childs with 2 parents: ~child|parent1:parent2
B <- list(~burglary, ~alarm|burglary:earthquake, ~earthquake, 
          ~tv, ~john|tv:alarm, ~nap, ~mary|nap:alarm)
B <- dagList(B)

plot(B)

Badj <- as(B,"matrix")
B.pr <- inducedDAG(Badj, order=c("burglary", "earthquake", "tv", "nap", "john", "mary"))
B.pr.gr <- as(B.pr,"graphNEL")

dev.off()
plot.new()
par(mfrow=c(2,1))
plot(B)
plot(B.pr.gr)

###########################################
#### Q3 ##################################
###########################################
library(gRain)
g <- list(~a, ~c|a, ~d|a:b, ~b, ~e|b, ~g|d:e, ~f|c:e, ~g|d:e, ~h|f:g)
g.dag <- dagList(g)
plot(g.dag)

dSep(as(g.dag, "matrix"), "c", "g", NULL)
dSep(as(g.dag, "matrix"), "c", "e", NULL)
dSep(as(g.dag, "matrix"), "c", "e", "g")
dSep(as(g.dag, "matrix"), "a", "g", c("d","e"))
dSep(as(g.dag, "matrix"), "a", "g", "d")



