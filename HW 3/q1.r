#the code from our homework
#install.packages('igraph')
library('igraph')
nexus.get('karate')
?nexus.get
mis.data=nexus.get('miserables')
dol.data=nexus.get('dolphins')

#see the demo
igraphdemo("hrg")

#copy the code from demo 
g1 <- make_full_graph(5)
g2 <- make_ring(5)

g <- g1 + g2
g <- g + edge(1, vcount(g1)+1)
#the plot looks good
plot(g2)


#See the property of it
typeof(g)
typeof(miserables)
#They are the list,so get the attributes' name
names(g)
names(miserables)
#This is an unexpected answer,so I guess it is a class,not a list.R may just treate a class like a list.
#And we see "data" and "mis.data" are really similiar in their structure,so I suppose they are from the same class.
#now we know "edge()" "vcount()" are functions defined in this type of class.See the result step by step.

#This should be the number of vertice.
vcount(g1)

#some information written by a list,we want to see the effect of this function
edge(1, vcount(g1)+1)
par(mfrow=c(1,2))
g <- g1 + g2
plot(g)
g <- g + edge(1, vcount(g1)+1)
plot(g)
par(mfrow=c(1,1))

#well,it will add an edge between two vertices.Keep that in mind,for the question 2 we may need it,because if we know 
#how to add,That means we know how to drop an edge.Here we still don't know how does the function know wich edge we want
#to add.But we don't care it right now.




#code from demo

### Fit HRG
ghrg <- fit_hrg(g)
plot_dendrogram(ghrg)


### Create a consensus dendrogram from multiple samples, takes longer...
hcons <- consensus_tree(g)
hcons$consensus
#The problem is here,we get the null answer,so just skip it.Now we do our homework.

#looks pretty ugly........but that's the answer for question 1a.
#demo says this package will make the plot better,but still not good.
#install.packages('ape')
library('ape')
#
mis.hrg <- fit_hrg(mis.data)
plot_dendrogram(mis.hrg)

dol.hrg <- fit_hrg(dol.data)
plot_dendrogram(dol.hrg)

#question 1b
#now we need to create our own data by deleting some edges,so we need to see how "edge()" works.
?edge
#It redefine the "+" and "-" sign for this type of class,so we only need to know the following:
#1.what does "a","b" in "edge(a,b)" mean
#2.how do we know which edge is already exist in the data
#3.randomly delete the edge.

#for 1,I will recommend to try different number and see the plots look like.
g.old <- g1 + g2
g <- g.old + edge(1, 6)#it is equivalent to "g.old + edge(1, vcount(g1)+1)" for vcount(g1)=5
plot(g)
#The answer is clear,the parameter is just the index of the vertice.

#for 2,we need to see the help package.
?igraph
#extrace the edge
E(dol.data)
#try some common operation
E(dol.data)[1]
length(E(dol.data))
#perfect!it is an egde,I guess we don't need "edge" function now.let's prove our guess
a1=E(dol.data)[1]
a2=edge(1,2)
#They don't have same structure,So we need to show our guess on the other way.
g <- make_ring(5)
a=E(g)[1]
a
g=g-a
plot(g)
#Even they have different structure,the "+" and "-" sign still work.Now we can finish this question.

set.seed(1)
mydata=dol.data
record=c()
N.delete=ceiling(ecount(mydata)*0.05)
#this is used for testing the result
#N.delete=30
deleted<-sample(1:ecount(mydata),N.delete)
dol.del<-delete.edges(mydata, deleted)


plot(dol.del)


#now we predict the missing edge
pred<-predict_edges(dol.del)
del.record<-get.edges(mydata, deleted)#edges really deleted



#get the predict vertice
v.pred=pred$edges
v.prob=pred$prob
# cound the number of right prediction.
isFind=c()


#query function
query<-function(v,data){
  for(i in 1:dim(data)[1]){
    if((v[1]==data[i,1]&&v[2]==data[i,2])||(v[2]==data[i,1]&&v[1]==data[i,2])){
      return (TRUE);
    } 
  }
  return (FALSE);
}


for(i in 1:dim(v.pred)[1]){
  find=query(v.pred[i,],del.record)
  isFind=c(isFind,find)
}
index=which(isFind)
prob=pred$prob[isFind]


mean(isFind[1:100])
index
prob









