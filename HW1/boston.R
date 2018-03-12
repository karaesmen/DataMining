rm(list=ls())
library(ElemStatLearn)
library(MASS)
data(Boston)
head(Boston)

## boxplots
par(mfrow = c(3,5), mar=(c(2,2,2,2)))
for(i in 1:ncol(Boston)){boxplot(Boston[,i], main=colnames(Boston)[i])}

## histograms
par(mfrow = c(3,5), mar=(c(2,2,2,2)))
den=NULL
for(i in 1:ncol(Boston)){
  den <- density(Boston[,i])
  hist(Boston[,i], probability = T, xlim= range(den$x), ylim=range(den$y),
       main=colnames(Boston)[i])
  lines(den)}

## boxplots
par(mfrow = c(3,5), mar=(c(2,2,2,2)))
for(i in 1:ncol(Boston)){boxplot(Boston[,i], main=colnames(Boston)[i])}

#exclude some of the unclear variables
boston <- subset(Boston, select=-c(zn, chas, rad, black))

#summary
apply(Boston,2,summary)

# Crime rate
Boston[,"crim"][1:20]
summary(Boston[,"crim"])
crim_low <- as.numeric(subset(boston, crim < 1, select=crim)[,1])
summary(crim_low)
crim_mod <- as.numeric(subset(boston, crim > 1 & crim < 20, select=crim)[,1])
summary(crim_mod)
crim_high <- as.numeric(subset(boston, crim > 20, select=crim)[,1])
summary(crim_high)

par(mfrow = c(1,3), mar=(c(2,2,2,2)))
den=NULL
den <- density(crim_low)
truehist(crim_low, prob = T, xlim= range(den$x), #ylim=range(den$y),
       breaks=seq(0, 1, by=0.1),main="crim_low", col="gray70")
lines(den, lwd=3)

en=NULL
den <- density(crim_mod)
truehist(crim_mod, prob = T, xlim= range(den$x), #ylim=range(den$y),
         main="crim_mod", col="gray70")
lines(den, lwd=3)

den=NULL
den <- density(crim_high)
truehist(crim_high, prob = T, xlim= range(den$x), #ylim=range(den$y),
     breaks=seq(5, 90, by=5),main="crim_high", , col="gray70")
lines(den, lwd=3)

boston[["crim"]] <- ordered(cut(boston[["crim"]], c(0,1,20,90)) ,
                            labels=c("Safe", "Moderate", "Dangerous"))


the_density <- function(x, t){
  den=NULL
  den <- density(x)
  truehist(x, prob = T, xlim= range(den$x), #ylim=range(den$y),
           breaks=s,main=t, col="gray70")
  lines(den, lwd=3)
}

summary(boston$indus)
norm <- function(x){
  x-mean(x, na.rm=T)
}
norBoston <- apply(boston, 2, function(x) x-mean(x, na.rm=T))
par(mfrow = c(3,5), mar=(c(2,2,2,2)))
den=NULL
for(i in 1:ncol(norBoston)){
  den <- density(norBoston[,i])
  hist(norBoston[,i], probability = T, xlim= range(den$x), ylim=range(den$y),
       main=colnames(norBoston)[i])
  lines(den)}

subset(boston, black == 0.32)

#exclude some of the unclear variables
boston <- subset(Boston, select=-c(zn, chas, rad, black))

boston[["crim"]] <- ordered(cut(boston[["crim"]], c(0,1,20,90)) ,
                            labels=c("Safe", "Moderate", "Dangerous"))
boston[["indus"]] <- ordered(cut(boston[["indus"]], c(0,15,30)) ,
                            labels=c("small", "large"))
boston[["nox"]] <- ordered(cut(boston[["nox"]], c(0.3, 0.5, 0.7, 0.9)) ,
                            labels=c("low", "moderate", "high"))
boston[["rm"]] <- ordered(cut(boston[["rm"]], c(3, 5, 7, 9)) ,
                            labels=c("3-5", "5-7", "7-more"))
boston[["age"]] <- ordered(cut(boston[["age"]], c(2, 20, 50, 80, 100)) ,
                             labels=c("new","mostly-new","mostly-old", "old"))
boston[["dis"]] <- ordered(cut(boston[["dis"]], c(1, 2, 3.7, 6, 13)) ,
                           labels=c("very-close", "close", "far", "very-far"))
boston[["tax"]] <- ordered(cut(boston[["tax"]], c(180, 350, 750)) ,
                           labels=c("low", "high"))
boston[["ptratio"]] <- ordered(cut(boston[["ptratio"]], c(12.6,17.4,20.2,23)) ,
                           labels=c("low", "moderate", "high"))
boston[["lstat"]] <- ordered(cut(boston[["lstat"]], c(0, 6, 12, 17, 38)) ,
                           labels=c("very-low-poverty", "low-poverty", "moderate", "high-poverty"))
boston[["medv"]] <- ordered(cut(boston[["medv"]], c(5, 15, 25, 50)) ,
                             labels=c("low", "moderate", "high"))

apply(boston,2,table)
apply(boston,2,class)


# Convert to a binary incidence matrix
t_boston <- as(boston, "transactions")
summary(t_boston)

quartz()
itemFrequencyPlot(t_boston, support = 0.05, cex.names= 0.8, main="support = 0.05")

quartz()
itemFrequencyPlot(t_boston, support = 0.1, cex.names= 0.8, main="support = 0.1")


b_rules <- apriori(t_boston, parameter = list(support=0.05, confidence=0.6))
rulesCrimeLow <- subset(b_rules, subset = rhs %in% "dis=close" & lift>1.05)
rulesCrimeLow
inspect(head(sort(rulesCrimeLow, by = "support"), n=20))
inspect(rulesCrimeLow)
write(rulesIncomeSmall, file="~/Google Drive/Data Mining II/small_rules.csv", 
      sep=",", col.names=NA)



rulesPTLow <- subset(b_rules, subset = rhs %in% "ptratio=low" & lift>1.05)
inspect(head(sort(rulesPTLow, by = "confidence"), n=20))
inspect(head(sort(rulesPTLow, by = "support"), n=20))
