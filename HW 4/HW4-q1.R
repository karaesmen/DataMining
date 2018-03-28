###############################
### Homework 4 - Question 1 ###
###############################
rm(list=ls())
library(gRain)
library(gRbase)
library(ggm)

## Consider the "cad1" dataset in the package gRbase. 
## There are 236 observations on fourteen variables from the Danish Heart Clinic
## A structural learning algorithm has identified the "optimal network" as given below.
## For simplicity, not all variables in the networks are represented

## Load CAD Data
data(cad1)

# ##### PART A ######
# ## Construct this network in R and infer the Conditional Probability Tables using the cad1 data.
# ny <- c("no", "yes")
# table(cad$Sex)  # are they males?      # no - 47 ; yes - 189
# table(cad$Smoker)                      # no - 51 ; yes - 185
# table(cad$Inherit)                     # no - 162; yes - 74
# table(cad$Hyperchol)                   # no - 108; yes - 128
# table(cad$SuffHeartF)                  # no - 167; yes - 69
# table(cad$CAD)                         # no - 129; yes - 107
# 
# ## Construct CPT tables
# sex <- cptable(~sex, values=c(47,189), levels=ny)
# smoker.sex <- cptable(~smoker|sex, values=c(51,185,47,189), levels=ny)
# shf <- cptable(~suffheartf, values=c(167,69), levels=ny)
# inherit.smoker <- cptable(~inherit|smoker, values=c(162,74,51,185), levels=ny)
# hc.shf.smoke <- ortable(~hyperchol+suffheartf+smoker, levels=ny)
# cad.hc.inherit <- ortable (~cad+hyperchol+inherit, levels=ny)
# 
# ## Compile CPT tables
# plist <- compileCPT(list(sex, smoker.sex, shf, inherit.smoker, hc.shf.smoke, cad.hc.inherit))
# net1 <- grain(plist) #'grain' builds a graphical independence network
# dev.off()
# plot(net1)


###### Part A ######
DAG.opt <- dag(~CAD:Inherit:Hyperchol + Hyperchol:SuffHeartF + Inherit:Smoker + Hyperchol:Smoker + Smoker:Sex)
CPT <- extractCPT(cad1, DAG.opt) #smooth = 0.01) #smooth = 0.01 to avoid 0s in the CPTs
CPTlist <- compileCPT(CPT)
cad.DAG <- grain(CPTlist)
dev.off()
plot(cad.DAG)

kable()
CPTlist$CAD
CPTlist$Sex

## Identify any d-separations in the graph
## Specify the DAG
CAD.netw <- list(~Sex, ~Smoker|Sex, ~Inherit|Smoker, ~SuffHeartF, ~Hyperchol|Suffheartf:Smoker, ~CAD|Hyperchol:Inherit)
## To use dSep it needs to be the cadg needs to be as an adjacency matrix
CAD.nw <- dagList(CAD.netw, "matrix")


dSep(CAD.nw, "Inherit", "Sex", NULL) 
dSep(CAD.nw, "Inherit", "Sex", "Smoker") #inherit and sex are d-separated given evidence of smoker
dSep(CAD.nw, "Inherit", "Smoker", "Sex") #inherit and sex are d-separated given evidence of smoker
dSep(CAD.nw, "CAD", "Smoker", c("Inherit", "Hyperchol"))
dSep(CAD.nw, "CAD", "Smoker", "Hyperchol")
dSep(CAD.nw, "CAD", "SuffHeartF", c("Inherit", "Hyperchol"))
dSep(CAD.nw, "CAD", "SuffHeartF", c("Inherit", "Hyperchol"))

dSep(CAD.nw, "Hyperchol", "SuffHeartF", "Sex") #Hyperchol and Sex are d-separated given evidence of smoker
dSep(CAD.nw, "Smoker", "CAD", c("Inherit", "Hyperchol")) #Smoker and CAD are d-separated given evidence of inherit/hyperchol
dSep(CAD.nw, "SuffHeartF", "Smoker", "Hyperchol") #SuffHeartF and CAD are d-separated given evidence of Hyperchol

dSep(CAD.nw, "SuffHeartF", "Smoker", "CAD")

dSep(CAD.nw, "CAD", "Hyperchol", "SuffHeartF")
dSep(CAD.nw, "CAD", "Inherit", c("SuffHeartF", "Smoker"))
dSep(CAD.nw, "CAD", "Inherit", c("SuffHeartF", "Smoker"))

dSep(CAD.nw, "Inherit", "Hyperchol", "Smoker") #inherit and hyperchol are d-separated given information about smoker


##### PART B #####
## Suppose it is known that a new observation is female with hypercholesterolemia (high cholesterol)
## Absorb this evidence into the graph and revise the probabilities
## How does the probability of heart-failure and CAD change after this info is taken into account?

cad.newEvid <- setEvidence(cad.net, evidence=list(Sex="Female", Hyperchol="Yes"))
querygrain(cad.DAG)$CAD 
## CAD probability is 54% no and 46% yes
querygrain(cad.DAG)$SuffHeartF 
## Heart failure probability is 70.7% and 29.3%
querygrain(cad.newEvid)$CAD 
## CAD probability is 39.3% no and 60.7% yes with new evidence
querygrain(cad.newEvid)$SuffHeartF 
## Heart-failure probability is 61.7% no 38.3% yes with new evidence


##### PART C #####
## Simulate a new data set with new observations conditional upon this new evidence (part B).
## Save this new data as *.txt and submit it with your assignment
## Use the new data to determine the joint distribution of "smoker" and "cad" given this evidence
## (hint: use "simulate in the gRain packages (for help: ?simulate.grain))
#Simulation

new.cad <- simulate.grain(cad.newEvid, nsim = 5000)
head(new.cad)
write.table(new.cad, file="newCAD.txt")
#joint distribution of "Smoker" and "CAD" given the new evidence
new <- querygrain(cad.newEvid, nodes=c("Smoker", "CAD"), type="joint", result = "data.frame")
new$Freq <- round(new$Freq, 3)
new

old <- querygrain(cad.DAG, nodes=c("Smoker", "CAD"), type="joint", result = "data.frame")
old$Freq <- round(old$Freq, 3)
old

