###############################
### Homework 4 - Question 4 ###
######### Abbas Rizvi #########
###############################
rm(list=ls())
# setwd("/Users/aarizvi/Google Drive/STA546/hw4/")

## The sinking of the Titanic is a famous event in history
## The titanic data (UB Learns) was collected by the British Board of Trade to investigate the sinking.
## Many well-known facts--from the proportions of first-class passengers to the "women and children" first policy,
## and the fact that the policy was not entirely successful in saving the women and children in third class--
## are reflected in the survival rates for various classes of passengers
## You have been petitioned to investigate this data
## Analyze this data with tool(s) that we learned in STA546
## Summarize your findings for British Board of Trade



## Load data
load("./hw4/titanic.raw-2.rdata")
dim(titanic.raw)
head(titanic.raw)

fem3 <- titanic.raw[which(titanic.raw$Sex == "Female"& titanic.raw$Class == "3rd"), ]
table(fem3$Age, fem3$Survived)
table(titanic.raw$Sex)

## Convert to a binary incidence matrix
library(arules)
titanic <- as(titanic.raw, "transactions")
summary(titanic)
as(head(titanic,5), "matrix")

## itemFrequencyPlot
itemFrequencyPlot(titanic, support = 0.01, cex.names = 0.8)

## Apply the apriori algorithm
rules  <- apriori(titanic, parameter = list(support = 0.01, confidence = 0.2))
rulesL1 <- subset(rules, subset = lift > 1.1)


## take a closer look at differences
summary(rulesL1)
inspect(rules)


## To select interesting rules from the set of all possible rules, constraints on various measures of significant/interest can be used
## The best known constraints are minimum thresholds on support and confidence
## The support of an itemset is defined as the proportions of transactions in the data set which contain the item set.
## The confidence of a rule is defined as confidence(X -> Y) = support(X OR Y)/support(X). 
## For example rule {milk, bread} -> {butter} has a confidence of 0.2/0.4 = 0.05 in the database
## This means that for 50% of the transactions containing milk and bread the rule is correct
## Confidence can be interpreted as an estimate of the probability P(Y|X), 
## the probability of finding the RHS of the rule in transactions under the condition these transactions also contain the LHS


## Association rules are required to satisfy both a minimum support and a minimum cofidence constraint at the same time. 

## Lift -- solution to the problem of finding too many association rules satisfying the support and confidence constraints
## Lift -- filter or rank found rules using additional interest measures
## Lift -- lift(X->Y) = support(X or Y)/(support(X)*support(Y))
## can be intrepreted as the deviation of the support of the whole rule from the support expected under independence given the supports
## Of LHS and RHS. Greater lift value indicate stronger assocations
## Lift < 1 is bad

## A priori is a level-wise, breadth-first algorithm which counts transactions


## Is there evidence that "women and children" were the first evacuated?

survivors.fem <- subset(rules, subset = rhs %in% "Survived=No" & lift > 1) # & lhs %in% "Sex=Female")
survivors.child <- subset(rules, subset = lift > 1.2 & lhs %in% "Age=Child")
inspect(sort(survivors.fem, by = "lift"))
inspect(sort(survivors.child, by = "lift"))
#Yes, considering that only women and children are represented in the rules for RHS = "Survived=Yes"
#Female survived = 0.16 support; confidence 0.73; lift 2.2
#Class = 1st, Sex = Female -> Survived = Yes: Support: 0.

child <- subset(rules, subset = rhs %in% "Age=Child") #no rules
inspect(sort(child, by = "lift"))
women <- subset(rules, subset = rhs %in% "Sex=Female")
inspect(sort(women, by = "lift"))

men <- subset(rules, subset = rhs %in% "Sex=Male")
inspect(sort(men, by = "lift"))

dead <- subset(rules, subset = rhs %in% "Survived=No")
inspect(sort(dead, by = "lift"))

## What characteristics/demographics are more likely in surviving passengers?



## How does your results support the popular movie "Titanic?"

## For example, what is the probability that Rose (1st class adult and female) and Jack (3rd class adult and male) would not survive?
#163 {Class=3rd,Sex=Male,Age=Adult}   => {Survived=No} 0.175829169 0.8376623  1.2373791






