rm(list=ls())

jaccard <- function(x1, x2){
  aib <- sum(x1[x1==x2])
  aub <- sum(x1+x2)-aib
  round(1-(aib/aub), 3)
}

cosine <- function(x1, x2){
  ab <- x1*x2
  ab <- sum(ab[!is.na(ab)])
  AA <- x1^2
  AA <- sum(AA[!is.na(AA)])
  BB <- x2^2
  BB <- sum(BB[!is.na(BB)])
  round(ab/(sqrt(AA)*sqrt(BB)), 3)
}

# original matrix
A=c(4, 5, NA, 5, 1, NA, 3, 2)
B=c(NA, 3, 4, 3, 1, 2, 1, NA)
C=c(2, NA, 1, 3, NA, 4, 5, 3)

mat0 <- as.matrix(rbind(A,B,C), dimnames = list(c("A","B", "C")))
colnames(mat0) <- c(letters[1:8])
mat0

#  boolean matrix 1
A1=c(T, T, F, T, T, F, T, T)
B1=c(F, T, T, T, T, T, T, F)
C1=c(T, F, T, T, F, T, T, T)

mat1 <- as.matrix(rbind(A1,B1,C1), dimnames = list(c("A","B", "C")))
colnames(mat1) <- c(letters[1:8])
mat1

ab.j1 <- jaccard(A1, B1)
ac.j1 <- jaccard(A1, C1)
bc.j1 <- jaccard(B1, C1)
matrix(c(ab.j1, ac.j1, bc.j1, bc.j1,), nrow = 2, ncol = 2, 
       dimnames=list(c("A","B"),c("B", "C") ))

AB_cos <- cosine(mat1[1,], mat1[2,])

#      a  b  c  d  e  f  g
eA =c(4, NA, NA, 5, 1, NA, NA)
eB =c(5, 5, 4, NA, NA, NA, NA)

cosine(eA, eB)

#    a  b  c  d  e  f  g  h
A2=c(1, 1, 0, 1, 0, 0, 1, 0)
B2=c(0, 1, 1, 1, 0, 0, 0, 0)
C2=c(0, 0, 0, 1, 0, 1, 1, 1)

mat2 <- as.matrix(rbind(A2,B2,C2), dimnames = list(c("A","B", "C")))
colnames(mat1) <- c(letters[1:8])

jac_mat2 <- dissimilarity(mat2, method = "jaccard")
cos_mat2 <- dissimilarity(mat2, method = "cosine")


#Normalize mat0
norm <- function(x){
  m <- mean(x, na.rm=T)
  x - m
}

# normalized mat 0
mat0_n <- t(round(apply(mat0, 1, norm),3))
mat0_n

An <- mat0_n[1,]
Bn <- mat0_n[2,]
Cn <- mat0_n[3,]

#Cosine similarities for mat0_n
ab.cn <- cosine(An, Bn)
ac.cn <- cosine(An, Cn)
bc.cn <- cosine(Bn, Cn)
list("Cosine similarities between users for normalized matrix mat0_n",
     matrix(c(ab.cn, 1, ac.cn, bc.cn), nrow = 2, ncol = 2, 
            dimnames=list(c("A","B"),c("B", "C") )))