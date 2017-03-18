rm(list=ls(all=T))

### set the working directory using setwd fucntion

prod_rate <- read.csv("ProductRatings.csv",header = T, sep = ",")

# install.packages("tidyr") install this if you dont have.
require(tidyr)
prod_rate1 <- spread(prod_rate,Product,Rating)
prod_rate1 <- prod_rate1[,-1]

### apply svd
svd <- svd(prod_rate1)
u <- svd$u
S <- diag(svd$d)
v <- svd$v
vt <- t(v)

prodrate1_hat = u %*% S %*% vt

eigenval = svd$d
e_sqare_energy = (eigenval/sum(eigenval))*100
cumsum(e_sqare_energy)

### 17 dimenstions explain much variation
svd <- svd(prod_rate1,nu=17,nv=17)
S <- diag(svd$d[1:17])
svd$u %*% S %*% t(svd$v)

### test user rating
Newuserratings = c(1,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
b <-matrix(Newuserratings,byrow=T, ncol=19)
S1 <- diag(1/svd$d[1:17])

newuserrating = b %*% svd$v %*% S1 

### correlation

# Creating function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  cosine <- abs(sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y))))
  return(cosine)
}

## user based recommendation
distances = apply(svd$u,1,getCosine,newuserrating) 
userid = which((distances==max(distances))==TRUE)
similaruser = prod_rate1[userid,]
Newuserratings
zeroposofNewUser = which((Newuserratings==0)==TRUE)
recom = which(similaruser ==max(similaruser[zeroposofNewUser]))
recom
