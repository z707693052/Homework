#Q1                    
CC = 0.4*(pnorm(15) - pnorm(-5)) + 0.6*(pnorm(15,8,2) - pnorm(-5,8,2))
C = 1/CC

f <- function(x){
  if(x < -5 || x > 15){
    y <- 0
  }
  else{
  y <- C*(0.4*(dnorm(x)) + 0.6*(dnorm(x,8,2)))
  }
}

f = Vectorize(f)

X1 <- runif(350,-5,15)               
Y <- runif(350,0,0.16)
X1 <- X1[Y < f(X1)]
X1 <- X1[1:100]
# plot(f, -5, 15)
# hist(X1,breaks = 20,prob = T,add = T)
# X2 <- runif(3600,-5,15)               
# Y <- runif(3600,0,0.16)
# X2 <- X2[Y < f(X2)]
# X2 <- X2[1:1000]
# plot(f, -5, 15)
# hist(X2,breaks = 20,prob = T,add = T)
# 
#Q3
fnum <- function(x,h,data){
  XX = X1[data < x + h & data > x - h]
  return(length(XX))
}


S <- c()
T = -5 + 20*(1:100)/101

H <- function(X){
  n <- length(X)
  IQX <- sort(X)[n*0.75]
  h <- 0.9*n^(-0.2)*min(sqrt(var(X)),IQX/1.349)
  return(h)
}


AISE <- function(X){
  h <- H(X)
  n <- length(X)
  fhat <- function
}
# 

  X1 <- runif(350000,-5,15)               
  Y <- runif(350000,0,0.16)
  X1 <- X1[Y < f(X1)]
  X1 <- X1[1:100000]
  X1_mat <- matrix(X1,nrow = 1000,ncol = 100)
  H1 <- apply(X1_mat,1,H)


# plot(fhat1,-5,15)
# plot(f,-5,15,add = T)