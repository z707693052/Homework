#Q1                    
CC = 0.4*(pnorm(15) - pnorm(-5)) + 0.6*(pnorm(15,8,2) - pnorm(-5,8,2))
C = 1/CC

f <- function(x){
    y <- C*(0.4*(dnorm(x)) + 0.6*(dnorm(x,8,2)))
}

#Q2

X1 <- runif(350,-5,15)               
Y <- runif(350,0,0.16)
X1 <- X1[Y < f(X1)]
X1 <- X1[1:100]
plot(f, -5, 15)
hist(X1,breaks = 20,prob = TRUE,add = TRUE)
X2 <- runif(3500,-5,15)               
Y <- runif(3500,0,0.16)
X2 <- X2[Y < f(X2)]
X2 <- X2[1:1000]
plot(f, -5, 15)
hist(X2,breaks = 20,prob = TRUE,add = TRUE)

#Q3

T = -5 + 20*(1:100)/101

H <- function(X){
  n <- length(X)
  IQX <- sort(X)[n*0.75]
  h <- 0.9*n^(-0.2)*min(sqrt(var(X)),IQX/1.349)
  return(h)
}


AISE <- function(X){
  h <- H(X)
  n = length(X)
  fhat <- function(t){
    Xh = X[X < t + h & X > t - h]
    length(Xh)/(2*h*n)
  }
  fhat <- Vectorize(fhat)
  AISE <- sum((fhat(T) - f(T))^2)/100
  return(AISE)
}

simulation <- function(n){
  X1 <- runif((n*3400),-5,15)               
  Y <- runif((n*3400),0,0.16)
  X1 <- X1[Y < f(X1)]
  X1 <- X1[1:(n*1000)]
  X1_mat <- matrix(X1,nrow = 1000,ncol = n)
  AISEs <- apply(X1_mat,1,AISE)
  RAISEs <- sqrt(AISEs)
  M <- mean(RAISEs)
  SD <- sd(RAISEs)
  c(M,SD)
}

s1 = simulation(100)
m1 = s1[1]
sd1 = s1[2]

s2 = simulation(1000)
m2 = s2[1]
sd2 = s2[2]
