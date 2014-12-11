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

X1 <- runif(360,-5,15)               
Y <- runif(360,0,0.16)
X1 <- X1[Y < f(X1)]
X1 <- X1[1:100]
plot(f, -5, 15)
hist(X1,breaks = 15,prob = T,add = T)
X2 <- runif(3600,-5,15)               
Y <- runif(3600,0,0.16)
X2 <- X2[Y < f(X2)]
X2 <- X2[1:100]
plot(f, -5, 15)
hist(X2,breaks = 15,prob = T,add = T)