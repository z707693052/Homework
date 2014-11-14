f <- function(x){                    #define the pdf f
  if (0 <= x && x< 0.5){
    y = 4*x/3
  }
  else if (0.5 <= x && x< 1){
    y = (4/3)*(1 - x)
  }
  else if (1 <= x && x< 1.5){
    y = (8.0/3)*(x - 1)
  }
  else if (1.5 <= x && x<= 2){
    y = (2.0/3)*(8-4*x)
  }
  else 
  {y = 0}
  y
}
f = Vectorize(f)                         
FF <- function(x){                     #define the cdf
  if (0 <= x && x< 0.5){
    y = 2*x^2/3
  }
  else if (0.5 <= x && x< 1){
    y = 1/3 - 2*(x-1)^2/3
  }
  else if (1 <= x && x< 1.5){
    y = 1/3 + 4*(x - 1)^2/3
  }
  else if (1.5 <= x && x<= 2){
    y = 1 - 4*(x - 2)^2/3
  }
  else if(x < 0){
    y = 0
  }
  else
  {y = 1}
  y
}
FF = Vectorize(FF)

invF <- function(x){                     #define the invers cdf
  if (0 <= x && x< 1/6){
    y <- sqrt(3*y/2)
  }
  else if (1/6 <= x && x< 1/3){
    y <- 1 - sqrt((1 - 3*y)/2)
  }
  else if (1/3 <= x && x< 2/3){
    y <- 1  sqrt(y - 1)/2
  }
  else if (2/3 <= x && x<= 1){
    y <- 2 + sqrt(3*(1 - y))/2
  }
  y
}
invF = Vectorize(invF)
plot(invF,0,1)

integrate(function(x) f(x)*x, 0, 2)              #solve Q2
integrate(function(x) f(x)*(x - 7/6)^2, 0, 2)

U = runif(2000)
X1 = invF(U)
mu1 <- mean(X1)
plot(f, 0, 2)
hist(X1,breaks = seq(0,2,0.02),freq = FALSE ,add = TRUE)

i <- 0
X2 <- c()
while (TRUE){                      #Accept-Reject
  x <- runif(1,0,2)
  y <- runif(1,0,4/3)
  if (y < f(x) ){
    X2[i] <- x
    i <- i + 1
  }
  if (i == 2000)
    break
}
mu2 <- mean(X2)
plot(f, 0, 2)
hist(X2,breaks = seq(0,2,0.02),freq = FALSE ,add = TRUE)
