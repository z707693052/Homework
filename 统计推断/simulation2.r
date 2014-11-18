f <- function(x){                    # define the pdf f
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
FF <- function(x){                     # define the cdf
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

invF <- function(x){                     # define the invers cdf
  if (0 <= x && x< 1/6){
    y <- sqrt(3*x/2)
  }
  else if (1/6 <= x && x< 1/3){
    y <- 1 - sqrt((1 - 3*x)/2)
  }
  else if (1/3 <= x && x< 2/3){
    y <- 1 + sqrt(3*x - 1)/2
  }
  else if (2/3 <= x && x<= 1){
    y <- 2 - sqrt(3*(1 - x))/2
  }
  y
}
invF = Vectorize(invF)

plot(f,0,2)
plot(FF,0,2,ylab = 'F')

mu = integrate(function(x) f(x)*x, 0, 2)              # solve Q2
sigma = integrate(function(x) f(x)*(x - 7/6)^2, 0, 2)


U = runif(1000)                                  # Direct method
X1 = invF(U)
mu1 <- mean(X1)
plot(f, 0, 2,main = 'Direct method')
hist(X1,breaks = 20,freq = FALSE ,add = TRUE)
                                    
X2 <- runif(2800,0,2)                 # Accept-Reject
Y <- runif(2800,0,4/3)
X2 <- X2[Y < f(X2)]
X2 <- X2[1:1000]
mu2 <- mean(X2)
plot(f, 0, 2, main = 'Accept-Reject')
hist(X2,breaks = 20,freq = FALSE ,add = TRUE)

print('wait...')

#repeat Direct method ,using Vectorize and the apply function to speed up.
#using proc.time to record the time consumption
stime <- proc.time()
X1_mat <- matrix(invF(runif(1000*2000)),nrow = 2000 , ncol = 1000)  
Mu1 = apply(X1_mat,1,mean)
proct1 <- proc.time() - stime

#repeat Accept-Reject method,
stime <- proc.time()
X2 <- runif((2670*2000),0,2) 
Y <- runif((2670*2000),0,4/3)
X2 <- X2[Y < f(X2)]
X2 <- X2[1:(1000*2000)]
X2_mat <- matrix(X2,nrow = 2000 , ncol = 1000)  
Mu2 = apply(X2_mat,1,mean)
proct2 <- proc.time() - stime

#relation between mu_bar and mu
Mu_1 = sqrt(1000)*(Mu1 - 7/6)/sqrt(19/72)
plot(dnorm, -3, 3,main = 'Direct method')
hist(Mu_1,breaks = 30,freq = FALSE ,add = TRUE)
Mu_2 = sqrt(1000)*(Mu2 - 7/6)/sqrt(19/72)
plot(dnorm, -3, 3,main = 'Accept-Reject')
hist(Mu_2,breaks = 30,freq = FALSE ,add = TRUE)
