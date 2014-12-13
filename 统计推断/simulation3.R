# #Q1                    
# CC = 0.4*(pnorm(15) - pnorm(-5)) + 0.6*(pnorm(15,8,2) - pnorm(-5,8,2))
# C = 1/CC
# 
# f <- function(x){
#   if(x < -5 || x > 15){
#     y <- 0
#   }
#   else{
#   y <- C*(0.4*(dnorm(x)) + 0.6*(dnorm(x,8,2)))
#   }
# }
# 
# f = Vectorize(f)
# 
# X1 <- runif(360,-5,15)               
# Y <- runif(360,0,0.16)
# X1 <- X1[Y < f(X1)]
# X1 <- X1[1:100]
# plot(f, -5, 15)
# hist(X1,breaks = 20,prob = T,add = T)
# X2 <- runif(3600,-5,15)               
# Y <- runif(3600,0,0.16)
# X2 <- X2[Y < f(X2)]
# X2 <- X2[1:1000]
# plot(f, -5, 15)
# hist(X2,breaks = 20,prob = T,add = T)
# 
# #Q3
# fhat1 <- function(x){
#   XX = X1[X1 < x + 0.5 & X1 > x - 0.5]
#   length(XX)/(2*100*0.5)
# }
# 
# fhat1 <- Vectorize(fhat1)
# S <- c()
# T = -5 + 20*(1:100)/101
# for(i in 1:1000){
#   X1 <- runif(360,-5,15)               
#   Y <- runif(360,0,0.16)
#   X1 <- X1[Y < f(X1)]
#   X1 <- X1[1:100]
#   S[i] <- sum((fhat1(T) - f(T))^2)/100
# }

plot(fhat1,-5,15)
plot(f,-5,15,add = T)