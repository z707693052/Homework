MLE <- function(x){
  mean(x)^2
}
JK <- function(x){
  n = length(x)
  n*mean(x)^2/(n - 1) - mean(x)/(n - 1)
}


X <- rbinom(100000,1,0.5)
X_mat <- matrix(X,nrow = 10000 , ncol <- 10)
MLEs <- apply(X_mat,1,MLE)
m11 = mean(MLEs)
JKs <- apply(X_mat,1,JK)
m12 = mean(JKs)

X <- rbinom(100000,1,0.7)
X_mat <- matrix(X,nrow = 10000 , ncol <- 10)
MLE2s <- apply(X_mat,1,MLE)
m21 = mean(MLE2s)
JK2s <- apply(X_mat,1,JK)
m22 = mean(JK2s)

X <- rbinom(100000,1,0.3)
X_mat <- matrix(X,nrow = 10000 , ncol <- 10)
MLE3s <- apply(X_mat,1,MLE)
m31 = mean(MLE3s)
JK3s <- apply(X_mat,1,JK)
m32 = mean(JK3s)


