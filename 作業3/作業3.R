#使結果可重現
set.seed(123)

#樣本大小
n = 100

X1 = c()

X2 = c()

X3 = c()

for(i in 1:n){
  #產生x1
  u1 = runif(1,0,1)
  
  F1 <- function(x) 3*x- 3*x^2 + x^3 - u1
  
  x1 = uniroot(F1, c(0, 1))$root   
  
  #產生x2
  u2 = runif(1,0,1)
  
  F2_1 <- function(x) 1-((1-x)/(1-x1))^2 - u2
  
  x2_1 = uniroot(F2_1, c(x1, 1))$root 
  
  #產生x3
  x3_12 = runif(1,x2_1,1)
  
  X1[i] = x1
  X2[i] = x2_1
  X3[i] = x3_12
}

hist(X1, breaks = 20, main = "Histogram of X1", col = "skyblue", xlab = "X1")
hist(X2, breaks = 20, main = "Histogram of X2", col = "salmon", xlab = "X2")
hist(X3, breaks = 20, main = "Histogram of X3", col = "lightgreen", xlab = "X3")


mean(X1)








