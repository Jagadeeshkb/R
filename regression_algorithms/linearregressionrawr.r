ols<-function(y,x){
    data<-model.matrix(y ~ x, data = x)
    decomp <- svd(data)
    
    return(decomp$v %*% diag(1 / decomp$d) %*% t(decomp$u) %*% y)
  }

set.seed(1)
x <- rnorm(1000)
y <- 4 * x + rnorm(1000, sd = .5)
ols(y=y,x=matrix(x, ncol = 1))

svd_vec = svd(matrix(x,ncol=1))
str(svd_vec)
library(lobstr)
diag(svd_vec$d)

svd_vec$v %*% diag(1 / svd_vec$d) %*% t(svd_vec$u) %*% y

