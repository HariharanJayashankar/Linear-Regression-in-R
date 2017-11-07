rm(list = ls())

x0 <- rep.int(1, 50)
x1 <- sample(100:1000, 50)
Y <- as.matrix(rnorm(50, 2, 4))

X <- as.matrix(cbind(x0, x1))

B <- matrix(, nrow = 2, ncol = 50)

#Y <- XB #This is our basic linear regression model where Y and X are matrices and B is our parameter

#We need to find the values of the parameter matrix B which is 2x1

#First we need to define our loss function

#We can scale the data

X.scaled <- X
X.scaled[, 2] <- (X[,2] - mean(X[,2]))/sd(X[,2])

#1) Using the usual OLS method
#Do some linear algebra magic
#Our loss function is going to be SSE
XX_inv <- solve( t(X.scaled) %*% X.scaled)

B <- ((XX_inv) %*% t(X.scaled) %*% Y)

#2) Using gradient descent

m <- nrow(X)

for(i in 1:1000){
    
    B <- matrix( c(0, 0), nrow = 2, ncol = 1)
    
    B <- B + (0.5)*(((1/m) * 2 * t(X.scaled)) %*% (Y - ((X.scaled)%*%B)))
    
        if (identical ((Y - (X.scaled %*% B)), as.matrix( rep(50), nrow = 50, ncol = 1))) {
            break    
        }
}

