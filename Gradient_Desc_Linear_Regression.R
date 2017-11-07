#Trying to create the code for gradient descent
rm(list = ls())

x0 <- rep.int(1, 50)
x1 <- sample(100:1000, 50)
Y <- as.matrix(rnorm(50, 2, 4))

X <- as.matrix(cbind(x0, x1))

B <- matrix(, nrow = 2, ncol = 50)

X.scaled <- X
X.scaled[, 2] <- (X[,2] - mean(X[,2]))/sd(X[,2])


grad.desc <- function(x, y, alpha = 0.5, k = 1000) {
        
        m <- nrow(x)
        
        for (i in 1:k) {
                B <- matrix( rep(0, ncol(x)), nrow = ncol(x), ncol = 1)
                
                B <- B - (alpha)*(((1/m) * 2 * t(x)) %*% (((x)%*%B) - Y))
                
                if (identical ((y - (x %*% B)), as.matrix( rep(0, nrow(x)), nrow = nrow(x), ncol = 1))) {
                        break    
                }
                
                
        }
        return(B)
}