generate_data <- function(n, alpha1, alpha2, alpha3) {
  x1 <- runif(100, 0, 1000)
  x2 <- runif (100, 0, 1000)
  x3 <- runif (100, 100, 1300)
  y <- alpha1*x1 + alpha2*x2 + alpha3*(x3)^2
  return(data.frame(x1=x1,
                    x2=x2,
                    x3=x3,
                    y=y))
}

data <- generate_data(10000, 5, 7, -1)

evaluate <- function(est, ) {
  return(c(alpha1 - est[1],  alpha2 - est[2], alpha1 - est[2]))
}