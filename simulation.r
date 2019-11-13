install.packages("pbapply")
install.packages("dplyr")
library("pbapply")
library("dplyr")

generate_data <- function(n, alpha1, alpha2, alpha3) {
  x1 <- runif(100, 0, 1000)
  x2 <- runif (100, 0, 1000)
  x3 <- runif(100, 0, 1300)
  y <- alpha1*x1 + alpha2*x2 + alpha3*x3
  return(data.frame(x1=x1,
                    x2=x2,
                    x3=x3,
                    y=y))
}
 

estimate <- function(data) {
  ls <- lm(y ~ x1 + x2 + x3, data = data)
  coef <- ls$coefficients
  return(coef)
}

evaluate <- function(est, alpha1, alpha2, alpha3) {
  return(c(alpha1 - est[1],  alpha2 - est[2], alpha3 - est[3]))
}

iteration <- function(alpha1, alpha2, alpha3) {
  data <- generate_data(alpha1, alpha2, alpha3)
  est <- estimate(data)
  return(evaluate(alpha1, alpha2, alpha3))
}

simulation <- function(n, alpha1, alpha2, alpha3) {
  result <- pbsapply (1:n, function (x) {
    iteration(alpha1, alpha2, alpha3)
  })
  rowMeans(result)
  
}


simulation(100, 10, 13, 3)

