generate_data <- function(n) {
  x1 <- runif(100, 0, 1000)
  x2 <- runif (100, 0, 1000)
  x3 <- runif (100, 100, 1300)
  y <- 5*x1 + 100*x2 - 7*(x3)
  return(data.frame(x1=x1,
                    x2=x2,
                    x3=x3,
                    y=y))
}

data <- generate_data(1000)

estimate <- lm(data=data, y~x1+x2+x3) 

evaluate <- function(est, m) {
  return(c(est[1] < m && m < est[2], est[2] - est[1]))
}
