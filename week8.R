generate_data <- function(n) {
  x1 <- runif(100, 0, 1000)
  x2 <- runif (100, 0, 1000)
  x3 <- runif (100, 100, 1300)
  y <- 5*x1 + 100*x2 - (x3)^2
  return(data.frame(x1=x1,
                    x2=x2,
                    x3=x3,
                    y=y))
}

data <- generate_data(10000)