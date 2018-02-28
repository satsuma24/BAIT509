library(tidyverse)
library(ggplot2)

genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y) # dataframe
}

genreg(10)

dat <- genreg(1000)

dat <- mutate(dat,
       yhat = 5,
       yhat1 = 5-x1,
       yhat2 = 5+2*x2,
       yhat12 = 5-x1+2*x2)
dat


mse <- mean((dat$yhat-dat$y)^2)
mse1 <- mean((dat$yhat1-dat$y)^2)      
mse2 <- mean((dat$yhat2-dat$y)^2)      
mse12 <- mean((dat$yhat12-dat$y)^2)
mse
mse1
mse2
mse12

# category
gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-0.2-x)))
  tibble(x=x, y=y)
}
gencla(10)

# X = 1
pA = 0.2
pB = 0.8/(1+exp(-1)) # mode is the prediction
pC = 1-pB-pA

# X = -2
pA = 0.2
pB = 0.8/(1+exp(2)) # mode is the prediction
pC = 1-pB-pA

dat2 = gencla(1000)
dat2

sapply(dat2$x)
dat2 = mutate(dat2, 
              yhat = sapply(x, function(x_) if (x_<0)"C" else "B"))
dat2

1- mean(dat2$yhat == dat2$y)
