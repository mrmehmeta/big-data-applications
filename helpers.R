library(tidyverse)
library(zoo)
library(lme4)
library(forecast)

autoregress <- function(var, p){
  n <- length(var)
  y <- var[-c(1:p)]
  
  x <- matrix(, nrow = (n-p), ncol = p)
  
  for(i in 1:p){
    x[,i] <- var[(p+1-i):(n-i)]
  }
  
  lm <- lm(y ~ x)
  
  return <- list(
    y = y,
    x = x,
    lm = lm
  )
}