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
  
  return(return)
  
}

bic_comp <- function(var){
  n <- length (var)
  bic_all <- matrix(, nrow = (n-1), ncol = 2)
  bic_all[, 1] <- 1:(n-1)
  
  for(i in 1:(n-1)){
    bic_all[i, 2] <- BIC(autoregress(var, i)$lm)
  }
  
  bic_all <- as.data.frame(bic_all)
  
  graph <- bic_all |> ggplot(aes(x = V1, y = V2)) +
     geom_point()
  
  return <- list(
    bic_all = bic_all,
    graph = graph
  )
  
  return(return)
  
}