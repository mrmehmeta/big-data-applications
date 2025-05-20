library(tidyverse)
library(zoo)
library(lme4)
library(forecast)
library(glmnet)

autoregress <- function(var, p){
  n <- length(var)
  y <- var[-c(1:p)]
  
  x <- matrix(, nrow = (n-p), ncol = p)
  
  for(i in 1:p){
    x[,i] <- var[(p+1-i):(n-i)]
  }
  
  list <- list(
    y = y,
    x = x
  )
  
  return(list)
  
}

autoregress_lm <- function(var, p){
  return <- lm(autoregress(var, p)$y ~ autoregress(var, p)$x)
  
  return(return)
  
}

bic_ar <- function(var, min = 1, max = (length(var)-1)){
  n <- length(var)
  bic_all <- matrix(, nrow = max, ncol = 2)
  bic_all[, 1] <- min:max
  
  for(i in min:max){
    bic_all[i, 2] <- BIC(autoregress_lm(var, i))
  }
  
  bic_all <- as.data.frame(bic_all)
  
  graph <- bic_all |> ggplot(aes(x = V1, y = V2)) +
     geom_point()
  
  list <- list(
    bic_all = bic_all,
    graph = graph
  )
  
  return(list)
  
}

multivar <- function(y, vars, opt, lambda){
  n <- length(vars)
  y <- y[-c(1:p)]
  x <- matrix(, nrow = (n-1), ncol = (n+1))
  
  x[,1] <- y[1:(n-1)]
  
  for(i in 1:n){
    x[,(i+1)] <- vars[[i]][1:(n-1)]
  }
  
  if(opt == "lm"){
    result <- lm(y ~ x)
  }
  else if(opt == "lasso"){
    result <- glmnet(x, y, alpha = 1, lambda = lambda)
  }
  else if(opt == "ridge"){
    result <- glmnet(x, y, alpha = 0, lambda = lambda)
  }
  
  return(result)

}