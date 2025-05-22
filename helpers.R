library(tidyverse)
library(zoo)
library(lme4)
library(forecast)
library(glmnet)
library(modelsummary)

bic <- function(model, p = (model$rank - 1)) {
  data <- (model$residuals + model$fitted.values)
  ssr <- sum((model$residuals)^2)
  t <- length(data)
  return(log((ssr / t)) + ((p + 1) * (log(t) / t)))
}

bic_ridge <- function(model, lambda) {
  data <- (model$residuals + model$fitted.values)
  ssr <- sum((model$residuals)^2)
  sigma_squared <- (var(model$residuals))^2
  n <- length(data)
  d_lambda <- t(data %*% solve(data %*% t(data) + lambda %*% solve(data)) %*% solve(data))
  return(ssr + (log(n) / n) * d_lambda * sigma_squared)
}

# bic_lasso <- function(model) {
#   data <- (model$residuals + model$fitted.values)
#   ssr <- sum((model$residuals)^2)
#   n <- length(data)
#   p <- (model$rank - 1)
#   return(log((ssr / t)) + ((p + 1) * (log(t) / t)))
# }

autoregress <- function(var, p) {
  n <- length(var)
  y <- var[-c(1:p)]

  x <- matrix(, nrow = (n - p), ncol = p)

  for (i in 1:p) {
    x[, i] <- var[(p + 1 - i):(n - i)]
  }

  list <- list(
    y = y,
    x = x
  )

  return(list)
}

autoregress_lm <- function(var, p) {
  return(lm(autoregress(var, p)$y ~ autoregress(var, p)$x))
}

bic_ar <- function(var, min = 1, max = (length(var) - 1)) {
  n <- length(var)
  bic_all <- matrix(, nrow = max, ncol = 2)
  bic_all[, 1] <- min:max

  for (i in min:max) {
    bic_all[i, 2] <- bic(autoregress_lm(var, i))
  }

  bic_all <- as.data.frame(bic_all)

  graph <- bic_all %>% ggplot(aes(x = V1, y = V2)) +
    geom_point()

  list <- list(
    bic_all = bic_all,
    graph = graph
  )

  return(list)
}

multivar <- function(y, x, opt, lambda) {
  n <- nrow(x)
  y <- y[-1]
  x <- as.matrix(x)
  x <- x[1:(n - 1), ]

  if (opt == "lm") {
    result <- lm(y ~ x)
  } else if (opt == "lasso") {
    result <- glmnet(x, y, alpha = 1, lambda = lambda)
  } else if (opt == "ridge") {
    result <- glmnet(x, y, alpha = 0, lambda = lambda)
  }

  return(result)
}

bic_glm <- function(fit) {
  tLL <- fit$nulldev - deviance(fit)
  k <- fit$df
  n <- fit$nobs
  BIC <- log(n) * k - tLL
  return(BIC)
}

bic_mvar <- function(y, x, opt) {
  lambdas <- seq(0.001, 0.1, by = 0.001)
  n <- length(lambdas)
  bic_all <- matrix(, nrow = n, ncol = 2)
  bic_all[, 1] <- lambdas

  for (i in 1:n) {
    bic_all[i, 2] <- bic_glm(multivar(y, x, opt, lambdas[i]))
  }

  bic_all <- as.data.frame(bic_all)

  graph <- bic_all %>% ggplot(aes(x = V1, y = V2)) +
    geom_point()

  list <- list(
    bic_all = bic_all,
    graph = graph
  )

  return(list)
}

forecast_all <- function(model, training, test) {
  data <- cbind(training, test)
  ntr <- nrow(training)
  ntst <- nrow(test)
  coefs <- as.vector(model$coefficients)
  int <- coefs[1]
  coefs <- coefs[-1]
  ncoefs <- length(coefs)
  result <- c()

  for (i in 1:ntst) {
    data[ntr - 1 + i, ]
  }

  return(result)
}
