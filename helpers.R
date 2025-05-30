library(tidyverse)
library(zoo)
library(lme4)
library(forecast)
library(glmnet)
library(modelsummary)
library(lmridge)
library(FactoMineR)
library(factoextra)
library(paletteer)

# =============================================================================
# BIC CALCULATIONS
# =============================================================================

bic <- function(model, lambda = NULL, data = NULL) {
  bic_ols <- function(model) {
    p <- (model$rank - 1)
    data <- (model$residuals + model$fitted.values)
    ssr <- sum((model$residuals)^2)
    n <- length(data)
    return(log(ssr / n) + ((p + 1) * log(n) / n))
  }
  
  bic_lasso <- function(model, data) {
    p <- (model$df)
    y <- data[, 1]
    x <- as.matrix(data[, -1])
    y_pred <- predict(model, newx = x, s = model$lambda)
    
    residuals <- y - y_pred
    ssr <- sum(residuals^2)
    
    if (ssr <= 0) {
      stop("The ssr in the lasso regression is <= 0. This means that the data is either
        artificially made to fit lasso or that there's a logic error.")
    }
    n <- length(y)
    return(log(ssr / n) + (p * log(n) / n))
  }
  
  
  bic_ridge <- function(model, lambda) {
    trace <- function(m) {
      n <- dim(m)[1]
      tr <- 0
      for (i in 1:n) {
        tr <- tr + m[i, i]
      }
      
      return(tr[[1]])
    }
    x <- model$xs
    residuals <- residuals(model)
    ssr_m <- mean(residuals^2)
    sigma_squared <- var(residuals)
    n <- nrow(x)
    d_lambda <- trace(x %*% solve(t(x) %*% x + lambda * diag(ncol(x))) %*% t(x))
    return(ssr_m + (log(n) * d_lambda * sigma_squared / n))
  }
  
  if (length(class(model)) > 1) {
    model_class <- class(model)[2]
  } else {
    model_class <- class(model)
  }
  
  if (model_class == "lm") {
    return(bic_ols(model))
  } else if (model_class == "lmridge") {
    return(bic_ridge(model, lambda))
  } else if (model_class == "glmnet") {
    return(bic_lasso(model, data))
  } else {
    stop("\n Error: unhandled model class:\n")
    print(model_class)
  }
}

# =============================================================================
# AUTOREGRESSIVE MODELS
# =============================================================================

autoregress <- function(variable, p) {
  n <- nrow(variable)
  y <- variable[-c(1:p)]
  
  x <- matrix(, nrow = (n - p), ncol = p)
  
  for (i in 1:p) {
    x[, i] <- variable[(p + 1 - i):(n - i)]
  }
  
  list <- list(
    y = y,
    x = x
  )
  
  return(list)
}

autoregress_lm <- function(variable, p) {
  return(lm(autoregress(variable, p)$y ~ autoregress(variable, p)$x))
}

bic_ar <- function(variable, min = 1, max = (length(variable) - 1)) {
  n <- length(variable)
  bic_all <- matrix(, nrow = max, ncol = 2)
  bic_all[,1] <- min:max
  
  for (i in min:max) {
    bic_all[i,2] <- bic(autoregress_lm(variable, i))
  }
  
  for(i in min:max){
    if(bic_all[i,2] < bic_all[(i+1),2]){
      p_min <- bic_all[i,1]
      break
    }
  }
  
  bic_all <- as.data.frame(bic_all)
  
  graph <- bic_all |> ggplot(aes(x = V1, y = V2)) +
    geom_point() +
    geom_line() +
    labs(title = paste("BIC by p for Autoregression"), x = "p", y = "BIC")
  
  list <- list(
    bic_all = bic_all,
    graph = graph,
    p_min = p_min
  )
  
  return(list)
}

# =============================================================================
# MULTIVARIATE MODELS
# =============================================================================

multivar <- function(y, x, opt, lambda) {
  n <- nrow(x)
  y <- y[-1]
  x <- x[1:(n - 1), ]
  x <- as.matrix(x)
  data <- as.data.frame(cbind(y, x))
  
  
  if (opt == "lm") {
    model <- lm(y ~ x)
  } else if (opt == "lasso") {
    model <- glmnet(x, y, alpha = 1, lambda = lambda)
  } else if (opt == "ridge") {
    model <- lmridge(y ~ ., data = data, K = lambda, scaling = "non")
  } else {
    stop("Error: unknown option '", opt, "'. Use 'lm', 'lasso', or 'ridge'")
  }
  
  return(model)
}

bic_mvar <- function(y, x, opt) {
  if (opt == "lasso") {
    lambdas <- exp(seq(log(0.001), log(1), length.out = 100))
  } else {
    lambdas <- seq(0.001, 10000, length.out = 100)
  }
  n <- length(lambdas)
  bic_all <- matrix(, nrow = n, ncol = 2)
  bic_all[, 1] <- lambdas
  y_trimmed <- y[-1]
  x_trimmed <- as.matrix(x[1:(nrow(x) - 1), ])
  data <- as.data.frame(cbind(y_trimmed, x_trimmed))
  
  for (i in 1:n) {
    bic_all[i, 2] <- bic(multivar(y, x, opt, lambdas[i]),
                         lambdas[i],
                         data = data
    )
  }
  
  bic_all <- as.data.frame(bic_all)
  
  graph <- bic_all |> ggplot(aes(x = V1, y = V2)) +
    geom_point() +
    geom_line() +
    labs(title = paste("BIC by lambda for", case_when(opt == "lasso" ~ "Lasso", opt == "ridge" ~ "Ridge")), x = "lambda", y = "BIC")
  
  if (opt == "lasso") {
    graph <- graph + scale_x_log10()
  }
  
  list <- list(
    bic_all = bic_all,
    graph = graph,
    lambda_min = bic_all[bic_all[,2] == min(bic_all[,2]),1]
  )
  
  return(list)
}

# =============================================================================
# PRINCIPAL COMPONENT REGRESSION
# =============================================================================
pcr <- function(data, r = ncol(data)){
  fac <- ((as.matrix(data) %*% as.matrix(rev(as.data.frame(eigen((t(as.matrix(data)) %*% as.matrix(data)))$vectors)))[,1:r])/sqrt(nrow(data)))
  fac <- fac[1:(nrow(fac)-1),]
  
  return(fac)
}

bic_pcr <- function(y, x) {
  max <- ncol(x)
  bic_all <- matrix(, nrow = max, ncol = 2)
  bic_all[,1] <- 1:max
  
  for (i in 1:max) {
    bic_all[i, 2] <- bic(lm(y[-1,] ~ pcr(x)[,1:i]))
  }
  
  bic_all <- as.data.frame(bic_all)
  graph <- bic_all |> ggplot(aes(x = V1, y = V2)) +
    geom_point() +
    geom_line() +
    labs(title = paste("BIC by r for Principal Component Regression"), x = "r", y = "BIC")
  
  list <- list(
    bic_all = bic_all,
    graph = graph,
    r_min = bic_all[bic_all[,2] == min(bic_all[,2]),1]
  )
  return(list)
}

# =============================================================================
# FORECASTING
# =============================================================================

forecast_ar <- function(model, training, test) {
  data <- rbind(training, test)
  coefs <- as.vector(model$coefficients)
  int <- coefs[1]
  coefs <- coefs[-1]
  p <- length(coefs)
  m <- matrix(, nrow = nrow(test), ncol = p)
  
  for (i in 1:nrow(test)) {
    m[i,] <- data[(nrow(training) + i - 1):(nrow(training) + i - p),]
  }
  
  for (i in 1:ncol(m)) {
    m[, i] <- m[, i] * coefs[i]
  }
  
  result <- rowSums(m, na.rm = T) + int
  
  return(result)
}

forecast_mvar <- function(model, training, test) {
  data <- rbind(training, test)
  
  if (length(class(model)) > 1) {
    model_class <- class(model)[2]
  } else {
    model_class <- class(model)
  }
  
  if (model_class == "lm") {
    coefs <- as.vector(model$coefficients)
  } else if (model_class == "lmridge") {
    coefs <- as.vector(model$coef)
  } else if (model_class == "glmnet") {
    coefs <- as.vector(model$beta)
  } else {
    stop("\n Error: unhandled model class:\n")
    print(model_class)
  }
  
  int <- coefs[1]
  coefs <- coefs[-1]
  m <- data[nrow(training):(nrow(data)-1),]
  
  for (i in 1:ncol(m)) {
    m[, i] <- m[, i] * coefs[i]
  }
  
  result <- rowSums(m, na.rm = T) + int
  
  return(result)
}

forecast_rw <- function(model, training, test){
  data <- rbind(training, test)
  coef <- as.vector(model$coefficients)[1]
  result <- as.vector(data[nrow(training):(nrow(data)-1),]) + coef
  
  return(result)
}

forecast_pcr <- function(model, training, test){
  data <- rbind(training, test)
  coefs <- as.matrix(model$coefficients)
  int <- coefs[1]
  coefs <- as.matrix(coefs[-1])
  r <- nrow(coefs)
  result <- c()
  
  for(i in 1:nrow(test)){
    f <- pcr(data[1:(nrow(training)+i),], r)
    pred <- (f %*% coefs)
    result[i] <- pred[nrow(pred)] + int
  }
  
  return(result)
}

# =============================================================================
# CALCULATING RMSE
# =============================================================================
rmse <- function(pred, observed){
  return(sqrt((sum((pred - observed)^2))/length(observed)))
}
