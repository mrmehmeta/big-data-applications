library(tidyverse)
library(zoo)
library(lme4)
library(forecast)
library(glmnet)
library(modelsummary)
library(lmridge)

bic <- function(model, lambda = NULL, data = NULL) {
  bic_ols <- function(model) {
    p <- (model$rank - 1)
    data <- (model$residuals + model$fitted.values)
    ssr <- sum((model$residuals)^2)
    t <- length(data)
    return(log(ssr / t) + ((p + 1) * log(t) / t))
  }

  bic_lasso <- function(model, data) {
    p <- (model$df)
    y <- data[, 1]
    x <- as.matrix(data[, -1])
    y_pred <- predict(model, newx = x, s = model$lambda / 1000)
    residuals <- y - y_pred
    ssr <- sum(residuals^2)
    t <- length(y)
    return(log(ssr / t) + (p * log(t) / t))
  }


  bic_ridge <- function(model, lambda) {
    trace <- function(A) {
      n <- dim(A)[1]
      tr <- 0
      for (k in 1:n) {
        l <- A[k, k]
        tr <- tr + l
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

  graph <- bic_all |> ggplot(aes(x = V1, y = V2)) +
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
  x <- x[1:(n - 1), ]
  x <- as.matrix(x)
  data <- as.data.frame(cbind(y, x))


  if (opt == "lm") {
    model <- lm(y ~ x)
  } else if (opt == "lasso") {
    model <- glmnet(x, y, alpha = 1, lambda = (lambda / 1000))
  } else if (opt == "ridge") {
    model <- lmridge(y ~ ., data = data, K = lambda, scaling = "non")
  }

  return(model)
}

bic_mvar <- function(y, x, opt) {
  lambdas <- seq(1, 1000, by = 1)
  n <- length(lambdas)
  bic_all <- matrix(, nrow = n, ncol = 2)
  bic_all[, 1] <- lambdas
  y_trimmed <- y[-1]
  x_trimmed <- as.matrix(x[1:(nrow(x) - 1), ])
  data <- as.data.frame(cbind(y_trimmed, x_trimmed))

  for (i in 1:n) {
    bic_all[i, 2] <- bic(multivar(y, x, opt, lambdas[i]), lambdas[i], data = data)
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
