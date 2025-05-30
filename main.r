source("helpers.R")

data <- read_csv("data_BDA_2025.csv")

# =============================================================================
# EXTRACTING VARIABLES
# =============================================================================

training <- data[cbind(0:(2 * nrow(data) / 3)), ]
test <- tail(data, n = (nrow(data) / 3))

data_std <- data %>%
  select(!sasdate) %>%
  mutate_all(scale, center = T, scale = T)

datastd_wdate <- data_std %>%
  cbind(as.Date(data$sasdate, format = "%m/%d/%y"), .) %>%
  rename(sasdate = "as.Date(data$sasdate, format = \"%m/%d/%y\")")

train_std <- training %>%
  select(!sasdate) %>%
  mutate_all(scale, center = T, scale = T)

trainstd_wdate <- train_std %>%
  cbind(as.Date(training$sasdate, format = "%m/%d/%y"), .) %>%
  rename(sasdate = "as.Date(training$sasdate, format = \"%m/%d/%y\")")

train_mean <- training %>%
  select(!sasdate) %>%
  mutate_all(mean) %>%
  filter(row_number() == 1)

train_stdev <- training %>%
  select(!sasdate) %>%
  mutate_all(sd) %>%
  filter(row_number() == 1)

test_std <- test %>%
  select(!sasdate) %>%
  mutate_all(scale, center = T, scale = T)

teststd_wdate <- test_std %>%
  cbind(as.Date(test$sasdate, format = "%m/%d/%y"), .) %>%
  rename(sasdate = "as.Date(test$sasdate, format = \"%m/%d/%y\")")

ipi <- list(
  id = "ipi",
  value = training$INDPRO,
  std = train_std$INDPRO,
  std_test = test_std$INDPRO,
  mean = train_mean$INDPRO,
  stdev = train_stdev$INDPRO
)

cpi <- list(
  id = "cpi",
  value = training$PCEPI,
  std = train_std$PCEPI,
  std_test = test_std$PCEPI,
  mean = train_mean$PCEPI,
  stdev = train_stdev$PCEPI
)

main <- function(variable) {
  # =============================================================================
  # AR MODELS
  # =============================================================================

  # AR1 Model
  ar1 <- autoregress_lm(variable$std, 1)
  bic(ar1)

  # AR(p) model
  bic_arp <- bic_ar(variable$std) # 1 is optimal
  model_arp <- autoregress_lm(variable$std, 1)

  bic(model_arp)

  # cpi_bic_arp <- bic_ar(cpi_std, max = 10)
  # cpi_arp <- autoregress_lm(cpi_std, 1)
  # bic(cpi_arp)

  # =============================================================================
  # RANDOM WALK
  # =============================================================================
  random_var <- variable$std

  for (i in length(random_var):2) {
    random_var[i] <- random_var[i] - random_var[i - 1]
  }

  random_var <- random_var[2:length(random_var), ]

  model_rw <- lm(random_var ~ 1)

  # =============================================================================
  # MULTIVARIATE MODELS
  # =============================================================================

  ## OLS
  model_ols <- multivar(variable$std, opt = "lm", x = train_std)

  ## Ridge
  bic_ridge <- bic_mvar(variable$std, opt = "ridge", x = train_std)
  model_ridge <- multivar(variable$std, opt = "ridge", lambda = bic_ridge$lambda_min, x = train_std)

  # modelsummary(ipi_ridge)

  ## Lasso
  bic_lasso <- bic_mvar(variable$std, opt = "lasso", x = train_std)
  model_lasso <- multivar(variable$std, opt = "lasso", lambda = bic_lasso$lambda_min, x = train_std)

  # =============================================================================
  # PRINCIPAL COMPONENTS ANALYSIS
  # =============================================================================

  notquite <- prcomp(train_std)
  summary(notquite)
  fviz_eig(notquite, addlabels = TRUE)
  plot(summary(notquite)$importance[3, ])


  # gam_pca <- (t(as.matrix(train_std)) %*% as.matrix(train_std))
  # eigenstuff <- eigen(gam_pca)
  #
  # ipi_eval <- rev(eigenstuff$values)
  # ipi_evec <- as.matrix(rev(as.data.frame(eigenstuff$vectors)))
  #
  # F1_ipi <- (as.matrix(train_std) %*% ipi_evec[,1])/sqrt(nrow(train_std))
  # F6_ipi <- (as.matrix(train_std) %*% ipi_evec[,1:6])/sqrt(nrow(train_std))
  # Fn_ipi <- (as.matrix(train_std) %*% ipi_evec)/sqrt(nrow(train_std))
  #
  # ipi_pca1 <- lm(variable$std[-1,] ~ F1_ipi[-nrow(F1_ipi),])
  # ipi_pca6 <- lm(variable$std[-1,] ~ F6_ipi[-nrow(F6_ipi),])

  f_var <- pca(train_std)

  variable$pca1 <- lm(variable$std[-1, ] ~ f_var[, 1])
  variable$pca6 <- lm(variable$std[-1, ] ~ f_var[, 1:6])

  # TODO: bic_p# TODO: bic_pca(data = variable$std, regressors = Fn_ipi)

  # =============================================================================
  # FORECASTING THE STANDARDIZED DATA
  # =============================================================================

  # AR(p)
  forecast_ar(model_arp, variable$std, variable$std_test)
  ar <- (variable$stdev * forecast_ar(model_arp, variable$std, variable$std_test)) + variable$mean

  # Random Walk
  forecast_rw(model_rw, variable$std, variable$std_test)
  randomwalk <- (variable$stdev * forecast_rw(model_rw, variable$std, variable$std_test)) + variable$mean

  # OLS
  forecast_mvar(model_ols, train_std, test_std)
  ols <- (variable$stdev * forecast_mvar(model_ols, train_std, test_std)) + variable$mean

  # Ridge
  forecast_mvar(model_ridge, train_std, test_std)
  ridge <- (variable$stdev * forecast_mvar(model_ridge, train_std, test_std)) + variable$mean

  # Lasso
  forecast_mvar(model_lasso, train_std, test_std)
  lasso <- (variable$stdev * forecast_mvar(model_lasso, train_std, test_std)) + variable$mean

  # PCA
  forecast_pca(variable$pca1, train_std, test_std)
  pca_1 <- (variable$stdev * forecast_pca(variable$pca1, train_std, test_std)) + variable$mean

  forecast_pca(variable$pca6, train_std, test_std)
  pca_6 <- (variable$stdev * forecast_pca(variable$pca6, train_std, test_std)) + variable$mean

  # =============================================================================
  # LEVELING THE FORECASTS
  # =============================================================================
  data_orig <- read_csv("current.csv")

  if (variable$id == "ipi") {
    level <- data_orig[2:nrow(data_orig), ] %>%
      mutate(sasdate = as.character(as.Date(sasdate, format = "%m/%d/%Y"))) %>%
      filter(sasdate %in% rbind(trainstd_wdate[nrow(trainstd_wdate), ], teststd_wdate)$sasdate) %>%
      arrange(as.Date(sasdate))

    level <- log(as.vector(level[1:(nrow(level) - 1), ]$INDPRO))
  } else if (variable$id == "cpi") {
    level <- data_orig[2:nrow(data_orig), ] %>%
      mutate(sasdate = as.character(as.Date(sasdate, format = "%m/%d/%Y"))) %>%
      filter(sasdate %in% rbind(trainstd_wdate[(nrow(trainstd_wdate) - 1):nrow(trainstd_wdate), ], teststd_wdate)$sasdate) %>%
      arrange(as.Date(sasdate))

    level <- (2 * log(as.vector(level[2:(nrow(level) - 1), ]$PCEPI))) - log(as.vector(level[1:(nrow(level) - 2), ]$PCEPI))
  }

  # AR(p)
  ar_level <- exp(ar + level)

  # Random Walk
  rw_level <- exp(randomwalk + level)

  # OLS
  ols_level <- exp(ols + level)

  # Ridge
  ridge_level <- exp(ridge + level)

  # Lasso
  lasso_level <- exp(lasso + level)

  # PCA
  pca1_level <- exp(pca_1 + level)
  pca6_level <- exp(pca_6 + level)

  # Merging the forecasts with the dates and observed values
  test_orig <- data_orig[2:nrow(data_orig), ] %>%
    mutate(sasdate = as.character(as.Date(sasdate, format = "%m/%d/%Y"))) %>%
    filter(sasdate %in% teststd_wdate$sasdate) %>%
    arrange(as.Date(sasdate))

  forecasts <- cbind(test$sasdate, test_orig$INDPRO, ar_level, rw_level, ols_level, ridge_level, lasso_level, pca1_level, pca6_level) %>%
    as.data.frame() %>%
    rename(sasdate = V1, INDPRO = V2)

  for (i in 2:ncol(forecasts)) {
    forecasts[, i] <- as.numeric(forecasts[, i])
  }

  # =============================================================================
  # CALCULATING RMSE
  # =============================================================================

  ar_rmse <- rmse(forecasts$ar_level, forecasts$INDPRO)
  rw_rmse <- rmse(forecasts$rw_level, forecasts$INDPRO)
  ols_rmse <- rmse(forecasts$ols_level, forecasts$INDPRO)
  ridge_rmse <- rmse(forecasts$ridge_level, forecasts$INDPRO)
  lasso_rmse <- rmse(forecasts$lasso_level, forecasts$INDPRO)
  pca1_rmse <- rmse(forecasts$pca1_level, forecasts$INDPRO)

  # =============================================================================
  # GRAPHING
  # =============================================================================
  forecasts %>% ggplot(aes(x = as.Date(sasdate, format = "%m/%d/%y"))) +
    geom_line(aes(y = INDPRO), color = "black") +
    geom_line(aes(y = ar_level), color = "blue") +
    geom_line(aes(y = rw_level), color = "red") +
    geom_line(aes(y = ols_level), color = "green") +
    geom_line(aes(y = ridge_level), color = "yellow") +
    geom_line(aes(y = lasso_level), color = "magenta") +
    geom_line(aes(y = pca1_level), color = "cyan")
}

main(ipi)
main(cpi)
