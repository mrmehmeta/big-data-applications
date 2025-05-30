source("helpers.R")

data <- read_csv("data_BDA_2025.csv")
data_orig <- read_csv("current.csv")

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

# Merging the forecasts with the dates and observed values
test_orig <- data_orig[2:nrow(data_orig), ] %>%
  mutate(sasdate = as.character(as.Date(sasdate, format = "%m/%d/%Y"))) %>%
  filter(sasdate %in% teststd_wdate$sasdate) %>%
  arrange(as.Date(sasdate))

ipi <- list(
  id = "ipi",
  value = training$INDPRO,
  std = train_std$INDPRO,
  std_test = test_std$INDPRO,
  orig_test = test_orig$INDPRO,
  mean = train_mean$INDPRO,
  stdev = train_stdev$INDPRO
)

cpi <- list(
  id = "cpi",
  value = training$PCEPI,
  std = train_std$PCEPI,
  std_test = test_std$PCEPI,
  orig_test = test_orig$PCEPI,
  mean = train_mean$PCEPI,
  stdev = train_stdev$PCEPI
)

modelling <- function(variable) {
  # =============================================================================
  # AUTOREGRESSIVE MODELS
  # =============================================================================

  # AR(1)
  autoregress_lm(variable$std, 1)
  bic(autoregress_lm(variable$std, 1))

  # AR(p)
  bic_ar <- bic_ar(variable$std, max = 7)
  model_ar <- autoregress_lm(variable$std, bic_ar$p_min)

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

  # OLS
  model_ols <- multivar(variable$std, opt = "lm", x = train_std)

  # Ridge
  bic_ridge <- bic_mvar(variable$std, opt = "ridge", x = train_std)
  model_ridge <- multivar(variable$std, opt = "ridge", lambda = bic_ridge$lambda_min, x = train_std)
  
  # Lasso
  bic_lasso <- bic_mvar(variable$std, opt = "lasso", x = train_std)
  model_lasso <- multivar(variable$std, opt = "lasso", lambda = bic_lasso$lambda_min, x = train_std)

  # =============================================================================
  # PRINCIPAL COMPONENT REGRESSION
  # =============================================================================

  prs <- prcomp(train_std)
  summary(prs)
  fviz_eig(prs, addlabels = TRUE)
  plot(summary(prs)$importance[3, ])

  bic_pcr <- bic_pcr(variable$std, train_std)
  model_pcr <- lm(variable$std[-1, ] ~ pcr(train_std, bic_pcr$r_min))

  # =============================================================================
  # FORECASTING THE STANDARDIZED DATA
  # =============================================================================

  # AR(p)
  forecast_ar(model_ar, variable$std, variable$std_test)
  ar <- (variable$stdev * forecast_ar(model_ar, variable$std, variable$std_test)) + variable$mean

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

  # PCR
  forecast_pcr(model_pcr, train_std, test_std)
  pcr <- (variable$stdev * forecast_pcr(model_pcr, train_std, test_std)) + variable$mean

  # =============================================================================
  # LEVELING THE FORECASTS
  # =============================================================================
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

  # PCR
  pcr_level <- exp(pcr + level)

  forecasts <- cbind(test$sasdate, variable$orig_test, ar_level, rw_level, ols_level, ridge_level, lasso_level, pcr_level) %>%
    as.data.frame() %>%
    rename(sasdate = V1, orig_test = V2)

  for (i in 2:ncol(forecasts)) {
    forecasts[, i] <- as.numeric(forecasts[, i])
  }

  # =============================================================================
  # CALCULATING RMSE
  # =============================================================================

  ar_rmse <- rmse(forecasts$ar_level, forecasts$orig_test)
  rw_rmse <- rmse(forecasts$rw_level, forecasts$orig_test)
  ols_rmse <- rmse(forecasts$ols_level, forecasts$orig_test)
  ridge_rmse <- rmse(forecasts$ridge_level, forecasts$orig_test)
  lasso_rmse <- rmse(forecasts$lasso_level, forecasts$orig_test)
  pcr_rmse <- rmse(forecasts$pcr_level, forecasts$orig_test)

  return(list(
    rmse = list(
      ar = ar_rmse,
      rw = rw_rmse,
      ols = ols_rmse,
      ridge = ridge_rmse,
      lasso = lasso_rmse,
      pcr = pcr_rmse
    ),
    levels = list(
      ar = ar_level,
      rw = rw_level,
      ols = ols_level,
      ridge = ridge_level,
      lasso = lasso_level,
      pcr = pcr_level
    ),
    models = list(
      ar = model_ar,
      rw = model_rw,
      ols = model_ols,
      ridge = model_ridge,
      lasso = model_lasso,
      pcr = model_pcr
    ),
    forecasts = forecasts
  ))
}

ipi_model <- modelling(ipi)
cpi_model <- modelling(cpi)

# =============================================================================
# GRAPHING
# =============================================================================
var_labels <- c(
  `ar_level` = "Autoregression",
  `lasso_level` = "Lasso",
  `ols_level` = "OLS",
  `pcr_level` = "Principal Component Regression",
  `ridge_level` = "Ridge",
  `rw_level` = "Random Walk")

ipi_pivot <- ipi_model$forecasts %>%
  pivot_longer(
    cols = !c(sasdate,orig_test),
    names_to = "model",
    values_to = "value"
  )

cpi_pivot <- cpi_model$forecasts %>%
  pivot_longer(
    cols = !c(sasdate, orig_test),
    names_to = "model",
    values_to = "value"
  )

ipi_model$forecasts %>% ggplot(aes(x = as.Date(sasdate, format = "%m/%d/%y"), y = value)) +
  geom_line(aes(y = orig_test), color = "black") + 
  geom_line(aes(y = ar_level), color = "blue") +
  geom_line(aes(y = rw_level), color = "red") +
  geom_line(aes(y = ols_level), color = "green") +
  geom_line(aes(y = ridge_level), color = "yellow") +
  geom_line(aes(y = lasso_level), color = "magenta") +
  geom_line(aes(y = pcr_level), color = "cyan")+
  xlab("Date")+
  ylab("Value")+
  ggtitle("Overall comparison for IPI")+
  theme_grey()

cpi_model$forecasts %>% ggplot(aes(x = as.Date(sasdate, format = "%m/%d/%y"), y = value)) +
  geom_line(aes(y = orig_test), color = "black") + 
  geom_line(aes(y = ar_level), color = "blue") +
  geom_line(aes(y = rw_level), color = "red") +
  geom_line(aes(y = ols_level), color = "green") +
  geom_line(aes(y = ridge_level), color = "yellow") +
  geom_line(aes(y = lasso_level), color = "magenta") +
  geom_line(aes(y = pcr_level), color = "cyan")+
  xlab("Date")+
  ylab("Value")+
  ggtitle("Overall comparison for CPI")+
  theme_grey()

ipi_pivot %>% ggplot(aes(x = as.Date(sasdate, format = "%m/%d/%y"))) +
  geom_line(aes(y = orig_test))+
  facet_wrap(vars(model), labeller = as_labeller(var_labels))+
  geom_line(aes(y = value, colour = model))+
  xlab("Date")+
  ylab("Value")+
  theme_grey()+
  theme(legend.position = "none")+
  ggtitle("Graphical Comparison of Estimated Models for IPI", subtitle = "IPI values are in black.")+
  scale_fill_paletteer_d("MoMAColors::Abbott")

cpi_pivot %>% ggplot(aes(x = as.Date(sasdate, format = "%m/%d/%y"))) +
  geom_line(aes(y = orig_test))+
  facet_wrap(vars(model), labeller = as_labeller(var_labels))+
  geom_line(aes(y = value, colour = model))+
  xlab("Date")+
  ylab("Value")+
  theme_grey()+
  theme(legend.position = "none")+
  ggtitle("Graphical Comparison of Estimated Models for CPI", subtitle = "CPI values are in black.")+
  scale_fill_paletteer_d("MoMAColors::Abbott")
