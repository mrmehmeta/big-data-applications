source("helpers.R")

data <- read_csv("data_BDA_2025.csv")

# =============================================================================
# EXTRACTING VARIABLES
# =============================================================================

training <- data[cbind(0:(2 * nrow(data) / 3)),]
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

ipi <- training$INDPRO
ipi_std <- train_std$INDPRO
ipistd_test <- test_std$INDPRO
ipi_mean <- train_mean$INDPRO
ipi_stdev <- train_stdev$INDPRO

# cpi <- training$PCEPI
# cpi_std <- train_std$PCEPI
# cpi_stdtest <- test_std$PCEPI
# cpi_mean <- train_mean$PCEPI
# cpi_stdev <- train_stdev$PCEPI

# =============================================================================
# AR MODELS
# =============================================================================

# AR(1) Model
ipi_ar1 <- autoregress_lm(ipi_std, 1)
bic(ipi_ar1)

# AR(p) model
bic_arp <- bic_ar(ipi_std) # 1 is optimal
model_arp <- autoregress_lm(ipi_std, 1)

bic(model_arp)

# =============================================================================
# RANDOM WALK
# =============================================================================
random_ipi <- ipi_std

for (i in length(random_ipi):2) {
  random_ipi[i] <- random_ipi[i] - random_ipi[i - 1]
}

random_ipi <- random_ipi[2:length(random_ipi),]

# b0_ipi <- mean(random_ipi)
model_rw <- lm(random_ipi ~ 1)

# =============================================================================
# MULTIVARIATE MODELS
# =============================================================================

## OLS
model_ols <- multivar(ipi_std, opt = "lm", x = train_std)
# modelsummary(ipi_OLS)

## Ridge
bic_ridge <- bic_mvar(ipi_std, opt = "ridge", x = train_std)
model_ridge <- multivar(ipi_std, opt = "ridge", lambda = bic_ridge$lambda_min, x = train_std)

# modelsummary(ipi_ridge)

## Lasso
bic_lasso <- bic_mvar(ipi_std, opt = "lasso", x = train_std)
model_lasso <- multivar(ipi_std, opt = "lasso", lambda = bic_lasso$lambda_min, x = train_std)

# =============================================================================
# PRINCIPAL COMPONENT ANALYSIS
# =============================================================================

notquite <- prcomp(train_std)
summary(notquite)
fviz_eig(notquite, addlabels = TRUE)
plot(summary(notquite)$importance[3,])

F_ipi <- pca(train_std)

ipi_pca1 <- lm(ipi_std[-1,] ~ F_ipi[,1])

bic_pca(ipi_std, train_std)

# =============================================================================
# FORECASTING THE STANDARDIZED DATA
# =============================================================================

# AR(p)
forecast_ar(model_arp, ipi_std, ipistd_test)
ar <- (ipi_stdev * forecast_ar(model_arp, ipi_std, ipistd_test)) + ipi_mean

# Random Walk
forecast_rw(model_rw, ipi_std, ipistd_test)
randomwalk <- (ipi_stdev * forecast_rw(model_rw, ipi_std, ipistd_test)) + ipi_mean

# OLS
forecast_mvar(model_ols, train_std, test_std)
ols <- (ipi_stdev * forecast_mvar(model_ols, train_std, test_std)) + ipi_mean

# Ridge
forecast_mvar(model_ridge, train_std, test_std)
ridge <- (ipi_stdev * forecast_mvar(model_ridge, train_std, test_std)) + ipi_mean

# Lasso
forecast_mvar(model_lasso, train_std, test_std)
lasso <- (ipi_stdev * forecast_mvar(model_lasso, train_std, test_std)) + ipi_mean

# PCA
forecast_pca(ipi_pca1, train_std, test_std)
pca_1 <- (ipi_stdev * forecast_pca(ipi_pca1, train_std, test_std)) + ipi_mean

# =============================================================================
# LEVELING THE FORECASTS
# =============================================================================
data_orig <- read_csv("current.csv")

level <- data_orig[2:nrow(data_orig),] %>% 
  mutate(sasdate = as.character(as.Date(sasdate, format = "%m/%d/%Y"))) %>% 
  filter(sasdate %in% rbind(trainstd_wdate[nrow(trainstd_wdate),], teststd_wdate)$sasdate) %>% 
  arrange(as.Date(sasdate))

level <- log(as.vector(level[1:(nrow(level)-1),]$INDPRO))

# level <- data_orig[2:nrow(data_orig),] %>% 
#   mutate(sasdate = as.character(as.Date(sasdate, format = "%m/%d/%Y"))) %>% 
#   filter(sasdate %in% rbind(trainstd_wdate[(nrow(trainstd_wdate)-1):nrow(trainstd_wdate),], teststd_wdate)$sasdate) %>% 
#   arrange(as.Date(sasdate))
# 
# level <- (2 * log(as.vector(level[2:(nrow(level)-1),]$PCEPI))) - log(as.vector(level[1:(nrow(level)-2),]$PCEPI))

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

# Merging the forecasts with the dates and observed values
test_orig <- data_orig[2:nrow(data_orig),] %>% 
  mutate(sasdate = as.character(as.Date(sasdate, format = "%m/%d/%Y"))) %>% 
  filter(sasdate %in% teststd_wdate$sasdate) %>% 
  arrange(as.Date(sasdate))

forecasts <- cbind(test$sasdate, test_orig$INDPRO, ar_level, rw_level, ols_level, ridge_level, lasso_level, pca1_level) %>% 
  as.data.frame() %>% 
  rename(sasdate = V1, INDPRO = V2)

for(i in 2:ncol(forecasts)){
  forecasts[,i] <- as.numeric(forecasts[,i])
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
forecasts_h <- forecasts %>%
  pivot_longer(
    cols = !c(sasdate,INDPRO),
    names_to = "model",
    values_to = "value"
      )

forecasts_h %>% ggplot(aes(x = as.Date(sasdate, format = "%m/%d/%y"), y = value, colour = model)) +
  facet_wrap(vars(model))+
  geom_line()+
  geom_line(aes(y = INDPRO))
  xlab("Date")+
  ylab("Value")+
  theme_grey()+
  scale_fill_paletteer_d("MoMAColors::Abbott")

forecasts %>% ggplot(aes(x = as.Date(sasdate, format = "%m/%d/%y"), y = value)) +
  geom_line(aes(y = INDPRO), color = "black") + 
  geom_line(aes(y = ar_level), color = "blue") +
  geom_line(aes(y = rw_level), color = "red") +
  geom_line(aes(y = ols_level), color = "green") +
  geom_line(aes(y = ridge_level), color = "yellow") +
  geom_line(aes(y = lasso_level), color = "magenta") +
  geom_line(aes(y = pca1_level), color = "cyan")+
  xlab("Date")+
  ylab("Value")+
  theme_grey()
