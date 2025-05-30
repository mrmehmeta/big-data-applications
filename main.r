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

# AR1 Model
ipi_ar1 <- autoregress_lm(ipi_std, 1)
bic(ipi_ar1)

# cpi_ar1 <- autoregress_lm(cpi_std, 1)
# bic(cpi_ar1)

# AR(2) Model
# ipi_ar2 <- autoregress_lm(ipi_std, 2)
# bic(ipi_ar2)

# cpi_ar2 <- autoregress_lm(cpi_std, 2)
# bic(cpi_ar2)

# AR(p) model
bic_arp <- bic_ar(ipi_std) # 1 is optimal
model_arp <- autoregress_lm(ipi_std, 1)

bic(model_arp)

# cpi_bic_arp <- bic_ar(cpi_std, max = 10) 
# cpi_arp <- autoregress_lm(cpi_std, 1)
# bic(cpi_arp)

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
# PRINCIPAL COMPONENTS ANALYSIS
# =============================================================================

notquite <- prcomp(train_std)
summary(notquite)
fviz_eig(notquite, addlabels = TRUE)
plot(summary(notquite)$importance[3,])


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
# ipi_pca1 <- lm(ipi_std[-1,] ~ F1_ipi[-nrow(F1_ipi),])
# ipi_pca6 <- lm(ipi_std[-1,] ~ F6_ipi[-nrow(F6_ipi),])

F_ipi <- pca(train_std)

ipi_pca1 <- lm(ipi_std[-1,] ~ F_ipi[,1])
ipi_pca6 <- lm(ipi_std[-1,] ~ F_ipi[,1:6])

# TODO: bic_p# TODO: bic_pca(data = ipi_std, regressors = Fn_ipi)

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

forecast_pca(ipi_pca6, train_std, test_std)
pca_6 <- (ipi_stdev * forecast_pca(ipi_pca6, train_std, test_std)) + ipi_mean

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
pca6_level <- exp(pca_6 + level)

# Merging the forecasts with the dates and observed values
test_orig <- data_orig[2:nrow(data_orig),] %>% 
  mutate(sasdate = as.character(as.Date(sasdate, format = "%m/%d/%Y"))) %>% 
  filter(sasdate %in% teststd_wdate$sasdate) %>% 
  arrange(as.Date(sasdate))

forecasts <- cbind(test$sasdate, test_orig$INDPRO, ar_level, rw_level, ols_level, ridge_level, lasso_level, pca1_level, pca6_level) %>% 
  as.data.frame() %>% 
  rename(sasdate = V1, INDPRO = V2)

for(i in 2:ncol(forecasts)){
  forecasts[,i] <- as.numeric(forecasts[,i])
}

# =============================================================================
# CALCULATING RMSE
# =============================================================================


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
