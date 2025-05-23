source("helpers.R")

data <- read_csv("data_BDA_2025.csv")

training <- data[cbind(0:(2 * nrow(data) / 3)), ]

test <- tail(data, n = (nrow(data) / 3))

# =======================================================================
# EXTRACT VARIABLES TO LATER USE FOR FORECASTING
# =======================================================================

cpi <- training$PCEPI
ipi <- training$INDPRO
dates <- training$sasdate
cpi_std <- scale(cpi, center = T, scale = T)
ipi_std <- scale(ipi, center = T, scale = T)
cpi_mean <- mean(cpi)
cpi_stdev <- sd(cpi)
ipi_mean <- mean(ipi)
ipi_stdev <- sd(ipi)

train_mean <- training %>%
  select(!sasdate) %>%
  mutate_all(mean) %>%
  t()

train_stdev <- training %>%
  select(!sasdate) %>%
  mutate_all(sd) %>%
  t()

train_std <- training %>%
  select(!sasdate) %>%
  mutate_all(scale, center = T, scale = T) %>%
  cbind(dates, .)


# =======================================================================
# AR MODELS
# =======================================================================

# AR1 Model
ipi_ar1 <- autoregress_lm(train_std$INDPRO, 1)
bic(ipi_ar1)

cpi_ar1 <- autoregress_lm(train_std$PCEPI, 1)
bic(cpi_ar1)

# AR(2) Model
ipi_ar2 <- autoregress_lm(train_std$INDPRO, 2)
bic(ipi_ar2)

cpi_ar2 <- autoregress_lm(train_std$PCEPI, 2)
bic(cpi_ar2)

# AR(p) model
ipi_bic_arp <- bic_ar(train_std$INDPRO) # 1 it optimal
ipi_arp <- autoregress_lm(train_std$INDPRO, 1)
bic(ipi_arp)

cpi_bic_arp <- bic_ar(train_std$PCEPI, max = 10) # 5 is optimal
cpi_arp <- autoregress_lm(train_std$INDPRO, 1)
bic(ipi_arp)

# =======================================================================
# RANDOM WALK
# =======================================================================
random_ipi <- ipi_std

for (i in length(random_ipi):2) {
  random_ipi[i] <- random_ipi[i] - random_ipi[i - 1]
}

random_ipi <- random_ipi[2:length(random_ipi), ]

b0_ipi <- mean(random_ipi)
lm(random_ipi ~ 1)

# =======================================================================
# MULTIVARIATE MODELS
# =======================================================================

trainstd_nodate <- train_std %>%
  select(!dates)

## OLS
ipi_ols <- multivar(ipi_std, opt = "lm", x = trainstd_nodate)
# modelsummary(ipi_OLS)

## Ridge
bic_mvar(ipi_std, opt = "ridge", x = trainstd_nodate)


ipi_ridge <- multivar(ipi_std, opt = "ridge", lambda = 3030.304 , x = trainstd_nodate)
# modelsummary(ipi_ridge)

## Lasso
bic_mvar(ipi_std, opt = "lasso", x = trainstd_nodate)

ipi_lasso <- multivar(ipi_std, opt = "lasso", lambda = 0.04328761 , x = trainstd_nodate)


# =============================================================================
# PCA (factor model)
# =============================================================================

notquite <- prcomp(trainstd_nodate)
summary(notquite)
fviz_eig(notquite, addlabels = TRUE)
plot(summary(notquite)$importance[3,])


gam_pca <- (t(as.matrix(trainstd_nodate)) %*% as.matrix(trainstd_nodate))
eigenstuff <- eigen(gam_pca)
eigenstuff[nrow(eigenstuff):1,]
ipi_eval <- rev(eigenstuff$values)
ipi_evec <- as.matrix(rev(as.data.frame(eigenstuff$vectors)))

F1_ipi <- (as.matrix(trainstd_nodate) %*% ipi_evec[,1])/sqrt(nrow(trainstd_nodate))
F6_ipi <- (as.matrix(trainstd_nodate) %*% ipi_evec[,1:6])/sqrt(nrow(trainstd_nodate))
Fn_ipi <- (as.matrix(trainstd_nodate) %*% ipi_evec)/sqrt(nrow(trainstd_nodate))



ipi_pca1 <- lm(ipi_std[-1,] ~ F1_ipi[-nrow(F1_ipi),])
ipi_pca6 <- lm(ipi_std[-1,] ~ F6_ipi[-nrow(F6_ipi),])
