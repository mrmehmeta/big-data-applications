source("helpers.R")

data <- read_csv("data_BDA_2025.csv")

training <- data[cbind(0:(2 * nrow(data) / 3)), ]

test <- tail(data, n = (nrow(data) / 3))

# Extract variables to later use for forecasting
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

#AR1 Model
ar_1 <- autoregress_lm(train_std$INDPRO, 1)

BIC(ar_1)


#AR(2) Model
# ar_2 <- autoregress_lm(train_std$INDPRO, 2)
# 
# BIC(ar_2)

#AR(p) model
bic_arp <- bic_ar(ipi)

#Random Walk 

#Multivariate OLS, Ridge, Lasso

#PCA (factor model)
