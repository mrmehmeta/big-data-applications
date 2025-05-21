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

# AR1 Model
ipi_ar1 <- autoregress_lm(train_std$INDPRO, 1)
BIC(ipi_ar1)

cpi_ar1 <- autoregress_lm(train_std$PCEPI, 1)
BIC(cpi_ar1)

# AR(2) Model
ipi_ar2 <- autoregress_lm(train_std$INDPRO, 2)
BIC(ipi_ar2)

cpi_ar2 <- autoregress_lm(train_std$PCEPI, 2)
BIC(cpi_ar2)
# AR(p) model 
ipi_bic_arp <- bic_ar(train_std$INDPRO) #1 it optimal
ipi_arp <- autoregress_lm(train_std$INDPRO, 1)
BIC(ipi_arp)

cpi_bic_arp <- bic_ar(train_std$PCEPI, max = 10) #5 is optimal
cpi_arp <- autoregress_lm(train_std$INDPRO, 1)
BIC(ipi_arp)
# Random Walk 
# Y_t=\beta_0+Y_{t-1}+\epsilon <=> Y_t-Y_{t-1}=\beta_0+\epsilon
random_ipi <- ipi_std

for (i in length(random_ipi):2) {
  random_ipi[i] <- random_ipi[i] - random_ipi[i - 1]
}

random_ipi <- random_ipi[2:length(random_ipi), ]

b0_ipi <- mean(random_ipi)
lm(random_ipi ~ 1)

#Multivariate OLS, Ridge, Lasso
trainstd_nodate <- train_std %>% 
  select(!dates)

##OLS
ipi_OLS <- ipi_std %>%
  multivar(opt = "lm", x = trainstd_nodate ) 

modelsummary(ipi_OLS)

##Ridge
ipi_std %>%
  bic_mvar(opt = "ridge", x = trainstd_nodate)

ipi_ridge <- ipi_std %>%
  multivar(opt = "ridge", lambda = 0.1, x = trainstd_nodate )

modelsummary(ipi_ridge)
##Lasso
ipi_std %>%
  bic_mvar(opt = "lasso", x = trainstd_nodate)

trainstd_nodate%>%
  bic_mvar()

train_std %>%
  multivar(opt = "lasso", lambda = )
# PCA (factor model)
