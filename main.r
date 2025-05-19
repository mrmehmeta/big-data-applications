library(tidyverse)

data <- read_csv("data_BDA_2025.csv")

training <- data[cbind(0:(2 * nrow(data) / 3)), ]

test <- tail(data, n = (nrow(data) / 3))

# Extract variables to later use for forecasting
cpi <- training$PCEPI
ipi <- training$INDPRO
dates <- training$sasdate
cpi_std <- scale(wCPI, center = T, scale = T)
ipi_std <- scale(wIPI, center = T, scale = T)
cpi_mean <- mean(wCPI)
cpi_stdev <- sd(wCPI)
ipi_mean <- mean(wIPI)
ipi_stdev <- sd(wIPI)

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
