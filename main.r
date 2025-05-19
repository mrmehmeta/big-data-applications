library(tidyverse)

data <- read_csv("data_BDA_2025.csv")

training <- data[cbind(0:(2 * nrow(data) / 3)), ]

test <- tail(data, n = (nrow(data) / 3))

print(dim(data))
print(dim(training))
print(dim(test))

wCPI <- training$PCEPI
wIPI <- training$INDPRO
dates <- training$sasdate

train_mean <- training %>%
  select(!sasdate) %>%
  mutate_all(mean) %>%
  t()

train_stdev <- training %>%
  select(!sasdate) %>%
  mutate_all(sd) %>%
  t()

Train_std <- training %>%
  select(!sasdate) %>%
  mutate_all(scale, center = T, scale = T) %>%
  cbind(dates, .)


wCPI_std <- scale(wCPI, center = T, scale = T)
wIPI_std <- scale(wIPI, center = T, scale = T)
