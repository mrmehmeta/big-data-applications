library(tidyverse)

data <- read_csv("data_BDA_2025.csv")

training <- data[cbind(0:(2 * nrow(data) / 3)), ]

print(dim(data))
print(dim(training))
