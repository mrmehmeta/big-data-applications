library(tidyverse)
library(caret)

data <- read_csv("current.csv")
training_size <- 2 * nrow(data) / 3

training <- createDataPartition(data, p = 0.6, list = FALSE)
print(dim(data))
print(dim(training))
