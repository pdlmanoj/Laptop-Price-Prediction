


# load Library

library(tidyverse)
library(GGally)


# load the dataset
dataset = read.csv("creditcard.csv")
view(dataset)

glimpse(dataset)

summary(dataset)

anyNA(dataset)

colSums(is.na(dataset))
