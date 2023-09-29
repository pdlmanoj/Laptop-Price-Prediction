
# import all required packages

library(tidyverse)
library(GGally)


# load laptop-prediction dataset

dataset = read.csv("Laptop-Price-Prediction-EDA/laptop_data.csv")
head(dataset)

# view(dataset)


dim(dataset) # total 1302 rows and 12 columns


glimpse(dataset) # dataset overview
# from the dataset, we can see many column like RAM, 
# memory, weight are in char datatype, we need to convert them 
# into numeric datatype by removing the unit written after the value.

# check if there any duplicate data present or not.
duplicated(dataset)  # no duplicated found in dataset

# Any Null value?
anyNA(dataset) # No null value present.

# =============================
  ## Data Cleaning
# =============================

# Changing the RAM and Weight dtype to numeric dtype

head(dataset)

# replace All occurance of "GB" and "kg" to "".
dataset$Ram <- gsub(pattern = "GB", replacement = "", x = dataset$Ram)
dataset$Weight <- gsub(pattern = "kg", replacement = "", x = dataset$Weight)

head(dataset)

# Changing the RAM and Weight dtype to numeric dtype

dataset$Ram <- as.integer(dataset$Ram)
dataset$Weight <- as.integer(dataset$Weight)

glimpse(dataset)




