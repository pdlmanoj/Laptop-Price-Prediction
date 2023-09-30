
# import all required packages

library(tidyverse)
library(GGally)
library(ggplot2)


# load laptop-prediction dataset

dataset = read.csv("Laptop-Price-Prediction-EDA/laptop_data.csv")
head(dataset)

# view(dataset)


dim(dataset) # total 1302 rows and 12 columns


glimpse(dataset) 

# check if there any duplicate data present or not.
duplicated(dataset) 

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


# =============================
## Data visualization / EDA
# =============================

# Which price laptop were more sold?
hist(dataset$Price, main='Distribution of Price', xlab='Price', col='blue', breaks=12)

# Which company have higher number of laptop ? - Dell, Lenovo, Hp
ggplot(data = dataset) +
  geom_bar(mapping = aes(x = Company))

# company relationship with price 
# which company laptop have high price?
ggplot(dataset, aes(x = factor(Company), y = Price, fill = Price, colour = Price)) + 
  geom_bar(stat = "identity", position = "dodge")
# Razer laptop are more expansive laptop, then other brand


# which type of laptop are available most in our dataset?
barplot(table(dataset$TypeName), main='Type of Laptop', col='blue', breaks=12)
# we can see, Laptop type Notebook are the most in the data set

# what's the price comparison with laptop type?
ggplot(dataset, aes(x = factor(TypeName), y = Price, fill = Price, colour = Price)) + 
  geom_bar(stat = "identity", position = "dodge")
# we can see, gaming laptop's are more costly than other's

# what about laptop size, do laptop size will vary the price?
hist(dataset$Inches, main='Distribution of Laptop Size', xlab='Inches', col='red', breaks=12)

ggplot(dataset, aes(x = factor(Inches), y = Price, fill = Price, colour = Price)) + 
  geom_bar(stat = "identity", position = "dodge")

# From the plot we can see there is a relationship 
# but not a strong relationship between the price and size column.
# We can say, Size doesn't matter for laptop price.

# =============================
## Feature Engineering
# =============================


# Does Screen Resolution affect Price of Laptop?
dataset$ScreenResolution
# we can see, this column, contain a lot of information. 
sum(duplicated(dataset$ScreenResolution))
duplicated(dataset$ScreenResolution) # lots of duplicate value
unique(dataset$ScreenResolution)

# table(dataset$ScreenResolution) # - count values

# As, we observe the unique values we can see information related 
# are a laptop touch screen or not, the IPS panel, 
# and the X-axis and Y-axis screen resolution.

#1. We will extract touch screen information

