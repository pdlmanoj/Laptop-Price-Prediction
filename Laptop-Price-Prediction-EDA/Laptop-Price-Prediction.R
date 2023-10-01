
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
unique(dataset$ScreenResolution) # only unique value (what are those value?)

# table(dataset$ScreenResolution) # - count values

# As, we observe the unique values we can see information related 
# are a laptop touch screen or not, the IPS panel, 
# and the X-axis and Y-axis screen resolution.

#1. We will extract touch screen information

# Fetching touchscreen data from Screen resolution column
dataset$touchscreen <- ifelse(grepl("Touchscreen", dataset$ScreenResolution), 1, 0)
# It is a binary variable so we can encode it as 0 and 1. 
# 1 means the laptop is with a touch screen and 0 indicates not a touch screen.

head(dataset) # we separated touchscreen data
table(dataset$touchscreen) # value count

#Number of Touchscreen laptop - have / not have
barplot(table(dataset$touchscreen), main='Number of TouchScreen laptop',ylab = "No. of occurance",xlab = "TouchScreen", col='blue', breaks=12)
# non-touch screen laptop are most in dataset 

# Relationship between Touchscreen and Price
ggplot(dataset, aes(x = factor(touchscreen), y = Price, fill = Price, colour = Price)) + 
  geom_bar(stat = "identity", position = "dodge")
# By observing, touchscreen laptop price is high.

# 2. Fetching IPS panel 
dataset$Ips <- ifelse(grepl("IPS Panel", dataset$ScreenResolution), 1, 0)

head(dataset)
table(dataset$Ips)
barplot(table(dataset$Ips), main='Number of IPS Panel laptop',ylab = "Number",xlab = "IPS Panel", col='blue', breaks=12)
# Laptop with IPS Panel are less in our dataset.


# Relationship between IPS and Price
ggplot(dataset, aes(x = factor(Ips), y = Price, fill = Price, colour = 'red')) + 
  geom_bar(stat = "identity", position = "dodge")

# We can see, without IPS Panel laptop are expensive than with IPS panel.
#So, with IPS Panel doesn't mean Price will be high.

#3. Fetching X-axis and Y-axis screen resolution

res <- strsplit(dataset$ScreenResolution, "x", fixed = TRUE)
res # seperated by x (cross symbol) eg: "1920x1080" to "1920" "1080"

dataset$X_res <- sapply(res, function(x) x[1])
dataset$Y_res <- sapply(res, function(x) x[2])
head(dataset)

dataset$X_res <- gsub(",", "", dataset$X_res)
dataset$X_res <- as.numeric(regmatches(dataset$X_res, regexpr("\\d+\\.?\\d+", dataset$X_res)))
glimpse(dataset) 
# X and Y res are char dtype so need to convert into numeric dtype
dataset$Y_res <- as.numeric(dataset$Y_res)
glimpse(dataset)

# Let see, what the correlation of all columns with Price 

column_to_compare <- dataset$Price
correlations <- sapply(dataset[sapply(dataset, is.numeric)], function(x) cor(x, column_to_compare))
correlations

# we can see that inches do not have a strong correlation, but X and Y-axis resolution do,
# so we can take advantage of this and convert these three columns to a single column 
# known as Pixel per inches (PPI). 

# Replacing inches, X and Y resolution to PPI
dataset$ppi <- (((dataset$X_res^2) + (dataset$Y_res^2))^0.5) / as.numeric(dataset$Inches)

head(dataset)           

# Drop the extra columns which are not helpful our price prediction
# X','X_res', 'Y_res', 'Inches', 'ScreenResolution'
dataset <- dataset[, !(names(dataset) %in% c('X','X_res', 'Y_res', 'Inches', 'ScreenResolution'))]
glimpse(dataset)


# Now, CPU column, does it affect Price?
unique(dataset$Cpu)

# Cpu have 117 different categories, which give information about pre-processor and 
# speed of laptops.

# Extract the pre-processor - i3,i5, i7, i6,AMD,....

# extract the first three words from the Cpu name
dataset$CpuName <- sapply(dataset$Cpu, function(x) {
  words <- unlist(strsplit(x, " "))
  if (length(words) >= 3) {
    paste0(words[1:3], collapse = " ")
  } else {
    paste0(words, collapse = " ")  # Use all available words if less than 3
  }
})


fetch_processor <- function(text) {
  if (text == 'Intel Core i7' || text == 'Intel Core i5' || text == 'Intel Core i3') {
    return(text)
  } else {
    if (strsplit(text, " ")[[1]][1] == 'Intel') {
      return('Other Intel Processor')
    } else {
      return('AMD Processor')
    }
  }
}
dataset$Cpu_brand <- sapply(dataset$CpuName, fetch_processor)
head(dataset)


barplot(table(dataset$Cpu_brand), main='Number of CPU brand',ylab = "Count",xlab = "CPU brand", col='blue', breaks=12)
# Intel i7 and i5 are the most in the dataset.

table(dataset$Cpu_brand) # value count

# Will price vary based on cpu processor?

# Relationship between Cpu brand and Price
ggplot(dataset, aes(x = factor(Cpu_brand), y = Price, fill = Price, colour = Price)) + 
  geom_bar(stat = "identity", position = "dodge")
# As, Intel Core i7 have high price than i5,i3,AMD processor.
# Also, the AMD and i5 have almost same range of Price.
# So, we can say, Price will vary according to Cpu processor.

# Drop Cpu and Cpu Name column
dataset <- dataset[, !(names(dataset) %in% c('Cpu', 'CpuName'))]
glimpse(dataset)

# What about RAM vs Price?

barplot(table(dataset$Ram), main='Number of RAMs',ylab = "Count",xlab = "RAM", col='blue', breaks=12)
# Laptop with 8 GB rams are the most in the dataset. 

# Relationship between Price and RAM
ggplot(dataset, aes(x = factor(Ram), y = Price, fill = Price, colour = Price)) + 
  geom_bar(stat = "identity", position = "dodge")
# here, we can see 64 GB RAM price less than 32 GB RAM.
# So, size of high RAM doesn't mean high price.

# Memory vs Price?

# dataset$Memory # lots of information 
unique(dataset$Memory) # what are the unique one - 39 chan
table(dataset$Memory) # value count

# Feature Engineering

# many categories and some only have 1 (difficult to prediction)
# some with both ssd and hdd, some have only ssd, some have hybrid, flash storage.
# so, we separate the value in 4 column - 1. Hard drive 2. SSD 3. Flash Storage, 4. Hybrid

# glimpse(dataset)

# dataset$Memory <- gsub("\\.0", "", as.character(dataset$Memory))
# #Replacing GB to ''(null or empty)
# dataset$Memory <- gsub("GB", "", dataset$Memory)
# #Replacing TB to 000 (1TB = 1000GB)
# dataset$Memory <- gsub("TB", "000", dataset$Memory)
# split 64GB Flash Storage +  1TB(000) HDD
# new <- strsplit(dataset$Memory, "\\+")[1][2]



#----------------------

table(dataset$Gpu)
# Many different graphics card information, but we only select brand name of
# particular graphics card

dataset$Gpubrand <- sapply(dataset$Gpu, function(x) {strsplit(x, " ")[[1]][1]})
table(dataset$Gpubrand)
dataset <- dataset[dataset$`Gpubrand` != 'ARM', ] # we don't need GPU ARM only brand name.
table(dataset$Gpubrand)

barplot(table(dataset$Gpubrand), main='count of GPU',ylab = "Count",xlab = "GPU", col='blue', breaks=12)
# Intel GPU are most in dataset.

# Relationship between GPU and Price
ggplot(dataset, aes(x = factor(Gpubrand), y = Price, fill = Price, colour = Price)) + 
  geom_bar(stat = "identity", position = "dodge")
# Nvidia GPU are much expansive than Intel and AMD

# Drop GPU column
dataset <- dataset[, !(names(dataset) %in% c('Gpu'))]
head(dataset)


# Operating System

table(dataset$OpSys) 
# we can see, there is Mac OS x, MacOs | Windows 10 , Windows 10s
# To make easy, we keep all Mac categories at one, same to windows
cat_os <- function(os) {
  if (os == 'Windows 10' || os == 'Windows 7' || os == 'Windows 10 S') {
    return('Windows')
  } else if (os == 'Mac OS X' || os == 'macOS') {
    return('Mac')
  } else {
    return('others/NO OS/Linux')
  }
}

dataset$OS <- sapply(dataset$OpSys, cat_os)
head(dataset)
table(dataset$OS) 

# Drop OpSys column
dataset <- dataset[, !(names(dataset) %in% c('OpSys'))]
head(dataset)

barplot(table(dataset$OS), main='count of Operating System',ylab = "Count",xlab = "OS", col='blue', breaks=12)
# Intel GPU are most in dataset.

# Relationship between GPU and Price
ggplot(dataset, aes(x = factor(OS), y = Price, fill = Price, colour = Price)) + 
  geom_bar(stat = "identity", position = "dodge")
# windows laptop are more expansive compare to other's
