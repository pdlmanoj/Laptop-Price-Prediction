
## REQUIRED PACKAGES
## IMPORT
library(tidyverse)
library(GGally)
library(ggplot2)
library(caret)
library(dplyr)
library(corrplot)
library(caTools)
library(caret)
library(e1071) # Support Vector Regression (SVR)
library(rpart)
library(randomForest) # Random Forest
library(MASS)
library(car) # vif - Variance Inflation Factor

# load laptop-prediction dataset

dataset <- read.csv("Laptop-Price-Prediction-EDA/laptop_data.csv")
head(dataset)

#view(dataset)

summary(data)

dim(dataset) # total 1302 rows and 12 columns


glimpse(dataset)

# check if there any duplicate data present or not.
duplicated(dataset) 

# Any Null value?
anyNA(dataset) # No null value present.

# Whole Dataset each column by column unique value
d = sapply(colnames(dataset), function(col) unique(dataset[[col]]))
print(d)

# =============================
## Data Cleaning
# =============================

# Changing the RAM and Weight dtype to numeric dtype

glimpse(dataset)

# replace All occurance of "GB" and "kg" to "".
dataset$Ram <- gsub(pattern = "GB", replacement = "", x = dataset$Ram)
dataset$Weight <- gsub(pattern = "kg", replacement = "", x = dataset$Weight)

head(dataset)

# Changing the RAM and Weight dtype to numeric dtype

dataset$Ram <- as.integer(dataset$Ram)
dataset$Weight <- as.double(dataset$Weight)


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

head(dataset$Memory)
# dataset$Memory # lots of information 
unique(dataset$Memory) # what are the unique one - 39 chan
table(dataset$Memory) # value count

# Feature Engineering

# many categories and some only have 1 (difficult to prediction)
# some with both ssd and hdd, some have only ssd, some have hybrid, flash storage.
# so, we separate the value in 4 column - 1. Hard drive 2. SSD 3. Flash Storage, 4. Hybrid

dataset$Memory <- gsub("\\.0", "", as.character(dataset$Memory))
dataset$Memory <- gsub("GB", "", dataset$Memory)
dataset$Memory <- gsub("TB", "000", dataset$Memory)
new <- strsplit(dataset$Memory, "\\+", fixed = TRUE)
table(unlist(new))

dataset$first <- sapply(new, function(x) x[1])
dataset$first <- trimws(dataset$first)
dataset$second <- sapply(new, function(x) ifelse(length(x) > 1, x[2], NA))
unique(dataset$first)
head(dataset)

dataset$Layer1hdd <- ifelse(grepl("HDD", dataset$first), 1, 0)
dataset$Layer1ssd <- ifelse(grepl("SSD", dataset$first), 1, 0)
dataset$Layer1Hybrid <- ifelse(grepl("Hybrid", dataset$first), 1, 0)
dataset$Layer1Flash_Storage <- ifelse(grepl("Flash Storage", dataset$first), 1, 0)
head(dataset)

dataset$first <- gsub("\\D", "", dataset$first)
head(df)

dataset$second <- ifelse(is.na(dataset$second), "0", dataset$second)

dataset$Layer2hdd <- ifelse(grepl("HDD", dataset$second), 1, 0)
dataset$Layer2ssd <- ifelse(grepl("SSD", dataset$second), 1, 0)
dataset$Layer2Hybrid <- ifelse(grepl("Hybrid", dataset$second), 1, 0)
dataset$Layer2Flash_Storage <- ifelse(grepl("Flash Storage", dataset$second), 1, 0)

dataset$second <- gsub("\\D", "", dataset$second)
head(dataset)


dataset$first <- as.integer(dataset$first)
dataset$second <- as.integer(dataset$second)

dataset$HDD <- (dataset$first * dataset$Layer1hdd) + (dataset$second * dataset$Layer2hdd)
dataset$SSD <- (dataset$first * dataset$Layer1ssd) + (dataset$second * dataset$Layer2ssd)
dataset$Hybrid <- (dataset$first * dataset$Layer1Hybrid) + (dataset$second * dataset$Layer2Hybrid)
dataset$Flash_Storage <- (dataset$first * dataset$Layer1Flash_Storage) + (dataset$second * dataset$Layer2Flash_Storage)



dataset <- dataset[, !(names(dataset) %in% c("first", "second", "Layer1hdd", "Layer1ssd", "Layer2hdd", "Layer2ssd", "Layer1Flash_Storage", "Layer2Flash_Storage", "Layer1Hybrid", "Layer2Hybrid"))]

# If we see correlation of Price with Hybrid and Flash_Stroage doesn't have less correlation.
# So, we drop this two column

dataset <- dataset[, !(names(dataset) == "Memory"), drop = FALSE]
head(dataset)


# GPU Column
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

# Relationship between OS and Price
ggplot(dataset, aes(x = factor(OS), y = Price, fill = Price, colour = Price)) + 
  geom_bar(stat = "identity", position = "dodge")
# windows laptop are more expansive compare to other's


# Weight Column
head(dataset)

# Relationship btn Weight and Price
ggplot(dataset, aes(x = Weight, y = Price)) +
  geom_point() +
  labs(x = "Weight", y = "Price", title = "Weight VS Price") +
  theme(text = element_text(size = 12))
# Sightly high Price with more weight laptop but not that much.

# Company vs Price box plot
ggplot(data= dataset, aes(x= Company, y= Price, fill= Company))+
  geom_boxplot()+
  ggtitle("Company VS Price")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


head(dataset)



glimpse(dataset)

# Only numeric column - cor()
cor(dataset[,unlist(lapply(dataset, is.numeric))])

# ploting correlation Graph
numericData <- dataset[,sapply(dataset, is.numeric)] #filter all numeric vars
numericData <- numericData[, -c(1, 15)] #drop the id column and dependent var
corMat <- cor(numericData) #correlation matrix
corrplot(corMat, method = "number", type = "lower")
# Which one is high correlated.
highlyCorrelated <- findCorrelation(corMat, cutoff = 0.7) #find highly correlated
highlyCorCol <- colnames(numericData)[highlyCorrelated]
highlyCorCol # SSD is as we can see.

head(dataset)


# What's About our Price Density

ggplot(dataset, aes(x = Price)) +
  geom_density(aes(fill = "Density"), alpha = 0.5) +
  geom_bar(aes(y = ..density.., fill = "Count"), alpha = 0.5, stat = "density") +
  scale_fill_manual(values = c("Density" = "blue", "Count" = "red")) +
  labs(title = "Price Distribution", x = "Price", y = "Count") +
  theme_minimal()
# As we can see, our targeted column Price is right skewed 
# By transforming it to normal. Performance of the algorithm 
# will increase.

ggplot(dataset, aes(x = log(Price))) +
  geom_freqpoly(binwidth = 0.2, size = .8, color = "red") +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black") +
  xlab("Log(Price)") +
  ylab("Frequency / Count") +
  ggtitle("Distribution of Logarithm of Price")

head(dataset)

#### Using log transformation in price column
dataset$Price = log(dataset$Price)
dataset

# Encoding cateogrical column to numeric
# Specify the categorical columns
catcols <- c("ScreenResolution", "Company", "TypeName", "OS_category", "Cpu_brand", "Gpu_brand")

# Encode categorical variables as integers using label encoding
for (col in catcols) {
  dataset[[col]] <- as.integer(factor(dataset[[col]]))
}

dataset



#### Compute the variance inflation factors (VIF)

vif_values <- vif(lm(Price ~ ., data = dataset))

# Print the VIF values
print(vif_values)

# Building Model
library(MASS)
model = rlm(Price ~ ., data = dataset)
residuals = residuals(model)


mad = median(abs(residuals - median(residuals)))
threshold = 3 * mad
outliers = which(abs(residuals) > threshold)


data_no_outliers = dataset %>%
  filter(!row_number() %in% outliers)


# Creating testing and training set

set.seed(123)
split = sample.split(data_no_outliers$Price, SplitRatio = .85)

training_set = subset(data_no_outliers, split== TRUE)
test_set = subset(data_no_outliers, split == FALSE)
y_test = test_set$Price


head(training_set)
head(test_set)
head(y_test)

# 1. Linear Regression

reg_1 = lm(formula = Price~.-Hybrid -Weight -TypeName,
           data = training_set)

summary(reg_1)

#printing adjusted R-squared

summary(reg_1)$adj.r.squared # 75 % accurary


y_pred = predict(reg_1, newdata= test_set)

# Comparing Traning Price Prediction with Real Price
comparison = data.frame(predicted= exp(y_pred), True= exp(y_test))
print(comparison)


#2. Support Vector Regression (SVR)
reg_2 = svm(formula= Price~.,
            data= training_set,
            type= 'eps-regression',
            kernel= 'radial',
            sigma= 0.1,
            C = 1)
#Prediction 

y_pred = predict(reg_2, newdata= test_set)

# Calculate R-squared score
r2_score = R2(y_test, y_pred)

# Calculate mean absolute error
mae = MAE(y_test, y_pred)

# Print R-squared score and mean absolute error
print(paste("R2 score:", r2_score)) # 87% accurarcy
print(paste("MAE:", mae))

# Comparing Traning Price Prediction with Real Price
comparison = data.frame(predicted= exp(y_pred), True= exp(y_test))
print(comparison)

## Bettter Result than Linear Regression

##. Random Forest

set.seed(1234)
reg_4 = randomForest(
  x= training_set[-7],
  y= training_set$Price,
  ntree= 200,
  mtry = 4,
  
)

#prediction

y_pred = predict(reg_4, newdata = test_set)

#calculate R2 score

r2_score = R2(y_pred, y_test)

#calculate MAE score

mae = MAE(y_pred, y_test)

#print R2 and mae score

print(paste("R2 score:", r2_score)) # 89 % accuracy
print(paste("MAE Score:", mae))

# Comparing Training Price Prediction with Real Price
comparison = data.frame(predicted= exp(y_pred), True= exp(y_test))
print(comparison)



