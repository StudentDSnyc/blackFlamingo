# Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(vtreat)
library(car)

# Load Helper Functions
source("Helpers.R")

##################
# Load
##################

source('02_Features.R')
load("./data/houses.train.RData")
load("./data/houses.test.RData")
class(houses.train) # "data.table" "data.frame"
class(houses.test) # "data.table" "data.frame"

#########################
# Create private test set
#########################

# Split our data into a train and test set - 80/20
set.seed(0)
split.ratio = 0.8
train.indices = sample(1:nrow(houses.train), nrow(houses.train)*split.ratio)
private.train = houses.train[train.indices,] # dim: 1168, 80 + engineered
private.test = houses.train[-train.indices,] # dim: 292, 80 + engineered

# Save unencoded dataframes
save(private.train, file = "./data/private.train.RData")
save(private.test, file = "./data/private.test.RData")

# Save unencoded dataframes as csvs
write.csv(private.train, "private_train.csv", row.names = FALSE)
write.csv(private.test, "private_test.csv", row.names = FALSE)

##################
# Encoding
##################

# Convert to data.frame (from data.table) to avoid bugs below
private.train <- as.data.frame(private.train)
private.test <- as.data.frame(private.test)
houses.train <- as.data.frame(houses.train)
houses.test <- as.data.frame(houses.test)

# One-hot encode categorical features using vtreat
# Scale all features including dummy ones per: https://stats.stackexchange.com/questions/69568/whether-to-rescale-indicator-binary-dummy-predictors-for-lasso
encoded.private.train <- encode.scale.df(private.train[ , -which(names(private.train) == "SalePrice")])
encoded.private.train['SalePrice'] <- private.train$SalePrice

encoded.private.test <- encode.scale.df(private.test[ , -which(names(private.test) == "SalePrice")])
encoded.private.test['SalePrice'] <- 0

encoded.houses.train <- encode.scale.df(houses.train[ , -which(names(houses.train) == "SalePrice")])
encoded.houses.train['SalePrice'] <- houses.train$SalePrice

encoded.houses.test <- encode.scale.df(houses.test[ , -which(names(houses.test) == "SalePrice")])
encoded.houses.test['SalePrice'] <- 0

# Add missing columns in test set with default value equal to 0
# This approach was chosen over binding training and test because it would leak information from test
# Also remove columns from test not in training (no predictive value)
encoded.private.test <- align.columns(encoded.private.train, encoded.private.test)

encoded.houses.test <- align.columns(encoded.houses.train, encoded.houses.test)


# Save encoded dataframes
save(encoded.private.train, file = "./data/encoded.private.train.RData")
save(encoded.private.test, file = "./data/encoded.private.test.RData")
save(encoded.houses.train, file = "./data/encoded.houses.train.RData")
save(encoded.houses.test, file = "./data/encoded.houses.test.RData")

#Save encoded dataframes as csvs:
write.csv(encoded.private.train, "encoded_private_train.csv", row.names = FALSE)
write.csv(encoded.private.test, "encoded_private_test.csv", row.names = FALSE)
write.csv(encoded.houses.train, "encoded_houses_train.csv", row.names = FALSE)
write.csv(encoded.houses.test, "encoded_houses_test.csv", row.names = FALSE)





