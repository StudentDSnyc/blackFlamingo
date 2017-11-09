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

source('./02_Features.R')
load("./data/houses.train.RData")
load("./data/houses.test.RData")
dim(houses.train) # "data.table" "data.frame"
dim(houses.test) # "data.table" "data.frame"

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
# private.train <- as.data.frame(private.train)
# private.test <- as.data.frame(private.test)
# houses.train <- as.data.frame(houses.train)
# houses.test <- as.data.frame(houses.test)

##################
# Label Count Encoding - No, just puts number of occurences right now
##################
# # Label Count encoding = Replaces labels with the number of occurences (for categorical columns)
# encoded.private.train <- label.count.encode.df(private.train)
# encoded.private.test <- label.count.encode.df(private.test)
# encoded.houses.train <- label.count.encode.df(houses.train)
# encoded.houses.test <- label.count.encode.df(houses.test)
# 
# # Scale all columns except SalePrice
# # Scaling test sets should really be done with means from training set
# encoded.private.train[ , -which(names(encoded.private.train) == "SalePrice")] <- scale(encoded.private.train[ , -which(names(encoded.private.train) == "SalePrice")])
# encoded.private.test[ , -which(names(encoded.private.test) == "SalePrice")] <- scale(encoded.private.test[ , -which(names(encoded.private.test) == "SalePrice")])
# encoded.houses.train[ , -which(names(encoded.houses.train) == "SalePrice")] <- scale(encoded.houses.train[ , -which(names(encoded.houses.train) == "SalePrice")])
# encoded.houses.test[ , -which(names(encoded.houses.test) == "SalePrice")] <- scale(encoded.houses.test[ , -which(names(encoded.houses.test) == "SalePrice")])

##################
# One hot Encoding
##################

# One-hot encode categorical features using vtreat
# Scale all features including dummy ones per: https://stats.stackexchange.com/questions/69568/whether-to-rescale-indicator-binary-dummy-predictors-for-lasso

# old version with dataframes
# encoded.private.train <- encode.scale.df(private.train[ , -which(names(private.train) == "SalePrice")])
# encoded.private.train['SalePrice'] <- private.train$SalePrice

encoded.private.train <- as.data.table(stats::model.matrix(~., data=private.train[ , -c("SalePrice")])[,-1])
cols <- colnames(encoded.private.train)
encoded.private.train[, (cols) := lapply(.SD, scale), .SDcols=cols]
encoded.private.train[, c('SalePrice')] <- private.train$SalePrice

encoded.private.test <- as.data.table(stats::model.matrix(~., data=private.test[ , -c("SalePrice")])[,-1])
cols <- colnames(encoded.private.test)
encoded.private.test[, (cols) := lapply(.SD, scale), .SDcols=cols]
encoded.private.test[, c('SalePrice')] <- 0

encoded.houses.train <- as.data.table(stats::model.matrix(~., data=houses.train[ , -c("SalePrice")])[,-1])
cols <- colnames(encoded.houses.train)
encoded.houses.train[, (cols) := lapply(.SD, scale), .SDcols=cols]
encoded.houses.train[, c('SalePrice')] <- houses.train$SalePrice

encoded.houses.test <- as.data.table(stats::model.matrix(~., data=houses.test[ , -c("SalePrice")])[,-1])
cols <- colnames(encoded.houses.test)
encoded.houses.test[, (cols) := lapply(.SD, scale), .SDcols=cols]
encoded.houses.test[, c('SalePrice')] <- 0


# Add missing columns in test set with default value equal to 0
# This approach was chosen over binding training and test because it would leak information from test
# Also remove columns from test not in training (no predictive value)

# encoded.private.test <- align.columns(encoded.private.train, encoded.private.test)
# encoded.houses.test <- align.columns(encoded.houses.train, encoded.houses.test)


# Save encoded dataframes
save(encoded.private.train, file = "./data/encoded.private.train.RData")
save(encoded.private.test, file = "./data/encoded.private.test.RData")
save(encoded.houses.train, file = "./data/encoded.houses.train.RData")
save(encoded.houses.test, file = "./data/encoded.houses.test.RData")

#Save encoded dataframes as csv:
write.csv(encoded.private.train, "../Data/encoded_private_train.csv", row.names = FALSE)
write.csv(encoded.private.test, "../Data/encoded_private_test.csv", row.names = FALSE)
write.csv(encoded.houses.train, "../Data/encoded_houses_train.csv", row.names = FALSE)
write.csv(encoded.houses.test, "../Data/encoded_houses_test.csv", row.names = FALSE)





