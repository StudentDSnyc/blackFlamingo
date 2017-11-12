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

source('./Features.R')
load("./data/Xhouses.train.RData")
load("./data/Xhouses.test.RData")

#########################
# Create private test set
#########################

# Split our data into a train and test set - 80/20
set.seed(0)
split.ratio = 0.8
train.indices = sample(1:nrow(houses.train), nrow(houses.train)*split.ratio)
private.train = houses.train[train.indices,] # dim: 1168, 80 + engineered
private.test = houses.train[-train.indices,] # dim: 292, 80 + engineered

# Convert to data.frame (from data.table) 
private.train <- as.data.frame(private.train)
private.test <- as.data.frame(private.test)
houses.train <- as.data.frame(houses.train)
houses.test <- as.data.frame(houses.test)

# Save unencoded dataframes
save(private.train, file = "./data/Xprivate.train.RData")
save(private.test, file = "./data/Xprivate.test.RData")

# Save unencoded dataframes as csvs
write.csv(private.train, "../Data/Xprivate_train.csv", row.names = FALSE)
write.csv(private.test, "../Data/Xprivate_test.csv", row.names = FALSE)

