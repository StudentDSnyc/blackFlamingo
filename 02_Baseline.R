# Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)

# Load Helper Functions
source("Helpers.R")

# Load Data
load("./house_imputed.RData") # Loads houses.train and houses.test

# Split our data into a train and test set - 80/20
set.seed(0)
split_ratio = 0.8
train = sample(1:nrow(houses.train), nrow(houses.train)*split_ratio)

private.train = houses.train[train,]
private.test = houses.train[-train,]

# Baseline linear model
houses.train$SalePrice <- log(houses.train$SalePrice + 1)
model.baseline <- lm(SalePrice ~ ., data=private.train)

plot(model.baseline)

# RMSE of model on our test split
private.test[which(private.test$Condition2 %in% c("PosA", "RRNn")),]$Condition2 <- "Norm"
private.test[which(private.test$Exterior1st %in% c("ImStucc")),]$Exterior1st <- "VinylSd"
## Not sure how to handle missing factor levels in test set

#Create Dummy Variables
features.to.encode <- c("MSZoning", "OverallQual", "OverallCond") # test with a few variables
f <- paste("~", paste(features.to.encode, collapse="+"))
encoder <- caret::dummyVars(as.formula(f),
                            data = houses.train,
                            sep = ".",
                            fullRank = FALSE) # check meaning of this. Includes no interactions between variables e.g. feature1:feature2
encoded.houses.train <- predict(encoder, houses.train)

print(colnames(encoded.houses.train))



predicted <- predict(model.baseline, private.test, na.action = na.exclude) # fails: new levels in test  
actual <- private.test$SalePrice

sqrt(mean((predicted-actual)^2))