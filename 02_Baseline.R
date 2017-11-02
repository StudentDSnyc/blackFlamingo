# Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)

# Load Helper Functions
source("Helpers.R")

# Load Data
load("./house_imputed.RData") # Loads houses.train and houses.test

# Take log of house price label
houses.train$SalePrice <- log(houses.train$SalePrice + 1)

plot(model.baseline)

# RMSE of model on our test split
private.test[which(private.test$Condition2 %in% c("PosA", "RRNn")),]$Condition2 <- "Norm"
private.test[which(private.test$Exterior1st %in% c("ImStucc")),]$Exterior1st <- "VinylSd"
## Not sure how to handle missing factor levels in test set

#Create Dummy Variables (full training data)
# encode using caret::dummyVars
features.to.encode <- c("MSZoning", "OverallQual", "OverallCond", "ExterCond") # test with a few variables
f <- paste("~", paste(features.to.encode, collapse="+"))
encoder <- caret::dummyVars(as.formula(f),
                            data = houses.train,
                            sep = ".",
                            fullRank = FALSE) # check meaning of this. Includes no interactions between variables e.g. feature1:feature2
encoded.houses.train <- predict(encoder, houses.train)

encoded.houses.train <- cbind(as.data.frame(encoded.houses.train), houses.train$SalePrice)
# rename imported column 
colnames(encoded.houses.train)[which(names(encoded.houses.train) == "houses.train$SalePrice")] <- "SalePrice"

# Split our data into a train and test set - 80/20
set.seed(0)
split_ratio = 0.8
train = sample(1:nrow(houses.train), nrow(encoded.houses.train)*split_ratio)

encoded.private.train = encoded.houses.train[train,]
encoded.private.test = encoded.houses.train[-train,]

# Baseline linear model
model.baseline <- lm(SalePrice ~ ., data=encoded.private.train)
predicted <- predict(model.baseline, encoded.private.test, na.action = na.exclude) 
actual <- encoded.private.test$SalePrice

sqrt(mean((predicted-actual)^2))
