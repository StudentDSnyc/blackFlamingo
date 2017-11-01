# Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)

# Load Helper Functions
source("Helpers.R")

# Load Data
load("./house_imputed.RData")

# Split our data into a train and test set - 80/20
set.seed(0)
train = sample(1:nrow(houses.train), 8*nrow(houses.train)/10)

private.train = houses.train[train,]
private.test = houses.train[-train,]

# Baseline linear model
houses.train$SalePrice <- log(houses.train$SalePrice + 1)
model.baseline <- lm(SalePrice ~ ., data=private.train)

plot(model.baseline)

# RMSE of model on our test split
private.test[which(private.test$Condition2 %in% c("PosA", "RRNn"))]$Condition2 <- "Norm"
private.test[which(private.test$Exterior1st %in% c("ImStucc"))]$Exterior1st <- "VinylSd"
## Not sure how to handle missing factor levels in test set

predicted <- predict(model.baseline, private.test, na.action = na.exclude)
actual <- houses.train[test,]$SalePrice

sqrt(mean((predicted-actual)^2))