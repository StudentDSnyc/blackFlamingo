# Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)

# Load Helper Functions
source("Helpers.R")

# Load Data
load("./house_imputed.RData") # Loads houses.train and houses.test
houses.train <- as.data.frame(houses.train)

##################
# Encoding
##################

# Take log of house price label
houses.train$SalePrice <- log(houses.train$SalePrice + 1)

# private.test[which(private.test$Condition2 %in% c("PosA", "RRNn")),]$Condition2 <- "Norm"
# private.test[which(private.test$Exterior1st %in% c("ImStucc")),]$Exterior1st <- "VinylSd"
## Not sure how to handle missing factor levels in test set

# Scale numeric features
indices <- sapply(houses.train, is.numeric)
indices["SalePrice"] <- FALSE # don't scale the labels
houses.train[indices]  <- data.frame(lapply(houses.train[indices], scale))

# One-hot encoding of categorical features
# using vtreat over caret::dummyVars because handles new levels in test set
set.seed(0)
vtreat_encoder <- vtreat::designTreatmentsZ(houses.train, 
                                   colnames(houses.train), 
                                   minFraction= 0,
                                   verbose=FALSE)

# restrict to common variables types
# see vignette('vtreatVariableTypes', package = 'vtreat') for details
sf <- vtreat_encoder$scoreFrame
newvars <- sf$varName[sf$code %in% c("lev", "clean", "isBAD")] 
houses.train.Vtreat <- as.data.frame(vtreat::prepare(vtreat_encoder, 
                                         houses.train, 
                                         varRestriction = newvars))
# Save encoded data.frame
save(houses.train.Vtreat, file = "./house_encoded.RData")

##################
# Run Linear Model
##################


# Split our data into a train and test set - 80/20
set.seed(0)
split_ratio = 0.8
train = sample(1:nrow(houses.train.Vtreat), nrow(houses.train.Vtreat)*split_ratio)

private.train = houses.train.Vtreat[train,]
private.test = houses.train.Vtreat[-train,]


# Baseline linear model
model.baseline <- lm(SalePrice_clean ~ ., data=private.train)
#plot(model.baseline) # review assumptions
predicted <- predict(model.baseline, private.test, na.action = na.exclude) 
actual <- private.test$SalePrice

sqrt(mean((predicted-actual)^2))






