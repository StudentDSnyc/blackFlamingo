source("03_Baseline.R")
house = load("./house_imputed.RData")

# private.train is a variable containing 80% of our given data, chosen randomly
# private.test is a variable containing 20% of our given data, chosen randomly
private.test$SalePrice = as.numeric(private.test$SalePrice)

# Create a private test set without SalePrice
privatetest.noSP = private.test[ , -which(names(private.test) %in% c("SalePrice"))]
housestest.noSP = houses.test[ , -which(names(houses.test) %in% c("SalePrice"))]

library(gbm)
library(caret)

# Tuning across four hyperparameters

# Define cross-validation details to be used in the tuning process
# Include verboseIter = TRUE to print out the iteration number and track progress.
fitControl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Specify a grid of all possible tuning parameter combos; use expand.grid to allow
# for multiple parameters to change
gbmGrid = expand.grid(interaction.depth = seq(4, 10, by = 2),
                      n.trees = seq(1000, 3000, by = 100),
                      shrinkage = 10^seq(-3, -1, by = 1),
                      n.minobsinnode = 10)

# Train one model per tuning parameter combo; using train() in 
# the caret package will automatically include the best model 
# according to the specified metric. This best model can be found
# by calling gbmTuneTest$bestTune afterwards.
set.seed(0)
gbmTuneTest <- train(log(SalePrice) ~ ., data = private.train, 
                     method = "gbm", 
                     trControl = fitControl, verbose = FALSE,
                     tuneGrid = gbmGrid, metric = 'RMSE')
#Note: n.minobsinnode parameter default = 10 used.

gbmTuneTest$bestTune


# Original results:
# 35th combo: n.trees = 1500, interaction.depth = 8, shrinkage = 0.01,
#             and n.minobsinode = 10.