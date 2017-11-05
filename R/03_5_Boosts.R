source("03_0_Baseline.R")
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
                      n.trees = seq(500, 1500, by = 250),
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
# 35th combo: n.trees = 1500, interaction.depth = 8, shrinkage = 0.01,
#             and n.minobsinode = 10.


# Train a model based on the above (coarse-search-found) tuning parameters
# Note: Boost defaults are 100 trees, interaction depth of 4, shrinkage rate of 0.001,
# and minimum observations per node is 10.
set.seed(0)
boost.tuned = gbm(log(SalePrice) ~ ., data = private.train,
                     distribution = "gaussian",
                     n.trees = 1500,
                     interaction.depth = 8,
                     shrinkage = 0.01)

# Predict on private.test
boost.tuned.test = predict(boost.tuned, newdata = privatetest.noSP, n.trees = 1500)

# Calculate test RMSE:
tuned.RMSE = sqrt(mean((boost.tuned.test - log(private.test$SalePrice))^2))
# 0.1349814


# Retrain on full training set houses.train
set.seed(0)
boost.best = gbm(log(SalePrice) ~ ., data = houses.train,
                  distribution = "gaussian",
                  n.trees = 1500,
                  interaction.depth = 8,
                  shrinkage = 0.01)

# Predict on full test set houses.test
boost.best.prediction = predict(boost.best, newdata = houses.test, n.trees = 1500)

# Create submission file
write.csv(data.frame(Id = 1461:2919, SalePrice = exp(boost.best.prediction)), 
          paste(format(Sys.time(),'%Y-%m-%d %H-%M-%S'), "house_submission.csv"), 
          row.names = FALSE)




###############################################################################
###############################################################################
# Unused code

# # Boosting baseline: fitting 10,000 trees with a depth of 4 and shrinkage rate 
# # of 0.001 (all the defaults).
# set.seed(0)
# boost.baseline = gbm(SalePrice ~ ., data = private.train,
#                      distribution = "gaussian",
#                      n.trees = 10000,
#                      interaction.depth = 4,
#                      shrinkage = 0.001)
# 
# summary(boost.baseline)
# 
# # Predict on private.test:
# boost.baseline.test = predict(boost.baseline, newdata = privatetest.noSP, n.trees = seq(1, 10000))
# 
# # Calculate RMSE:
# baseline.RMSE = sqrt(mean((boost.baseline.test - private.test$SalePrice)^2))
# # 0.3721532


# #Now we wish to tune the number of trees using all other default values:
# #Number of trees to test: 100, 200, 300, ..., 10000
# treenums = seq(from = 100, to = 10000, by = 100)
# 
# 
# #Predict the SalePrice for our private test data using each tree
# predbase = predict(boost.baseline, newdata = private.test, n.trees = treenums)
# 
# #Compute MSE on private.test for each model in the cross-validation
# treenum.err = with(private.test, apply((predbase - Log(SalePrice)^2, 2, mean))
#                    
#                    #Plot MSE against number of trees
#                    plot(n.trees, berr, pch = 16,
#                         ylab = "Mean Squared Error",
#                         xlab = "# Trees",
#                         main = "Boosting Test Error")
#                    
#                    # Conclusion: For a boosted model with interaction depth of 4 and shrinkage of 0.001,
#                    # we should use 6000 trees.



# # If dummified data frames are needed for caret:
# test.dummies = dummyVars(SalePrice ~ ., data = private.test)
# train.dummies = dummyVars(SalePrice ~ ., data = private.train)
# private.testD = predict(test.dummies, newdata = private.test)
# private.trainD = predict(train.dummies, newdata = private.train)
