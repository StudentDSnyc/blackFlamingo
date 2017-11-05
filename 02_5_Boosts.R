source("02_Baseline.R")
house = load("./house_imputed.RData")

# private.train is a variable containing 80% of our given data, chosen randomly
# private.test is a variable containing 20% of our given data, chosen randomly
# Create a private test set without SalePrice
private.test$SalePrice = as.numeric(private.test$SalePrice)
privatetest.noSP = private.test[ , -which(names(private.test) %in% c("SalePrice"))]

library(gbm)

# Boosting baseline: fitting 10,000 trees with a depth of 4 and shrinkage rate 
# of 0.001 (all the defaults).
set.seed(0)
boost.baseline = gbm(SalePrice ~ ., data = private.train,
                   distribution = "gaussian",
                   n.trees = 10000,
                   interaction.depth = 4,
                   shrinkage = 0.001)

summary(boost.baseline)

# Predict on private.test:
boost.baseline.test = predict(boost.baseline, newdata = privatetest.noSP, n.trees = 1)

# Calculate RMSE:
baseline.RMSE = sqrt(mean((boost.baseline.test - private.test$SalePrice)^2))
# 0.3721532

# Tuning 
library(caret)

# Cross-validation details; include verboseIter = TRUE to print out the
# iteration number and track progress.
fitControl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Specify a grid of all possible tuning parameter combos
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


# Train an informed model based on the above (coarse-search-found) 
# tuning parameters.
set.seed(0)
boost.tuned = gbm(log(SalePrice) ~ ., data = private.train,
                     distribution = "gaussian",
                     n.trees = 1500,
                     interaction.depth = 8,
                     shrinkage = 0.01)

# Predict on private.test
boost.tuned.test = predict(boost.tuned, newdata = privatetest.noSP, n.trees = 1)

# Calculate RMSE:
tuned.RMSE = sqrt(mean((boost.tuned.test - log(private.test$SalePrice))^2))
# 0.3697696




###############################################################################
###############################################################################
# Unused code

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
