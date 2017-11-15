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
source("./03_0_SplitEncode.R")
load("./data/private.train.RData")
load("./data/private.test.RData")

######################
# Basic Decision Tree
######################


# Create a private test set without SalePrice  
#privatetest.noSP = private.test[ , -which(names(private.test) %in% c("SalePrice"))]

library(tree)

# Basic decision tree built on private.train. Note that tree.control defaults are
# mincut = 5, minimum number of observations to include in each child node; minsize = 10, 
# smallest allowed node size; and mindev = 0.01. These defaults are NOT used because 
# they produce a tree built from only 3 out of the 79 predictor variables. 
tree.baseline = tree(log(SalePrice) ~ ., data = private.train, 
                     control = tree.control(nobs = 1168, mincut = 2, mindev = 1e-03))
tree.baseline
summary(tree.baseline) 

# Cross validate results on private.train
set.seed(0)
cv.baseline = cv.tree(tree.baseline)
par(mfrow = c(1, 2))
plot(cv.baseline$size, cv.baseline$dev, type = "b",
     xlab = "Terminal Nodes", ylab = "RSS")
plot(cv.baseline$k, cv.baseline$dev, type  = "b",
     xlab = "Alpha", ylab = "RSS")

# Unpruned prediction on privatetest.noSP
unpruned.tree.pred = predict(tree.baseline, privatetest.noSP)

# Assessment of unpruned tree fit
tree.baseline.RMSE = sqrt(mean((unpruned.tree.pred - log(private.test$SalePrice))^2))
tree.baseline.RMSE
# 0.1857989

######################
# Pruning
######################

# Now investigate results when pruning is introduced
# Pruning the tree to have the optimal number of terminal nodes according to 
# the previous plot
pruned.baseline = prune.tree(tree.baseline, best = 11)
summary(pruned.baseline) # Only OverallQual, Adj.GrLivArea, Neighborhood and 
                         # GarageArea used in pruned tree

# Pruned prediction on privatetest.noSP
pruned.tree.pred = predict(pruned.baseline, privatetest.noSP)

# Assessment of pruned tree fit
pruned.baseline.RMSE = sqrt(mean((pruned.tree.pred - log(private.test$SalePrice))^2))
pruned.baseline.RMSE
# 0.2053511

## NOTE: Although the plot from cross validation would have us pick 11 terminal
#        nodes, I manually looked through larger values and found that RMSE 
#        grew smaller as we approached the number of terminal nodes in the original
#        unpruned tree. Namely, as the number of terminal nodes increased, our
#        pruned tree's RMSE approached that of the unpruned tree. This suggests
#        that our tree should be left unpruned. 

# NOTE: Since the unpruned tree was better, we retrain on the full test set using the
# unpruned tree. 

# Retrain on full training set houses.train
set.seed(0)
tree.best = tree(log(SalePrice) ~ ., data = houses.train, 
                 control = tree.control(nobs = 1460, mincut = 2, mindev = 1e-03))
#summary(tree.best)

# Predict on full test set houses.test
tree.best.prediction = predict(tree.best, houses.test)

# Create submission file
write.csv(data.frame(Id = 1461:2919, SalePrice = exp(tree.best.prediction)), 
          paste(format(Sys.time(),'%Y-%m-%d %H-%M-%S'), "house_submission.csv"), 
          row.names = FALSE)

# 2017-11-06 20-33-55 house_submission.csv

