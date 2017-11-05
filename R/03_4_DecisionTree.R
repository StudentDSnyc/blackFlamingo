source("03_Baseline.R")
house = load("./house_imputed.RData")

# private.test and private.train loaded
# SalePrice in both are already log(original SalePrice

# Create a private test set without SalePrice  
privatetest.noSP = private.test[ , -which(names(private.test) %in% c("SalePrice"))]

library(tree)

# Basic decision tree built on private.train. Note that tree.control defaults are
# mincut = 5, minimum number of observations to include in each child node; minsize = 10, 
# smallest allowed node size; and mindev = 0.01. These defaults are NOT used because 
# they produce a tree built from only 3 out of the 79 predictor variables. 
tree.baseline = tree(log(SalePrice) ~ ., data = private.train, 
                     control = tree.control(nobs = 1168, mincut = 1, mindev = 1e-03))
tree.baseline
summary(tree.baseline) 
plot(tree.baseline)
text(tree.baseline, pretty = 0)

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
tree.baseline.RMSE = sqrt(mean((tree.pred - log(private.test$SalePrice))^2))
tree.baseline.RMSE
# 0.1958554


# Now investigate results when pruning is introduced
# Pruning the tree to have the optimal number of terminal nodes according to 
# the previous plot
pruned.baseline = prune.tree(tree.baseline, best = )
par(mfrow = c(1, 1))
plot(prune.baseline)
text(prune.baseline, pretty = 0)

# Pruned prediction on privatetest.noSP
pruned.tree.pred = predict(pruned.baseline, privatetest.noSP)

# Assessment of pruned tree fit
pruned.baseline.RMSE = sqrt(mean((pruned.tree.pred - log(private.test$SalePrice))^2))
pruned.tree.baseline.RMSE
#


