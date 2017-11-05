source("02_Baseline.R")
house = load("./house_imputed.RData")

# private.test and private.train loaded
# SalePrice in both are already log(original SalePrice

# Create a private test set without SalePrice  
privatetest.noSP = private.test[ , -which(names(private.test) %in% c("SalePrice"))]

library(tree)

# Basic decision tree built on private.train with the following defaults:
# mincut = 5, minimum number of observations to include in each child node
# minsize = 10, smallest allowed node size
# mindev = 0.01 
tree.baseline = tree(SalePrice ~ ., data = private.train, control = tree.control(nobs = 1168, mincut = 1, mindev = 1e-03))
tree.baseline
summary(tree.baseline) 
plot(tree.baseline)
text(tree.baseline, pretty = 0)

# Predicting on privatetest.noSP
tree.pred = predict(tree.baseline, privatetest.noSP)

# Assess fit 
tree.baseline.RMSE = sqrt(mean((tree.pred - private.test$SalePrice)^2))
tree.baseline.RMSE
# 0.1958554

