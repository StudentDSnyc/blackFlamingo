# Load Libraries
require(ggplot2)
require(caret)
require(gam)
require(dplyr)

# Load Helper Functions
source("Helpers.R")

##################
# Load
##################
# source("./03_0_SplitEncode.R")
load("./data/privtrain_linear.RData")
load("./data/privtest_linear.RData")
load("./data/housetrain_linear.RData")
load("./data/housetest_linear.RData")

#########################
# Create private test set
#########################

# Split our data into a train and test set - 80/20
set.seed(0)
split.ratio = 0.8
train.indices = sample(1:nrow(encoded.houses.train), nrow(encoded.houses.train)*split.ratio)
encoded.private.train = encoded.houses.train[train.indices,] # dim: 1168, 80 + engineered
encoded.private.test = encoded.houses.train[-train.indices,] # dim: 292, 80 + engineered

# Convert to data.frame (from data.table) 
encoded.private.train <- as.data.frame(encoded.private.train)
encoded.private.test <- as.data.frame(encoded.private.test)
encoded.houses.train <- as.data.frame(encoded.houses.train)
encoded.houses.test <- as.data.frame(encoded.houses.test)

#######################
# Splines Linear Model
#######################

# First run simple linear
set.seed(123)
require(splines)
train.control <- trainControl("cv", 10, savePred=T)
fit <- train(log(SalePrice) ~ ., 
             data=encoded.houses.train,
             method="glm",
             trControl=train.control)

# show predictions from CV
head(fit$pred)
# RMSE
sqrt(mean((fit$pred$pred - fit$pred$obs)^2)) # 0.1505522
cbind(fit$pred$obs, fit$pred$pred, log(encoded.houses.train$SalePrice)) %>% head

# Test Linear with caret pre-processing ####

# Run simple linear
fit.caret.preprocess <- train(log(SalePrice) ~ ., 
                              data=encoded.houses.train,
                              preProcess = "pca",
                              method="glm",
                              trControl=trainControl("cv", 10, savePred=T))

fit.caret.preprocess # RMSE 0.1445636
# Predict on same training data
pred.caret.preprocess  <- predict(fit.caret.preprocess, newdata = encoded.houses.train)
cbind(pred.caret.preprocess , log(encoded.houses.train$SalePrice)) %>% head
sqrt(mean((pred.caret.preprocess  - log(encoded.houses.train$SalePrice))^2)) # 0.1258621 - improvement

encoded.houses.train.withpred <- cbind(encoded.houses.train, pred.caret.preprocess)

# Fitting splines using the initial estimate of the house price --> different slopes in model
spline.fit <- train(log(SalePrice) ~ .-pred.caret.preprocess +bs(pred.caret.preprocess, df=10), 
                    data=encoded.houses.train.withpred,
                    preProcess = "pca",
                    method="glm",
                    trControl=trainControl("cv", 10, savePred=T))
# RMSE
sqrt(mean((spline.fit$pred$pred - spline.fit$pred$obs)^2)) # 0.143705 no meaninful improvement

# Running with the variable for doing the splines only
spline.test <- train(log(SalePrice) ~ bs(pred.caret.preprocess, df=7), 
                    data=encoded.houses.train.withpred,
                    # preProcess = "pca",
                    method="glm",
                    trControl=trainControl("cv", 10, savePred=T))
# RMSE
sqrt(mean((spline.test$pred$pred - spline.test$pred$obs)^2)) # 0.1296604 improvement

# Spline Cross Validation
set.seed(1331)
n_folds <- 5
folds <- createFolds(rownames(encoded.houses.train), k=n_folds)
splines.predictions <- list()
RMSE.folds <- list()

degrees_freedom <- 5
for (i in 1:n_folds) {
  spline.fit <- glm(log(SalePrice) ~ bs(pred.caret.preprocess, df=degrees_freedom), # df=6
                    data=encoded.houses.train.withpred[-folds[[i]], ])
  splines.predictions[[i]] <- predict(spline.fit, 
                                      newdata = encoded.houses.train.withpred[folds[[i]], -which(names(encoded.houses.train.withpred) == "SalePrice")])
  # RMSE for the fold
  RMSE <- sqrt(mean((splines.predictions[[i]] - log(encoded.houses.train.withpred[folds[[i]],c("SalePrice")]))^2))
  cat(RMSE)
  RMSE.folds[[i]] <- RMSE
}
# Mean RMSE
mean(as.numeric(RMSE.folds)) # 0.1290179 some improvement


# write.csv(data.frame(Id = 1:1459, splines_predictions = exp(splines.predictions.test)), 
#           paste(format(Sys.time(),'%Y-%m-%d %H-%M-%S'), "splines_predictions.csv"), 
#           row.names = FALSE)


#######################
# GAM
#######################
require(gam)
gam.fit <- gam(log(SalePrice) ~ .-pred.caret.preprocess +s(pred.caret.preprocess, df=6),
               data=encoded.houses.train.withpred)

set.seed(123)
train.control <- trainControl("cv", 10, savePred=T)
fit <- train(log(SalePrice) ~ .-pred.caret.preprocess +s(pred.caret.preprocess, df=6), 
             data=encoded.houses.train.withpred,
             method="glm",
             trControl=train.control)

# show predictions from CV
head(fit$pred)
# RMSE
sqrt(mean((fit$pred$pred - fit$pred$obs)^2)) # 0.1475498 Not good compared to splines


