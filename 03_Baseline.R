# Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(vtreat)

# Load Helper Functions
source("Helpers.R")

##################
# Load
##################

load("./house_imputed.RData") 
class(houses.train) # "data.table" "data.frame"
class(houses.test) # "data.table" "data.frame"

#########################
# Create private test set
#########################

# Split our data into a train and test set - 80/20
set.seed(0)
split.ratio = 0.8
train.indices = sample(1:nrow(houses.train), nrow(houses.train)*split.ratio)
private.train = houses.train[train.indices,] # dim: 1168, 80
private.test = houses.train[-train.indices,] # dim: 292, 80

##################
# Encoding
##################

# Convert to data.frame (from data.table) to avoid bugs below
private.train <- as.data.frame(private.train)
private.test <- as.data.frame(private.test)
houses.train <- as.data.frame(houses.train)
houses.test <- as.data.frame(houses.test)

# Take log of house price label
private.train$SalePrice <- log(private.train$SalePrice + 1)
private.test$SalePrice <- log(private.test$SalePrice + 1)
houses.train$SalePrice <- log(houses.train$SalePrice + 1)
houses.test$SalePrice <- log(houses.test$SalePrice + 1)

# One-hot encode categorical features using vtreat
# Scale all features including dummy ones per: https://stats.stackexchange.com/questions/69568/whether-to-rescale-indicator-binary-dummy-predictors-for-lasso
encoded.private.train <- encode.scale.df(private.train[ , -which(names(private.train) == "SalePrice")])
encoded.private.train['SalePrice'] <- private.train$SalePrice

encoded.private.test <- encode.scale.df(private.test[ , -which(names(private.test) == "SalePrice")])
encoded.private.test['SalePrice'] <- 0

encoded.houses.train <- encode.scale.df(houses.train[ , -which(names(houses.train) == "SalePrice")])
encoded.houses.train['SalePrice'] <- houses.train$SalePrice

encoded.houses.test <- encode.scale.df(houses.test[ , -which(names(houses.test) == "SalePrice")])
encoded.houses.test['SalePrice'] <- 0

# Add missing columns in test set with default value equal to 0
# This approach was chosen over binding training and test because it would leak information from test
# Also remove columns from test not in training (no predictive value)
encoded.private.test <- align.columns(encoded.private.train, encoded.private.test)

encoded.houses.test <- align.columns(encoded.houses.train, encoded.houses.test)


# Save encoded dataframes
save(encoded.private.train, file = "./encoded.private.train.RData")
save(encoded.private.test, file = "./encoded.private.test.RData")
save(encoded.houses.train, file = "./encoded.houses.train.RData")
save(encoded.houses.test, file = "./encoded.houses.test.RData")

#######################
# Baseline Linear Model
#######################


# Baseline linear model
model.baseline <- lm(SalePrice ~ ., data=encoded.private.train)
summary(model.baseline)
#plot(model.baseline) # review assumptions

predicted <- predict(model.baseline, encoded.private.test, na.action = na.exclude) 
actual <-private.test$SalePrice

# RMSE
sqrt(mean((predicted-actual)^2))

##################
# Regularization
##################

#Values of lambda over which to check.
grid_lambda = 10^seq(4, -3, length = 20)
# plot(1:20, grid_lambda)
grid_alpha = seq(0, 1, length=21)

# Cross-Validation for alpha & lambda
set.seed(1000)
train.control = trainControl(method = 'cv', number=10)
tune.grid = expand.grid(lambda = grid_lambda, alpha=grid_alpha)
ridge.caret = train(encoded.private.train[ , -which(names(encoded.private.train) == "SalePrice")], 
                    encoded.private.train$SalePrice,
                    method = 'glmnet',
                    trControl = train.control, 
                    tuneGrid = tune.grid)

alpha <- ridge.caret$bestTune$alpha
lambda <- ridge.caret$bestTune$lambda

tuned.linear.pred = predict(ridge.caret, 
                            newdata = encoded.private.test[ , -which(names(encoded.private.test) == "SalePrice")])
# RMSE
sqrt(mean((tuned.linear.pred - private.test$SalePrice)^2))

# Retraining on full training set

x = model.matrix(SalePrice ~ ., encoded.houses.train)
y = encoded.houses.train$SalePrice
z = model.matrix(SalePrice ~ ., encoded.houses.test)

ridge.caret.full.training = glmnet(x, y, 
                                   alpha = alpha, 
                                   lambda = lambda)

linear.pred.full.training = predict(ridge.caret.full.training, 
                                    newx = z)

# Create submission file
write.csv(data.frame(Id = 1461:2919, SalePrice = exp(linear.pred.full.training[,1])), 
          paste(format(Sys.time(),'%Y-%m-%d %H-%M-%S'), "house_submission.csv"), 
          row.names = FALSE)



