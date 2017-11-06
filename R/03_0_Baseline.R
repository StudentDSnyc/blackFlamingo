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

# load("./house_imputed.RData") 
load("./houses.train.RData")
load("./houses.test.RData")
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

bc <- boxCox(model.baseline)
bc.lambda = bc$x[which(bc$y == max(bc$y))]

x <- encoded.private.train %>% select(-SalePrice)
y <- (encoded.private.train$SalePrice^bc.lambda - 1)/bc.lambda

model.bc <- lm(y ~ . , data=x)

predicted <- predict(model.bc, encoded.private.test, na.action = na.exclude) 
predicted <- log(unbox(predicted, bc.lambda))
actual <-log(private.test$SalePrice)

# RMSE
sqrt(mean((predicted-actual)^2))

##################
# Regularization
##################

#Values of lambda over which to check.
grid_lambda = 10^seq(1, -3, length = 20)
#plot(1:20, grid_lambda)
grid_alpha = seq(0, 1, length=21)

# Cross-Validation for alpha & lambda
set.seed(1000)
train.control = trainControl(method = 'cv', number=10, verboseIter = TRUE)
tune.grid = expand.grid(lambda = grid_lambda, alpha=grid_alpha)

# test add interactions
# library("xyz")
# result <- xyz_regression(data.matrix(encoded.private.train[ , -which(names(encoded.private.train) == "SalePrice")]),
#                          as.vector((encoded.private.train$SalePrice^bc.lambda - 1)/bc.lambda),
#                          lambdas = 10^seq(1, -3, length = 10), n_lambda = 10,
#                          alpha = 0.1, L = 10,
#                          standardize = TRUE,
#                          standardize_response = TRUE)
# Model parameters:
#   intercept: -6.193536e-16
# Printing effects for lambda10=0.001
# Main effects:
#   Main effect: 231 coefficient: 0.3058002 # GrLivArea_clean
# Main effect: 118 coefficient: 0.1289097 # YearBuilt_clean
# Main effect: 22 coefficient: 0.1256254 # LotArea_clean
# Main effect: 215 coefficient: 0.1177676 # TotalBsmtSF_clean
# Main effect: 322 coefficient: 0.06945074 # SaleCondition_lev_x.Partial
# Interaction effect: 
# Reduced from before one-hot encoding
# Interaction effect: (35,45) coefficient: 0.04940778 # "LandContour_lev_x.Lvl" "LandSlope_lev_x.Sev"
# Interaction effect: (67,322) coefficient: 0.04885172 # "Neighborhood_lev_x.StoneBr"  "SaleCondition_lev_x.Partial"
# Interaction effect: (21,22) coefficient: -0.04836661 # "LotFrontage_clean" "LotArea_clean"
# Interaction effect: (238,240) coefficient: -0.04272843 # "KitchenQual_lev_x.Ex" "KitchenQual_lev_x.Gd"
# Interaction effect: (16,228) coefficient: 0.04085933 # "MSZoning_lev_x.C..all." "CentralAir_lev_x.Y" 

# f <- as.formula( ~ .*.) 
# x <- model.matrix(f, encoded.private.train[ , -which(names(encoded.private.train) == "SalePrice")])[, -1] 
# 
# glmnet.caret = train(x, 
#                      (encoded.private.train$SalePrice^bc.lambda - 1)/bc.lambda,
#                      method = 'glmnet',
#                      trControl = train.control, 
#                      tuneGrid = tune.grid)

# end test variations

glmnet.caret = train(encoded.private.train[ , -which(names(encoded.private.train) == "SalePrice")], 
                    (encoded.private.train$SalePrice^bc.lambda - 1)/bc.lambda,
                    method = 'glmnet',
                    trControl = train.control, 
                    tuneGrid = tune.grid)

alpha <- glmnet.caret$bestTune$alpha
lambda <- glmnet.caret$bestTune$lambda

# RMSE
sqrt(mean((log(unbox(tuned.linear.pred, bc.lambda)) - log(private.test$SalePrice))^2))

# Retraining on full training set

x = model.matrix(SalePrice ~ ., encoded.houses.train)
y = (encoded.houses.train$SalePrice^bc.lambda - 1)/bc.lambda
z = model.matrix(SalePrice ~ ., encoded.houses.test)

glmnet.caret.full.training = glmnet(x, y, 
                                   alpha = alpha, 
                                   lambda = lambda)

linear.pred.full.training = predict(glmnet.caret.full.training, 
                                    newx = z)

# Test quadratic
# f <- as.formula(paste0("SalePrice ~ poly(", paste0(colnames(encoded.private.train[ , -which(names(encoded.private.train) == "SalePrice")]), collapse = ' + '), ", 2)"))


# Create submission file
write.csv(data.frame(Id = 1461:2919, SalePrice = unbox(linear.pred.full.training[,1])), 
          paste(format(Sys.time(),'%Y-%m-%d %H-%M-%S'), "house_submission.csv"), 
          row.names = FALSE)



