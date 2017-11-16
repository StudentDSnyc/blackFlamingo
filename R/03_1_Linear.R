# Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(vtreat)
library(car)

# Source Helper Functions
source("Helpers.R")

##################
# Load
##################
# source("./03_0_SplitEncode.R")
load("./data/privtrain_linear.RData")
load("./data/privtest_linear.RData")
load("./data/housetrain_linear.RData")
load("./data/housetest_linear.RData")


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
actual <-log(encoded.private.test$SalePrice)

# RMSE
sqrt(mean((predicted-actual)^2))

# Plot actual vs predicted
plot(actual, predicted)
text(actual, predicted, labels=rownames(encoded.private.test), cex= 0.7, pos=3)
abline(0, 1)

#######################
# Cross Validation Linear Model
#######################

# Remove Outliers
which(encoded.houses.train$SalePrice==184750) # index 524
filt.encoded.houses.train <- encoded.houses.train[-c(524),]

require(boot) # Library to perform cross validation on linear regression with cv.glm
# Cross Validation error
set.seed(10)
cv.linear.model <- glm(log(SalePrice) ~ ., data=encoded.houses.train)
cv.err <- cv.glm(encoded.houses.train, cv.linear.model, RMSE, K = 5)$delta[1]
cv.err
# Problem: not sure how to retrieve the cv predictions from here (to see where model is weak)

require(caret)
set.seed(2)
# 5 fold regression
train.control <- trainControl("cv", 5, savePred=T)
fit <- train(log(SalePrice) ~ ., 
             data=encoded.houses.train,
             method="glm",
             trControl=train.control)

# show predictions from CV
head(fit$pred)
# RMSE
sqrt(mean((fit$pred$pred - fit$pred$obs)^2)) 
# Display outlier prediction
fit$pred[which(fit$pred$obs==log(184750)),] # very bad (3x the price)

# with outlier: 0.1439189
# without outlier: 0.1538019 !! improves model (?) despite huge error that it contributes

# Quadratic test
require(caret)
set.seed(2)
# 5 fold regression
train.control <- trainControl("cv", 5, savePred=T)

f <- as.formula(paste0("log(SalePrice) ~ poly(", paste0(colnames(filt.encoded.houses.train[ , -which(names(filt.encoded.houses.train) == "SalePrice")]), collapse = ' + '), ", 2)"))

fit <- train(f, 
             data=filt.encoded.houses.train,
             method="glm",
             trControl=train.control)
# show predictions from CV
head(fit$pred)
# RMSE
sqrt(mean((fit$pred$pred - fit$pred$obs)^2)) # 0.2058208 Doesn't help





##################
# Regularization
##################

#Values of lambda over which to check.
grid_lambda = 10^seq(1, -3, length = 5)
#plot(1:20, grid_lambda)
grid_alpha = seq(0, 1, length=6)

# Cross-Validation for alpha & lambda
set.seed(1000)
train.control = trainControl(method = 'repeatedcv', number=10, repeats=5, verboseIter = TRUE)
tune.grid = expand.grid(lambda = grid_lambda, alpha=grid_alpha)

# Test interactions with xyz library (to find best 2-way interactions in dataset)
# library("xyz")
# result <- xyz_regression(data.matrix(encoded.private.train[ , -which(names(encoded.private.train) == "SalePrice")]),
#                          as.vector((encoded.private.train$SalePrice^bc.lambda - 1)/bc.lambda),
#                          lambdas = 10^seq(1, -3, length = 10), n_lambda = 10,
#                          alpha = 0.1, L = 10,
#                          standardize = TRUE,
#                          standardize_response = TRUE)
# f <- as.formula( ~ .*.) 
# x <- model.matrix(f, encoded.private.train[ , -which(names(encoded.private.train) == "SalePrice")])[, -1] 

# end test variations


x <- encoded.private.train %>% select(-SalePrice)
y <- (encoded.private.train$SalePrice^bc.lambda - 1)/bc.lambda

glmnet.caret = train(x, y, method = 'glmnet',
                     trControl = train.control, 
                     tuneGrid = tune.grid)

alpha <- glmnet.caret$bestTune$alpha
lambda <- glmnet.caret$bestTune$lambda

tuned.linear.pred = predict(glmnet.caret, 
                            newdata = encoded.private.test[ , -which(names(encoded.private.test) == "SalePrice")])

# RMSE
sqrt(mean((log(unbox(tuned.linear.pred, bc.lambda)) - log(encoded.private.test$SalePrice))^2))

# Retraining on full training set

x = model.matrix(SalePrice ~ ., encoded.houses.train)
y = (encoded.houses.train$SalePrice^bc.lambda - 1)/bc.lambda
z = model.matrix(SalePrice ~ ., encoded.houses.test)

glmnet.caret.full.training = glmnet(x, y, 
                                    alpha = alpha, 
                                    lambda = lambda)

linear.pred.full.training = predict(glmnet.caret.full.training, 
                                    newx = z)

# Create submission file
write.csv(data.frame(Id = 1461:2919, SalePrice = unbox(linear.pred.full.training[,1], bc.lambda)), 
          paste(format(Sys.time(),'%Y-%m-%d %H-%M-%S'), "house_submission.csv"), 
          row.names = FALSE)
