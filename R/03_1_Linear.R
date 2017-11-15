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
# source("./03_0_SplitEncode.R")
load("./data/Xprivate.train.RData")
load("./data/Xprivate.test.RData")
load("./data/encoded.private.train.RData")
load("./data/encoded.private.test.RData")
load("./data/encoded.houses.train.RData")
load("./data/encoded.houses.test.RData")

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

# Plot actual vs predicted
plot(actual, predicted)
text(actual, predicted, labels=rownames(private.test), cex= 0.7, pos=3)
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

# Splines
set.seed(123)
require(splines)
train.control <- trainControl("cv", 10, savePred=T)
fit <- train(log(SalePrice) ~ .-GrLivArea_clean +bs(GrLivArea_clean, df=6) 
             -YearBuilt_clean +bs(YearBuilt_clean, df=6), 
             data=encoded.houses.train,
             method="glm",
             trControl=train.control)

# show predictions from CV
head(fit$pred)
# RMSE
sqrt(mean((fit$pred$pred - fit$pred$obs)^2)) # 0.126197


# Plot spline? ISLR p.289
# par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))
# plot(age,wage,xlim=agelims ,cex=.5,col="darkgrey")
# title("Plot",outer=T)
# x_axis=seq(from=0,to=range(encoded.houses.train$GrLivArea_clean)[1])
# lines(splines.cv,preds$fit,lwd=2,col="blue")
# # > matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# Plot spline?
plot(encoded.houses.train, splines.cv)
text(encoded.houses.train, splines.cv, labels=rownames(splines.cv), cex= 0.7, pos=3)
lines(encoded.houses.train,splines.cv$fit,lwd=2,col="blue") # plot splines
abline(0, 1)

# Spline CV
set.seed(1331)
folds <- createFolds(rownames(encoded.houses.train), k=5)
splines.predictions <- list(K1=c(), K2=c(), K3=c(), K4=c(), K5=c())

# K1
spline.fit <- glm(log(SalePrice) ~ .-GrLivArea_clean +bs(GrLivArea_clean, df=6), # df=6
                  data=encoded.houses.train[-folds$Fold1, ])
splines.predictions$K1 <- predict(spline.fit, 
                                  newdata = encoded.houses.train[folds$Fold1, -which(names(encoded.houses.train) == "SalePrice")])
# RMSE for the fold
sqrt(mean((splines.predictions$K1 - log(encoded.houses.train[folds$Fold1,c("SalePrice")]))^2))


# K2
spline.fit <- glm(log(SalePrice) ~ .-GrLivArea_clean +bs(GrLivArea_clean, df=6), # df=6
                  data=encoded.houses.train[-folds$Fold2, ])
splines.predictions$K2 <- predict(spline.fit, 
                                  newdata = encoded.houses.train[folds$Fold2, -which(names(encoded.houses.train) == "SalePrice")])
# RMSE for the fold
sqrt(mean((splines.predictions$K2 - log(encoded.houses.train[folds$Fold2,c("SalePrice")]))^2))

# K3 
spline.fit <- glm(log(SalePrice) ~ .-GrLivArea_clean +bs(GrLivArea_clean, df=6), # df=6
                  data=encoded.houses.train[-folds$Fold3, ])
splines.predictions$K3 <- predict(spline.fit, 
                                  newdata = encoded.houses.train[folds$Fold3, -which(names(encoded.houses.train) == "SalePrice")])
# RMSE for the fold
sqrt(mean((splines.predictions$K3 - log(encoded.houses.train[folds$Fold3,c("SalePrice")]))^2))


# K4
spline.fit <- glm(log(SalePrice) ~ .-GrLivArea_clean +bs(GrLivArea_clean, df=6), # df=6
                  data=encoded.houses.train[-folds$Fold4, ])
splines.predictions$K4 <- predict(spline.fit, 
                                  newdata = encoded.houses.train[folds$Fold4, -which(names(encoded.houses.train) == "SalePrice")])
# RMSE for the fold
sqrt(mean((splines.predictions$K4 - log(encoded.houses.train[folds$Fold4,c("SalePrice")]))^2))

# K5
spline.fit <- glm(log(SalePrice) ~ .-GrLivArea_clean +bs(GrLivArea_clean, df=6), # df=6
                    data=encoded.houses.train[-folds$Fold5, ])
splines.predictions$K5 <- predict(spline.fit, 
                                    newdata = encoded.houses.train[folds$Fold5, -which(names(encoded.houses.train) == "SalePrice")])
# RMSE for the fold
sqrt(mean((splines.predictions$K5 - log(encoded.houses.train[folds$Fold5,c("SalePrice")]))^2))

# Prediction CV
splines.cv <- c(splines.predictions$K1, 
                splines.predictions$K2, 
                splines.predictions$K3, 
                splines.predictions$K4, 
                splines.predictions$K5)

splines.cv <- splines.cv[order(c(folds$Fold1, folds$Fold2, folds$Fold3, folds$Fold4, folds$Fold5))]

# Total RMSE
sqrt(mean((splines.cv - log(encoded.houses.train[,c("SalePrice")]))^2))



write.csv(data.frame(Id = 1:1460, splines_features = exp(splines.cv)), 
          paste(format(Sys.time(),'%Y-%m-%d %H-%M-%S'), "splines_features.csv"), 
          row.names = FALSE)

# Get houses.test prediction

spline.fit <- glm(log(SalePrice) ~ .-GrLivArea_clean +bs(GrLivArea_clean, df=6), # df=6
                  data=encoded.houses.train)
splines.predictions.test <- predict(spline.fit, 
                                  newdata = encoded.houses.test)

write.csv(data.frame(Id = 1:1459, splines_predictions = exp(splines.predictions.test)), 
          paste(format(Sys.time(),'%Y-%m-%d %H-%M-%S'), "splines_predictions.csv"), 
          row.names = FALSE)

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

# Create submission file
write.csv(data.frame(Id = 1461:2919, SalePrice = unbox(linear.pred.full.training[,1], bc.lambda)), 
          paste(format(Sys.time(),'%Y-%m-%d %H-%M-%S'), "house_submission.csv"), 
          row.names = FALSE)


# 
# #######################
# # Assumptions
# #######################
# 
# #untransformed SalePrice
# model.baseline <- lm(SalePrice ~ ., data=private.train)
# summary(model.baseline)
# 
# # plot(model.baseline) # review assumptions
# # right skew
# # non-constant variance
# 
# #Log transform
# model.log <- lm(log(SalePrice) ~ ., data=private.train)
# summary(model.log)
# 
# # plot(model.log)
# # left skew
# # variance ok
# 
# #Box Cox transform
# bc <- boxCox(model.baseline)
# lambda = bc$x[which(bc$y == max(bc$y))]
# x <- private.train %>% select(-SalePrice)
# y <- (private.train$SalePrice^lambda - 1)/lambda
# 
# model.bc <- lm(y ~ ., data=x)
# summary(model.bc)
# # plot(model.bc)
# # balanced, but fat tailed
# # variance good
# 
# # compare training RMSE of log price
# actual.train <- log(private.train$SalePrice)
# 
# predict.train <- log(model.baseline$fitted.values)
# sqrt(mean((predict.train-actual.train)^2))
# 
# predict.train.log <- model.log$fitted.values
# sqrt(mean((predict.train.log-actual.train)^2))
# 
# predict.train.bc <- model.bc$fitted.values
# predict.train.bc <- ((predict.train.bc * lambda) + 1)^(1/lambda)
# predict.train.bc <- log(predict.train.bc)
# sqrt(mean((predict.train.bc-actual.train)^2))
# 
# # cross validate these
# library(caret)
# set.seed(0)
# 
# RMSElog <- function(data, lev = NULL, model = NULL){
#   err <- sqrt(mean((log(data[, "obs"]) - log(data[, "pred"]))^2, na.rm=TRUE))
#   print(err)
#   out <- c(err)
#   names(out) <- c("RMSElog")
#   out
# }
# 
# train_control.base <- trainControl(method = 'repeatedcv', number=5,
#                               repeats=10, summaryFunction = RMSElog, verboseIter = TRUE)
# 
# set.seed(8)
# cv.baseline <- train(x, private.train$SalePrice_clean, method='lm', 
#                      metric="RMSElog", trControl = train_control.base,
#                      na.action = na.exclude)
# 
# cv.baseline
# 
# train_control.log <- trainControl(method = 'repeatedcv', number=5,
#                                    repeats=10, verboseIter = TRUE)
# 
# set.seed(9)
# cv.log <- train(x, log(private.train$SalePrice_clean), method='lm',
#                 metric="RMSE", trControl = train_control.log)
# 
# cv.log
# 
# unbox <- function(data, lambda){
#   ((data * lambda) + 1)^(1/lambda)
# }
# 
# RMSEbc <- function(data, lev = NULL, model = NULL){
#   #print(data[,"pred"])
#   fitted <- unbox(data[, "pred"], lambda)
#   seen <- unbox(data[,"obs"], lambda)
#   err <- sqrt(mean((log(seen) - log(fitted))^2, na.rm=FALSE))
#   print(err)
#   out <- c(err)
#   names(out) <- c("RMSEbc")
#   out
# }
# 
# train_control.bc <- trainControl(method = 'repeatedcv', number=5,
#                                  repeats=10, verboseIter = TRUE, summaryFunction = RMSEbc)
# 
# set.seed(10)
# cv.bc <- train(x, y, method='lm', metric="RMSEbc", trControl = train_control.bc)
# 
# cv.bc
# 
# # peeking at private test -- BAD!!
# predicted <- predict(model.bc, private.test, na.action = na.exclude) 
# predicted <- ((predicted * lambda) + 1)^(1/lambda)
# predicted <- log(predicted)
# actual <-private.test$SalePrice_clean
# actual <- log(actual)
# 
# sqrt(mean((predicted-actual)^2))