# Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(vtreat)

# Load Helper Functions
source("Helpers.R")

# Load Data
load("./house_imputed.RData") # Loads houses.train and houses.test
houses.train <- as.data.frame(houses.train)
houses.test <- as.data.frame(houses.test)

# Take log of house price label
houses.train$SalePrice <- log(houses.train$SalePrice + 1)
houses.test$SalePrice <- log(houses.test$SalePrice + 1)

houses <- rbind(houses.train, houses.test)

##################
# Encoding
##################

# Scale numeric features
indices <- sapply(houses.train, is.numeric)
indices["SalePrice"] <- FALSE # don't scale the labels
houses.train[indices]  <- data.frame(lapply(houses.train[indices], scale))

indices <- sapply(houses, is.numeric)
indices["SalePrice"] <- FALSE # don't scale the labels
houses[indices]  <- data.frame(lapply(houses[indices], scale))



# One-hot encoding of categorical features
# using vtreat over caret::dummyVars because handles new levels in test set
set.seed(0)

vtreat_encoder_train <- vtreat::designTreatmentsZ(houses.train,
                                            colnames(houses.train),
                                            minFraction= 0,
                                            verbose=FALSE)

vtreat_encoder_full <- vtreat::designTreatmentsZ(houses, 
                                                 colnames(houses), 
                                                 minFraction= 0,
                                                 verbose=FALSE)

# One-hot Encoding with vtreat
# see vignette('vtreatVariableTypes', package = 'vtreat') for details
sf <- vtreat_encoder_train$scoreFrame
newvars <- sf$varName[sf$code %in% c("lev", "clean", "isBAD")] # TO DO check how this works
houses.train.Vtreat <- as.data.frame(vtreat::prepare(vtreat_encoder_train,
                                                     houses.train,
                                                     varRestriction = newvars))

sf <- vtreat_encoder_full$scoreFrame
newvars <- sf$varName[sf$code %in% c("lev", "clean", "isBAD")] # TO DO check how this works
houses.Vtreat <- as.data.frame(vtreat::prepare(vtreat_encoder_full, 
                                                    houses, 
                                                    varRestriction = newvars))


# Save encoded data.frame
save(houses.Vtreat, file = "./house_encoded.RData")

##################
# Run Linear Model
##################


# Split our data into a train and test set - 80/20
set.seed(0)
split_ratio = 0.8
train = sample(1:nrow(houses.train.Vtreat), nrow(houses.train.Vtreat)*split_ratio)
private.train = houses.train.Vtreat[train,]
private.test = houses.train.Vtreat[-train,]

# Baseline linear model
# model.baseline <- lm(SalePrice_clean ~ ., data=private.train)
# summary(model.baseline)

#plot(model.baseline) # review assumptions
# predicted <- predict(model.baseline, private.test, na.action = na.exclude) 
# actual <-private.test$SalePrice_clean
# 
# sqrt(mean((predicted-actual)^2))

##################
# Regularization
##################

#Values of lambda over which to check.
grid_lambda = 10^seq(4, -3, length = 20)
# plot(1:20, grid_lambda)
grid_alpha = seq(0, 1, length=21)

# Cross-Validation for alpha & lambda
set.seed(1000)
train_control = trainControl(method = 'cv', number=10)
tune.grid = expand.grid(lambda = grid_lambda, alpha=grid_alpha)
ridge.caret = train(private.train, 
                    private.train$SalePrice_clean,
                    method = 'glmnet',
                    trControl = train_control, 
                    tuneGrid = tune.grid)

alpha <- ridge.caret$bestTune$alpha
lambda <- ridge.caret$bestTune$lambda

tuned.linear.pred = predict(ridge.caret, newdata = private.test)
sqrt(mean((tuned.linear.pred - private.test$SalePrice_clean)^2))

# Retraining on full training set
x = model.matrix(SalePrice_clean ~ ., houses.Vtreat[1:1460,])
y = houses.Vtreat[1:1460,]$SalePrice_clean
z = model.matrix(SalePrice_clean~ ., houses.Vtreat[1461:2919,])

ridge.caret.full.training = glmnet(x, y, 
                                   alpha = alpha, 
                                   lambda = lambda)

linear.pred.full.training = predict(ridge.caret.full.training, 
                                    newx = z)

# Create submission file
write.csv(data.frame(Id = 1461:2919, SalePrice = exp(linear.pred.full.training[,1])), 
          paste(format(Sys.time(),'%Y-%m-%d %H-%M-%S'), "house_submission.csv"), 
          row.names = FALSE)



