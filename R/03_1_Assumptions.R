#untransformed SalePrice
model.baseline <- lm(SalePrice_clean ~ ., data=private.train)
summary(model.baseline)

plot(model.baseline) # review assumptions
# right skew
# non-constant variance

#Log transform
model.log <- lm(log(SalePrice_clean) ~ ., data=private.train)
summary(model.log)

plot(model.log)
# left skew
# variance ok

#Box Cox transform
bc <- boxCox(model.baseline)
lambda = bc$x[which(bc$y == max(bc$y))]
x <- private.train %>% select(-SalePrice_clean)
y <- (private.train$SalePrice_clean^lambda - 1)/lambda

model.bc <- lm(y ~ ., data=x)
summary(model.bc)
plot(model.bc)
# balanced, but fat tailed
# variance good

# compare training RMSE of log price
actual.train <- log(private.train$SalePrice_clean)

predict.train <- log(model.baseline$fitted.values)
sqrt(mean((predict.train-actual.train)^2))

predict.train.log <- model.log$fitted.values
sqrt(mean((predict.train.log-actual.train)^2))

predict.train.bc <- model.bc$fitted.values
predict.train.bc <- ((predict.train.bc * lambda) + 1)^(1/lambda)
predict.train.bc <- log(predict.train.bc)
sqrt(mean((predict.train.bc-actual.train)^2))

# cross validate these
library(caret)
set.seed(0)

RMSElog <- function(data, lev = NULL, model = NULL){
  err <- sqrt(mean((log(data[, "obs"]) - log(data[, "pred"]))^2, na.rm=TRUE))
  print(err)
  out <- c(err)
  names(out) <- c("RMSElog")
  out
}

train_control.base <- trainControl(method = 'repeatedcv', number=5,
                              repeats=10, summaryFunction = RMSElog, verboseIter = TRUE)

set.seed(8)
cv.baseline <- train(x, private.train$SalePrice_clean, method='lm', 
                     metric="RMSElog", trControl = train_control.base,
                     na.action = na.exclude)

cv.baseline

train_control.log <- trainControl(method = 'repeatedcv', number=5,
                                   repeats=10, verboseIter = TRUE)

set.seed(9)
cv.log <- train(x, log(private.train$SalePrice_clean), method='lm',
                metric="RMSE", trControl = train_control.log)

cv.log

unbox <- function(data, lambda){
  ((data * lambda) + 1)^(1/lambda)
}

RMSEbc <- function(data, lev = NULL, model = NULL){
  #print(data[,"pred"])
  fitted <- unbox(data[, "pred"], lambda)
  seen <- unbox(data[,"obs"], lambda)
  err <- sqrt(mean((log(seen) - log(fitted))^2, na.rm=FALSE))
  print(err)
  out <- c(err)
  names(out) <- c("RMSEbc")
  out
}

train_control.bc <- trainControl(method = 'repeatedcv', number=5,
                                 repeats=10, verboseIter = TRUE, summaryFunction = RMSEbc)

set.seed(10)
cv.bc <- train(x, y, method='lm', metric="RMSEbc", trControl = train_control.bc)

cv.bc

# peeking at private test -- BAD!!
predicted <- predict(model.bc, private.test, na.action = na.exclude) 
predicted <- ((predicted * lambda) + 1)^(1/lambda)
predicted <- log(predicted)
actual <-private.test$SalePrice_clean
actual <- log(actual)

sqrt(mean((predicted-actual)^2))