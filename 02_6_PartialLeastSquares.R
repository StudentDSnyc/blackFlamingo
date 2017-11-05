library(MASS)
library(car)
library(caret)

model.baseline <- lm(SalePrice_clean ~ ., data=private.train)
bc <- boxCox(model.baseline)
lambda = bc$x[which(bc$y == max(bc$y))]

x <- subset(private.train, select=-c(SalePrice_clean))
y <- (private.train$SalePrice_clean^lambda - 1)/lambda

set.seed(100)

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

train_control <- trainControl(method = 'repeatedcv', number=5,
                                   repeats=10, verboseIter = TRUE, summaryFunction = RMSEbc)

model.pls <- train(x, y, method = "pls", tuneLength = 200,
              metric="RMSEbc", maximize=FALSE,trControl = train_control)

#ncomp=10

pred = predict(model.pls, newdata = x)
pred = log(unbox(pred, lambda))
actual <- log(private.train$SalePrice_clean)

sqrt(mean((pred-actual)^2))

pred = predict(model.pls, newdata = private.test)
pred = log(unbox(pred, lambda))
actual <- log(private.test$SalePrice_clean)

sqrt(mean((pred-actual)^2))
