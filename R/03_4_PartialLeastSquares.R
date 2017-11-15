library(MASS)
library(car)
library(caret)

load("./data/encoded.private.train.RData")
load("./data/encoded.private.test.RData")
load("./data/private.test.RData")

source('./VIP.R')
## for calculating variable importance

model.baseline <- lm(SalePrice ~ ., data=encoded.private.train)
bc <- boxCox(model.baseline)
bc.lambda = bc$x[which(bc$y == max(bc$y))]

x <- subset(encoded.private.train, select=-c(SalePrice))
y <- (encoded.private.train$SalePrice^bc.lambda - 1)/bc.lambda

set.seed(100)

unbox <- function(data, lambda){
  ((data * lambda) + 1)^(1/lambda)
}

RMSEbc <- function(data, lev = NULL, model = NULL){
  #print(data[,"pred"])
  fitted <- unbox(data[, "pred"], bc.lambda)
  seen <- unbox(data[,"obs"], bc.lambda)
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

#ncomp=3

pred = predict(model.pls, newdata = x)
pred = log(unbox(pred, bc.lambda))
actual <- log(encoded.private.train$SalePrice)

sqrt(mean((pred-actual)^2))

pred = predict(model.pls, newdata = encoded.private.test)
pred = log(unbox(pred, bc.lambda))
actual <- log(private.test$SalePrice)

sqrt(mean((pred-actual)^2))

### Inspect components
features <- model.pls$finalModel$xNames
importances <- sapply(1:length(features), function(x){VIPjh(model.pls$finalModel, x, model.pls$finalModel$ncomp)})

importances <- data.frame(features=features, importance=importances)

library(dplyr)
top100 <- head(importances[order(-importances$importance),], 100)
top100$features <- gsub("_.*", "", top100$features)
top100 <- top100 %>% group_by(features) %>% summarize(importance = sqrt(sum(importance^2))) %>% arrange(desc(importance))

#write.csv(top100, "PLSR-results.csv", row.names = FALSE)
