library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(vtreat)
library(car)
library(DAAG)


# Load Helper Functions
source("Helpers.R")

##################
# Load
##################
# source("./03_0_SplitEncode.R")
load("./data/Xprivate.train.RData")
load("./data/Xprivate.test.RData")
load("./data/Xencoded.private.train.RData")
load("./data/Xencoded.private.test.RData")
load("./data/Xencoded.houses.train.RData")
load("./data/Xencoded.houses.test.RData")

#######################
# Baseline Linear Model
#######################

# Baseline linear model
model.baseline <- lm(SalePrice ~ ., data=Xencoded.private.train)
summary(model.baseline)


bc <- boxCox(model.baseline)
bc.lambda = bc$x[which(bc$y == max(bc$y))]

x <- Xencoded.private.train %>% select(-SalePrice)
y <- (Xencoded.private.train$SalePrice^bc.lambda - 1)/bc.lambda

model.bc <- lm(y ~ . , data=x)
model.bc.influence=influence.measures(model.bc)


predicted <- predict(model.bc, Xencoded.private.test, na.action = na.exclude) 
predicted <- log(unbox(predicted, bc.lambda))
actual <-log(private.test$SalePrice)

plot(cookd(model.bc))
plot(predicted, actual)
plot(model.bc)
influencePlot(model.bc)


#remove influential obs
#influential_obs <- which(apply(model.bc.influence$is.inf, 1, any)) 
Xencoded.private.train.influence= Xencoded.private.train[- c(436,1100,1078),]
x.influence <- Xencoded.private.train.influence %>% select(-SalePrice)
y.influence <- (Xencoded.private.train.influence$SalePrice^bc.lambda - 1)/bc.lambda


#model.bc2= lm(y.influence~ . , data=x.influence)


train.control = trainControl(method = 'cv', number=10, verboseIter = TRUE)


model.bc2 = train(x.influence, y.influence, method = 'lm',
                  trControl = train.control)

model.bc2

plot(model.bc2)

influencePlot(model.bc2)


predicted <- predict(model.bc2, Xencoded.private.test, na.action = na.exclude) 
predicted <- log(unbox(predicted, bc.lambda))
actual <-log(private.test$SalePrice)



# RMSE
sqrt(mean((predicted-actual)^2))
