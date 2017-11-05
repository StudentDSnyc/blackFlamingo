library(MASS)

model.empty = lm(SalePrice_clean ~ 1, data = private.train) #The model with an intercept ONLY.
model.full = lm(SalePrice_clean ~ ., data = private.train) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))

#Stepwise regression using AIC as the criteria (the penalty k = 2).
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)


predicted <- predict(forwardAIC, private.test)
predicted0 <- predict(model.full, private.test)

actual <- private.test$SalePrice_clean
sqrt(mean((predicted-actual)^2))

