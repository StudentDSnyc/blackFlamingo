library(xgboost)
library(Metrics)

source("03_0_Baseline.R")


# xgboost fitting with arbitrary parameters
xgb_params_1 = list(
  objective = "reg:linear", #objective function
  eta= 0.05,    # learning rate
  max.depth = 3,
  eval_metric = "rmse" 
)

# fit the model with the arbitrary parameters specified above
xgb.fit = xgboost(data = as.matrix(encoded.private.train[, -which(names(encoded.private.train) == "SalePrice")]),
                label = log(encoded.private.train$SalePrice),
                params = xgb_params_1,
                nrounds = 100,          # max number of trees to build
                verbose = TRUE,
                prediction= TRUE,
                print.every.n = 1,
                early.stop.round = 10   # stop if no improvement within 10 trees
)

xgb_test_baseline <- predict(xgb.fit, newdata = as.matrix(encoded.private.test[ , -which(names(encoded.private.test) == "SalePrice")]))
rmse(xgb_test_baseline - log(private.test$SalePrice))

#rmse test: 0.1729



#set up the cross-validated hyper-parameter search
xgb_grid_cv= expand.grid(
    nrounds = 3000,
    eta =  0.01,               #default=0.3
    max_depth = c(4,6),
    gamma = c(0,0.2,0.4),                 #default=1
    colsample_bytree = 0.6,    #default=1
    min_child_weight = c(0.5,1),
    subsample= 0.4
  )

# function to train the model for each parameter combination in the grid, 
# using CV to evaluate 
xgboost_train = function(df, grid){
  
  xgbTrControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE,
    returnData = FALSE,
    allowParallel = TRUE
  )
  
xgb_train_cv = train(
  x = as.matrix(df[, ! colnames(df) %in% c("SalePrice")]),
  y = log(df$SalePrice),
  trControl = xgbTrControl,
  tuneGrid = grid,
  nthreads= 8,
  method = "xgbTree",
  silent =0

)


return(xgb_train_cv)
}

#train model with parameter combination in grid
set.seed(0)
xgb_train=xgboost_train(encoded.private.train, xgb_grid_cv)



# get the top model and its results
top_result=head(xgb_train$results[with(xgb_train$results, 
                           order(RMSE)), ], 1)
top_result

#Best result
#Fitting nrounds = 3000, max_depth = 4, eta = 0.01, gamma = 0, 
#colsample_bytree = 0.6, min_child_weight = 0.5, subsample = 0.4 on full training set
#rmse train= 0.1217 

#predict using top model on private test data
xgb_test_1 <- predict(xgb_train, newdata = as.matrix(encoded.private.test[ , -which(names(encoded.houses.test) == "SalePrice")]))

rmse(xgb_test_1, log(private.test$SalePrice))

#rmse test= 0.12827




#best hyperparameters
xgb_grid_tuned=expand.grid(
  nrounds = 3000,
  eta =  0.01,              
  max_depth = 4,
  gamma = 0,                 
  colsample_bytree = 0.6,    
  min_child_weight = 0.5,
  subsample= 0.4
)


#fit to full house training set
xgb_train_tuned = train(
  x = as.matrix(encoded.houses.train[, ! colnames(encoded.houses.train) %in% c("SalePrice")]),
  y = log(encoded.houses.train$SalePrice),
  tuneGrid= xgb_grid_tuned,
  nthreads= 8,
  method = "xgbTree"
)

xgb_train_tuned
#rmse train=0.1242


#Make prediction on official test set and save predicted SalePrice values in CSV
xgb_test_official<- predict(xgb_train_tuned, newdata = as.matrix(encoded.houses.test[ , -which(names(encoded.houses.test) == "SalePrice")]))

write.csv(data.frame(Id = 1461:2919, SalePrice = exp(xgb_test_official)), 
          paste(format(Sys.time(),'%Y-%m-%d %H-%M-%S'), "house_submission.csv"), 
          row.names = FALSE)


# Variable Importance
names <- names(encoded.houses.train)[! names(encoded.houses.train) %in% c("SalePrice")]
importanceMatrix <- xgb.importance(names, 
                                   model = xgb_train_tuned$finalModel)
xgb.plot.importance(importanceMatrix[1:10,])

