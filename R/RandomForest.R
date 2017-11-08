library(randomForest)


source("03_0_Baseline.R")



#Fitting an initial random forest to the house training data.
set.seed(0)
rf.houses= randomForest(SalePrice ~ ., data = private.train, importance = TRUE)
rf.houses


#find optimal mtry attributes for each split
tuneRF(private.train[,1:79], private.train[,80], ntreeTry = 500)

#fit random forest for optimal mtry= 26
rf.houses= randomForest(log(SalePrice) ~., 
                        data=private.train, 
                        mtry=26, 
                        importance= TRUE,
                        nodesize=3,
                        maxnodes=3,
                        ntree=1000)
min(rf.houses$mse)



importance(rf.houses)
varImpPlot(rf.houses)


#predict on test data

rf.houses.predict = predict(rf.houses, private.test)
rf.houses.predict

rf.houses.test= as.vector(private.test[,"SalePrice"])



plot(rf.houses.predict, rf.houses.test)
test_mse=sqrt(mean((rf.houses.predict - rf.houses.test)^2))
test_mse





