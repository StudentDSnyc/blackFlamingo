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

source('./02_Features.R')
load("./data/Xhouses.train.RData")
load("./data/Xhouses.test.RData")
dim(houses.train) # "data.table" "data.frame"
dim(houses.test) # "data.table" "data.frame"

#########################
# Create private test set
#########################

# Split our data into a train and test set - 80/20
set.seed(0)
split.ratio = 0.8
train.indices = sample(1:nrow(houses.train), nrow(houses.train)*split.ratio)
private.train = houses.train[train.indices,] # dim: 1168, 80 + engineered
private.test = houses.train[-train.indices,] # dim: 292, 80 + engineered

# Convert to data.frame (from data.table) 
private.train <- as.data.frame(private.train)
private.test <- as.data.frame(private.test)
houses.train <- as.data.frame(houses.train)
houses.test <- as.data.frame(houses.test)

# Save unencoded dataframes
save(private.train, file = "./data/Xprivate.train.RData")
save(private.test, file = "./data/Xprivate.test.RData")

# Save unencoded dataframes as csvs
write.csv(private.train, "Xprivate_train.csv", row.names = FALSE)
write.csv(private.test, "Xprivate_test.csv", row.names = FALSE)


##################
# Label Count Encoding - No, just puts number of occurences right now
##################
# # Label Count encoding = Replaces labels with the number of occurences (for categorical columns)
# encoded.private.train <- label.count.encode.df(private.train)
# encoded.private.test <- label.count.encode.df(private.test)
# encoded.houses.train <- label.count.encode.df(houses.train)
# encoded.houses.test <- label.count.encode.df(houses.test)
# 

##################
# One hot Encoding
##################

# One-hot encode categorical features using vtreat
# Scale all features including dummy ones per: https://stats.stackexchange.com/questions/69568/whether-to-rescale-indicator-binary-dummy-predictors-for-lasso

# Using vtreat (with data.frame)
##############

# Create encoder 
encoder <- vtreat::designTreatmentsN(dframe = private.train, # theoretically should use separate data to encode
                                     varlist = colnames(private.train),
                                     outcomename = "SalePrice",
                                     rareCount=10,
                                     rareSig=1,
                                     verbose=TRUE) 
# Now encode both train and test
Xencoded.private.train <- vtreat::prepare(encoder,
                                         private.train,
                                         pruneSig=1,
                                         scale = FALSE)

Xencoded.private.test <- vtreat::prepare(encoder,
                                         private.test,
                                         pruneSig=1,
                                         scale = FALSE)

# Same for houses.train/ houses.test
encoder <- vtreat::designTreatmentsN(dframe = houses.train, # theoretically should use separate data to encode
                                     varlist = colnames(houses.train),
                                     outcomename = "SalePrice",
                                     rareCount=10,
                                     rareSig=1,
                                     verbose=TRUE) 
# Now encode both train and test
Xencoded.houses.train <- vtreat::prepare(encoder,
                                        houses.train,
                                         pruneSig=1,
                                         scale = TRUE)

Xencoded.houses.test <- vtreat::prepare(encoder,
                                        houses.test,
                                        pruneSig=1,
                                        scale = TRUE)
# Scale all columns except SalePrice
# Scaling test sets should really be done with means from training set
# encoded.private.train[ , -which(names(encoded.private.train) == "SalePrice")] <- scale(encoded.private.train[ , -which(names(encoded.private.train) == "SalePrice")])
# encoded.private.test[ , -which(names(encoded.private.test) == "SalePrice")] <- scale(encoded.private.test[ , -which(names(encoded.private.test) == "SalePrice")])
# encoded.houses.train[ , -which(names(encoded.houses.train) == "SalePrice")] <- scale(encoded.houses.train[ , -which(names(encoded.houses.train) == "SalePrice")])
# encoded.houses.test[ , -which(names(encoded.houses.test) == "SalePrice")] <- scale(encoded.houses.test[ , -which(names(encoded.houses.test) == "SalePrice")])

# Scaling using scale & center from training sets
Xencoded.private.train <- scale(Xencoded.private.train, center = FALSE)
Xencoded.private.test <- scale(Xencoded.private.test, center = FALSE, attr(Xencoded.private.train, "scaled:scale"))
Xencoded.houses.train <- scale(Xencoded.houses.train, center = FALSE)
Xencoded.houses.test <- scale(Xencoded.houses.test, center = FALSE, attr(Xencoded.houses.train, "scaled:scale"))

# Re-copy orignal y values (not scaled)
Xencoded.private.train[, c('SalePrice')] <- private.train$SalePrice
Xencoded.private.test[, c('SalePrice')] <- private.test$SalePrice
Xencoded.houses.train[, c('SalePrice')] <- houses.train$SalePrice
Xencoded.houses.test[, c('SalePrice')] <- 0

# Convert to data.frame (from data.table) 
Xencoded.private.train <- as.data.frame(Xencoded.private.train)
Xencoded.private.test <- as.data.frame(Xencoded.private.test)
Xencoded.houses.train <- as.data.frame(Xencoded.houses.train)
Xencoded.houses.test <- as.data.frame(Xencoded.houses.test)

# Using model.matrix (with data.table)
####################

# encoded.private.train <- as.data.table(stats::model.matrix(~., data=private.train[ , -c("SalePrice")])[,-1])
# cols <- colnames(encoded.private.train)
# encoded.private.train[, (cols) := lapply(.SD, scale), .SDcols=cols]
# encoded.private.train[, c('SalePrice')] <- private.train$SalePrice
# 
# encoded.private.test <- as.data.table(stats::model.matrix(~., data=private.test[ , -c("SalePrice")])[,-1])
# cols <- colnames(encoded.private.test)
# encoded.private.test[, (cols) := lapply(.SD, scale), .SDcols=cols]
# encoded.private.test[, c('SalePrice')] <- private.test$SalePrice
# 
# # Zeroing NA columns
# encoded.private.test[, c('StreetPave')] <- 0
# encoded.private.test[, c('NeighborhoodBlueste')] <- 0
# encoded.private.test[, c('NeighborhoodVeenker')] <- 0
# encoded.private.test[, c('ExterCondPo')] <- 0
# encoded.private.test[, c('BsmtQualNA')] <- 0
# encoded.private.test[, c('BsmtCondNA')] <- 0
# encoded.private.test[, c('BsmtExposureNA')] <- 0
# encoded.private.test[, c('BsmtFinType1NA')] <- 0
# encoded.private.test[, c('BsmtFinType2NA')] <- 0
# encoded.private.test[, c('FoundationSlab')] <- 0
# encoded.private.test[, c('HeatingQC1')] <- 0
# 
# encoded.houses.train <- as.data.table(stats::model.matrix(~., data=houses.train[ , -c("SalePrice")])[,-1])
# cols <- colnames(encoded.houses.train)
# encoded.houses.train[, (cols) := lapply(.SD, scale), .SDcols=cols]
# encoded.houses.train[, c('SalePrice')] <- houses.train$SalePrice
# 
# encoded.houses.test <- as.data.table(stats::model.matrix(~., data=houses.test[ , -c("SalePrice")])[,-1])
# cols <- colnames(encoded.houses.test)
# encoded.houses.test[, (cols) := lapply(.SD, scale), .SDcols=cols]
# encoded.houses.test[, c('SalePrice')] <- 0



# Add missing columns in test set with default value equal to 0
# This approach was chosen over binding training and test because it would leak information from test
# Also remove columns from test not in training (no predictive value)

# encoded.private.test <- align.columns(encoded.private.train, encoded.private.test)
# encoded.houses.test <- align.columns(encoded.houses.train, encoded.houses.test)

# Cut any linear combinations or duplicate columns generated by dummify
lincomb <- findLinearCombos(Xencoded.private.train)
keep = lincomb$remove[-127][-86]
remove = c(keep, 221, 222, 223)
#lapply(lincomb$linearCombos, function(x) colnames(encode.private.train)[x])
Xencoded.private.train <- Xencoded.private.train %>% select(-remove)
Xencoded.private.test <- Xencoded.private.test %>% select(-remove)

lincomb <- findLinearCombos(Xencoded.houses.train)
Xencoded.houses.train <- Xencoded.houses.train %>% select(-remove)
Xencoded.houses.test <- Xencoded.houses.test %>% select(-remove)


# Save encoded dataframes
save(Xencoded.private.train, file = "./data/Xencoded.private.train.RData")
save(Xencoded.private.test, file = "./data/Xencoded.private.test.RData")
save(Xencoded.houses.train, file = "./data/Xencoded.houses.train.RData")
save(Xencoded.houses.test, file = "./data/Xencoded.houses.test.RData")

#Save encoded dataframes as csv:
write.csv(Xencoded.private.train, "../Data/Xencoded_private_train.csv", row.names = FALSE)
write.csv(Xencoded.private.test, "../Data/Xencoded_private_test.csv", row.names = FALSE)
write.csv(Xencoded.houses.train, "../Data/Xencoded_houses_train.csv", row.names = FALSE)
write.csv(Xencoded.houses.test, "../Data/Xencoded_houses_test.csv", row.names = FALSE)





