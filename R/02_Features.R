library(devtools)
#install_github("gathanei/xyz")
library("xyz")


##################
# Load
##################
library(data.table)

load("./data/house_imputed.RData") 
class(houses.train) # "data.table" "data.frame"
class(houses.test) # "data.table" "data.frame"

houses = rbind(houses.train, houses.test) # need to only do feature engineering that doesn't leak information

# Check for constant features
houses[,sapply(houses, function(x) length(unique(x))) <= 1]

# Find features with low variances
colnames(houses)[caret::nearZeroVar(houses, saveMetrics = F)]
# Drop features - commenting because worst results
# houses <-houses[, -which(names(houses) %in% c("Utilities", "RoofMatl")), with=FALSE]



# Living Area http://homeguides.sfgate.com/calculate-living-area-square-footage-33755.html 
houses[, c("Adj.GrLivArea") := list(BsmtFinSF1+BsmtFinSF2+GrLivArea)]

# Number of bathrooms. Only count basement bathrooms if there's a 'finished' basement area
houses[BsmtFinSF1>0, c("TotalBath") := list(FullBath + .5*HalfBath + BsmtFullBath + .5*BsmtHalfBath)]
houses[BsmtFinSF1==00, TotalBath := list(FullBath + .5*HalfBath)]

# Interactions # Prints interaction but not sure how to create the features we need or get model using them
# result <- xyz_regression(data.matrix(houses[1:1460, -which(names(houses) == "SalePrice"), with=FALSE]),
#                houses[1:1460, ][["SalePrice"]], # using list subsetting '[[' to access data.table column (fast)
#                lambdas = 10^seq(1, -3, length = 10), n_lambda = 10,
#                alpha = 0.1, L = 10,
#                standardize = TRUE,
#                standardize_response = TRUE)

# Interaction effect:
# Interaction effect: (26,72) coefficient: 0.2037225 # MasVnrArea PoolQC
# Interaction effect: (14,28) coefficient: -0.1285461 # Condition2 ExterCond    # ok maybe
# Interaction effect: (14,38) coefficient: -0.1203811 # Condition2 TotalBsmtSF ??
# Interaction effect: (14,67) coefficient: -0.1122927 # Condition2 OpenPorchSF
# Interaction effect: (4,8) coefficient: -0.1066311 # LotArea LandContour       # ok makes sense
# Interaction effect: (38,72) coefficient: 0.08450363 # TotalBsmtSF PoolQC

# Interaction effect: (14,14) coefficient: -0.08428927
# Interaction effect: (30,72) coefficient: 0.07212801
# Interaction effect: (4,4) coefficient: -0.0693587
# Interaction effect: (3,26) coefficient: 0.06451609
# Interaction effect: (3,43) coefficient: -0.06385154

# Variable for MasVnrArea/ PoolQC effect? doesn't make sense (only 3 rows applicable)
# houses[, c("PoolMasVnrArea.interaction") := list(with(houses, interaction(quantile(houses$MasVnrArea, probs = seq(0, 1, 0.05)),  PoolQC)))]

houses[, c("Condition2.ExterCond.interaction") := list(with(houses, interaction(Condition2,  ExterCond)))]
houses[, c("LotArea.LandContour.interaction") := list(with(houses, interaction(quantile(houses$LotArea, probs = seq(0, 1, 0.05)),  LandContour)))]

# Put SalePrice back in the last position
setcolorder(houses, c(setdiff(names(houses), "SalePrice"), "SalePrice"))

# Split back
houses.train <- houses[1:1460,]
houses.test <- houses[1461:2919,]

##################
# Save
##################

# to .RData
save(houses.train, file = "./data/houses.train.RData")
save(houses.test, file = "./data/houses.test.RData")

# to .csv
fwrite(houses.train, file = "../Data/features.houses.train.csv", quote=F, row.names=T)
fwrite(houses.test, file = "../Data/features.houses.test.csv", quote=F, row.names=T)

