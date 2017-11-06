##################
# Load
##################
library(data.table)

load("./data/house_imputed.RData") 
class(houses.train) # "data.table" "data.frame"
class(houses.test) # "data.table" "data.frame"

houses = rbind(houses.train, houses.test) # need to only do feature engineering that doesn't leak information

# Living Area http://homeguides.sfgate.com/calculate-living-area-square-footage-33755.html 
houses[, c("Adj.GrLivArea") := list(BsmtFinSF1+BsmtFinSF2+GrLivArea)]

# Number of bathrooms. Only count basement bathrooms if there's a 'finished' basement area
houses[BsmtFinSF1>0, c("TotalBath") := list(FullBath + .5*HalfBath + BsmtFullBath + .5*BsmtHalfBath)]
houses[BsmtFinSF1==00, TotalBath := list(FullBath + .5*HalfBath)]

# Put SalePrice back in the last position
setcolorder(houses, c(setdiff(names(houses), "SalePrice"), "SalePrice"))

# Split back
houses.train <- houses[1:1460,]
houses.test <- houses[1461:2919,]

##################
# Save
##################
save(houses.train, file = "./data/houses.train.RData")
save(houses.test, file = "./data/houses.test.RData")
