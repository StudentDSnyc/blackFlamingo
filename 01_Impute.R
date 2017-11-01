# Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(Amelia)

# Load Helper Functions
source("Helpers.R")

# Load Data
load("./house_loaded.RData")

# Check missing values
houses.test$SalePrice <- 0
NA.analysis(rbind(houses.train, houses.test))

# Columns to impute with mode
to.impute <- c("MasVnrType", "MasVnrArea", "Utilities", "BsmtFullBath",
                   "BsmtHalfBath", "Functional", "BsmtFinSF1", "BsmtFinSF2",
                   "BsmtUnfSF", "TotalBsmtSF", "Electrical")

# Impute mode on train columns
for (column in to.impute){
  houses.train[[column]] <- impute.mode(houses.train, column) 
}

# Impute mode on test columns
for (column in to.impute){
  houses.test[[column]] <- impute.mode(houses.test, column) 
}


# Impute mode on column based on another column:
    #df is a dataframe
    #x is a vector of column names for imputation
    #y is a column name that the imputations in x will be based on (must be categorical)
    #type is the type of imputation method consistent with impute() in Hmisc library

ImputeDependent = function(df, x, y, type = mode) {
  require(Hmisc)
  for (column in x){
    #print(column)
    for (level in levels(df[[y]])){
      #print(level)
      missing_level = df[df[[y]] == level, c(column)]
      #print(missing_level)
      imputed_level <- impute(missing_level, type)
      #print(imputed_level)
      df[df[[y]] == level, c(column)] = imputed_level
    }
  }
  return(df)
}

# Call ImputeDependent on x = "MSZoning" and y = "Neighborhood"
ImputeDependent(house.test, "MSZoning", "Neighborhood", type = mode)

# Call ImputeDependent on x = "Exterior1st" and y = "Exterior2nd" and
# vice versa
ImputeDependent(house.test, "Exterior1st", "Exterior2nd", type = mode)
ImputeDependent(house.test, "Exterior2nd", "Exterior1st", type = mode)

# Impute LotFrontage from LotArea

# Possible imputation strategies
houses.all <- rbind(houses.train, houses.test)
impute.linear1 <- lm(LotFrontage ~ LotArea, data = houses.all)
impute.linear2 <- lm(LotFrontage ~ 0 + LotArea, data = houses.all)
impute.log1 <- lm(LotFrontage ~ log(LotArea), data = houses.all)
impute.log2 <- lm(LotFrontage ~ 0 + log(LotArea), data = houses.all)

# Justification for picking impute.log2:
# In the physical world, area is proportional to length squared
# Also, when the area is 0, the length should be 0

# Impute train using log2 model
imputed <- predict(impute.log2, houses.train)
houses.train$LotFrontage[is.na(houses.train$LotFrontage)] <- imputed[is.na(houses.train$LotFrontage)]

# Impute test using log2 model
imputed <- predict(impute.log2, houses.test)
houses.test$LotFrontage[is.na(houses.test$LotFrontage)] <- imputed[is.na(houses.test$LotFrontage)]

# Impute GarageYrBlt using YearBuilt
houses.train$GarageYrBlt[is.na(houses.train$GarageYrBlt)] <- 
  houses.train$YearBuilt[is.na(houses.train$GarageYrBlt)]

houses.test$GarageYrBlt[is.na(houses.test$GarageYrBlt)] <- 
  houses.test$YearBuilt[is.na(houses.test$GarageYrBlt)]

# Fix Outlier
houses.test$GarageYrBlt[which(houses.test$GarageYrBlt == 2207)] <- 2007

# Re-check
NA.analysis(rbind(houses.train, houses.test))

save(houses.train, houses.test, file = "./house_imputed.RData")