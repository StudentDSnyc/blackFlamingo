# Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(Amelia)

# Load Helper Functions
source("Helpers.R")

# Load Data
load("./data/Xhouse_loaded.RData")

# Check missing values
houses.test$SalePrice <- 0
NA.analysis(rbind(houses.train, houses.test))

# Columns to impute with mode
to.impute <- c("MSZoning", "MasVnrType", "MasVnrArea", "Utilities", "BsmtFullBath",
               "BsmtHalfBath", "Functional", "BsmtFinSF1", "BsmtFinSF2",
               "BsmtUnfSF", "TotalBsmtSF", "Electrical", "SaleType", "Exterior1st", 
               "Exterior2nd", "KitchenQual", "GarageCars")

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
ImputeDependent(houses.test, "MSZoning", "Neighborhood", type = mode)

# Call ImputeDependent on x = "Exterior1st" and y = "Exterior2nd" and
# vice versa
ImputeDependent(houses.test, "Exterior1st", "Exterior2nd", type = mode)
ImputeDependent(houses.test, "Exterior2nd", "Exterior1st", type = mode)

# Impute LotFrontage from LotArea
impute.log <- lm(log(LotFrontage) ~ log(LotArea), data = houses.train)

# Justification for impute.log:
# In the physical world, area is proportional to length squared

# Impute train using log model
imputed <- exp(predict(impute.log, houses.train))
houses.train$LotFrontage[is.na(houses.train$LotFrontage)] <- imputed[is.na(houses.train$LotFrontage)]

# Impute test using log model
imputed <- exp(predict(impute.log, houses.test))
houses.test$LotFrontage[is.na(houses.test$LotFrontage)] <- imputed[is.na(houses.test$LotFrontage)]

# Impute GarageYrBlt using YearBuilt
houses.train$GarageYrBlt[is.na(houses.train$GarageYrBlt)] <- 
  houses.train$YearBuilt[is.na(houses.train$GarageYrBlt)]

houses.test$GarageYrBlt[is.na(houses.test$GarageYrBlt)] <- 
  houses.test$YearBuilt[is.na(houses.test$GarageYrBlt)]

# Fix Outlier
houses.test$GarageYrBlt[which(houses.test$GarageYrBlt == 2207)] <- 2007

# Fix Outlier (row 1117 from houses.test) Impute GarageArea from overall mean
houses.train$GarageArea[is.na(houses.train$GarageArea)] = round(mean(houses.train$GarageArea, na.rm=TRUE))
houses.test$GarageArea[is.na(houses.test$GarageArea)] = round(mean(houses.test$GarageArea, na.rm=TRUE))

# Re-check
NA.analysis(rbind(houses.train, houses.test))

# Save data
save(houses.train, houses.test, file = "./data/Xhouse_imputed.RData")