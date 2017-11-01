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

# Re-check
NA.analysis(rbind(houses.train, houses.test))

save(houses.train, houses.test, file = "./house_imputed.RData")