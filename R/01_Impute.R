# Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(Amelia)

# Load Helper Functions
source("Helpers.R")

# Load Data
load("./data/house_loaded.RData")

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



#################
# Releveling #
#################

# Heating
levels(houses.test$Heating)[levels(houses.test$Heating) %in% 
                               c("Floor", "Grav", "OthW", "Wall")] <- "Other"

levels(houses.train$Heating)[levels(houses.train$Heating) %in% 
                               c("Floor", "Grav", "OthW", "Wall")] <- "Other"

# Functional
levels(houses.test$Functional)[levels(houses.test$Functional) %in% 
                                  c("Maj1", "Maj2", "Sev", "Sal")] <- "Maj"

levels(houses.train$Functional)[levels(houses.train$Functional) %in% 
                                  c("Maj1", "Maj2", "Sev", "Sal")] <- "Maj"

# Electrical
levels(houses.test$Electrical)[levels(houses.test$Electrical) %in% 
                                  c("FuseF", "FuseP", "Mix")] <- "FuseFP"

levels(houses.train$Electrical)[levels(houses.train$Electrical) %in% 
                                  c("FuseF", "FuseP", "Mix")] <- "FuseFP"

# SaleType
levels(houses.test$SaleType)[levels(houses.test$SaleType) %in% 
                                c("ConLw", "ConLI", "ConLD", "Oth")] <- "Con"
levels(houses.test$SaleType)[levels(houses.test$SaleType) %in% 
                                c("CWD")] <- "WD"

levels(houses.train$SaleType)[levels(houses.train$SaleType) %in% 
                                  c("ConLw", "ConLI", "ConLD", "Oth")] <- "Con"
levels(houses.train$SaleType)[levels(houses.train$SaleType) %in% 
                                c("CWD")] <- "WD"


# Foundation
levels(houses.test$Foundation)[levels(houses.test$Foundation) %in% 
                                  c("Stone", "Wood")] <- "Other"

levels(houses.train$Foundation)[levels(houses.train$Foundation) %in% 
                                  c("Stone", "Wood")] <- "Other"

# Exterior1st
levels(houses.test$Exterior1st)[levels(houses.test$Exterior1st) %in% 
                                   c("AsbShng", "AsphShn", "WdShing")] <- "Shingles"
levels(houses.test$Exterior1st)[levels(houses.test$Exterior1st) %in% 
                                   c("BrkComm", "BrkFace", "Brk Cmn", "Stone")] <- "Brick"
levels(houses.test$Exterior1st)[levels(houses.test$Exterior1st) %in% 
                                   c("CBlock", "CemntBd")] <- "Cement"

levels(houses.test$Exterior1st)[levels(houses.test$Exterior1st) %in% 
                                   c("ImStucc")] <- "Stucco"
levels(houses.test$Exterior1st)[levels(houses.test$Exterior1st) %in% 
                                   c("Other", "PreCast")] <- "VinylSd"

levels(houses.train$Exterior1st)[levels(houses.train$Exterior1st) %in% 
                                   c("AsbShng", "AsphShn", "WdShing")] <- "Shingles"
levels(houses.train$Exterior1st)[levels(houses.train$Exterior1st) %in% 
                                   c("BrkComm", "BrkFace", "Brk Cmn", "Stone")] <- "Brick"
levels(houses.train$Exterior1st)[levels(houses.train$Exterior1st) %in% 
                                   c("CBlock", "CemntBd")] <- "Cement"
levels(houses.train$Exterior1st)[levels(houses.train$Exterior1st) %in% 
                                   c("ImStucc")] <- "Stucco"
levels(houses.train$Exterior1st)[levels(houses.train$Exterior1st) %in% 
                                   c("Other", "PreCast")] <- "VinylSd"

# Exterior2nd
levels(houses.test$Exterior2nd)[levels(houses.test$Exterior2nd) %in% 
                                   c("AsbShng", "AsphShn", "WdShing")] <- "Shingles"
levels(houses.test$Exterior2nd)[levels(houses.test$Exterior2nd) %in% 
                                   c("BrkComm", "BrkFace", "Brk Cmn", "Stone")] <- "Brick"
levels(houses.test$Exterior2nd)[levels(houses.test$Exterior2nd) %in% 
                                   c("CBlock", "CmentBd")] <- "Cement"
levels(houses.test$Exterior2nd)[levels(houses.test$Exterior2nd) %in% 
                                   c("ImStucc")] <- "Stucco"
levels(houses.test$Exterior2nd)[levels(houses.test$Exterior2nd) %in% 
                                   c("Other")] <- "VinylSd"

levels(houses.train$Exterior2nd)[levels(houses.train$Exterior2nd) %in% 
                                   c("AsbShng", "AsphShn", "WdShing")] <- "Shingles"
levels(houses.train$Exterior2nd)[levels(houses.train$Exterior2nd) %in% 
                                   c("BrkComm", "BrkFace", "Brk Cmn", "Stone")] <- "Brick"
levels(houses.train$Exterior2nd)[levels(houses.train$Exterior2nd) %in% 
                                   c("CBlock", "CmentBd")] <- "Cement"
levels(houses.train$Exterior2nd)[levels(houses.train$Exterior2nd) %in% 
                                   c("ImStucc")] <- "Stucco"
levels(houses.train$Exterior2nd)[levels(houses.train$Exterior2nd) %in% 
                                   c("Other")] <- "VinylSd"

# ExterQual - Ordinal numeric relevel
levels(houses.test$ExterQual)[levels(houses.test$ExterQual) %in% 
                                 c("Ex")] <- 5
levels(houses.test$ExterQual)[levels(houses.test$ExterQual) %in% 
                                 c("Gd")] <- 4
levels(houses.test$ExterQual)[levels(houses.test$ExterQual) %in% 
                                 c("TA")] <- 3
levels(houses.test$ExterQual)[levels(houses.test$ExterQual) %in% 
                                 c("Fa")] <- 2
levels(houses.test$ExterQual)[levels(houses.test$ExterQual) %in% 
                                 c("Po")] <- 1

levels(houses.train$ExterQual)[levels(houses.train$ExterQual) %in% 
                                 c("Ex")] <- 5
levels(houses.train$ExterQual)[levels(houses.train$ExterQual) %in% 
                                 c("Gd")] <- 4
levels(houses.train$ExterQual)[levels(houses.train$ExterQual) %in% 
                                 c("TA")] <- 3
levels(houses.train$ExterQual)[levels(houses.train$ExterQual) %in% 
                                 c("Fa")] <- 2
levels(houses.train$ExterQual)[levels(houses.train$ExterQual) %in% 
                                 c("Po")] <- 1

# ExterCond - Ordinal numeric relevel
levels(houses.test$ExterCond)[levels(houses.test$ExterCond) %in% 
                                 c("Ex")] <- 5
levels(houses.test$ExterCond)[levels(houses.test$ExterQual) %in% 
                                 c("Gd")] <- 4
levels(houses.test$ExterCond)[levels(houses.test$ExterQual) %in% 
                                 c("TA")] <- 3
levels(houses.test$ExterCond)[levels(houses.test$ExterQual) %in% 
                                 c("Fa")] <- 2
levels(houses.test$ExterCond)[levels(houses.test$ExterQual) %in% 
                                 c("Po")] <- 1

levels(houses.train$ExterCond)[levels(houses.train$ExterCond) %in% 
                                 c("Ex")] <- 5
levels(houses.train$ExterCond)[levels(houses.train$ExterQual) %in% 
                                 c("Gd")] <- 4
levels(houses.train$ExterCond)[levels(houses.train$ExterQual) %in% 
                                 c("TA")] <- 3
levels(houses.train$ExterCond)[levels(houses.train$ExterQual) %in% 
                                 c("Fa")] <- 2
levels(houses.train$ExterCond)[levels(houses.train$ExterQual) %in% 
                                 c("Po")] <- 1

# KitchenQual - Ordinal numeric relevel
levels(houses.test$KitchenQual)[levels(houses.test$KitchenQual) %in% 
                                   c("Ex")] <- 5
levels(houses.test$KitchenQual)[levels(houses.test$KitchenQual) %in% 
                                   c("Gd")] <- 4
levels(houses.test$KitchenQual)[levels(houses.test$KitchenQual) %in% 
                                   c("TA")] <- 3
levels(houses.test$KitchenQual)[levels(houses.test$KitchenQual) %in% 
                                   c("Fa")] <- 2
levels(houses.test$KitchenQual)[levels(houses.test$KitchenQual) %in% 
                                   c("Po")] <- 1

levels(houses.train$KitchenQual)[levels(houses.train$KitchenQual) %in% 
                                   c("Ex")] <- 5
levels(houses.train$KitchenQual)[levels(houses.train$KitchenQual) %in% 
                                   c("Gd")] <- 4
levels(houses.train$KitchenQual)[levels(houses.train$KitchenQual) %in% 
                                   c("TA")] <- 3
levels(houses.train$KitchenQual)[levels(houses.train$KitchenQual) %in% 
                                   c("Fa")] <- 2
levels(houses.train$KitchenQual)[levels(houses.train$KitchenQual) %in% 
                                   c("Po")] <- 1

# GarageQual
levels(houses.test$GarageQual)[levels(houses.test$GarageQual) %in% 
                                  c("Ex", "Gd")] <- "ExGd"
levels(houses.test$GarageQual)[levels(houses.test$GarageQual) %in% 
                                  c("Fa", "Po")] <- "FaPo"

levels(houses.train$GarageQual)[levels(houses.train$GarageQual) %in% 
                                  c("Ex", "Gd")] <- "ExGd"
levels(houses.train$GarageQual)[levels(houses.train$GarageQual) %in% 
                                  c("Fa", "Po")] <- "FaPo"

# GarageCond
levels(houses.test$GarageCond)[levels(houses.test$GarageCond) %in% 
                                  c("Ex", "Gd")] <- "ExGd"
levels(houses.test$GarageCond)[levels(houses.test$GarageCond) %in% 
                                  c("Fa", "Po")] <- "FaPo"

levels(houses.train$GarageCond)[levels(houses.train$GarageCond) %in% 
                                  c("Ex", "Gd")] <- "ExGd"
levels(houses.train$GarageCond)[levels(houses.train$GarageCond) %in% 
                                  c("Fa", "Po")] <- "FaPo"

# HeatingQC - Ordinal numeric relevel
levels(houses.test$HeatingQC)[levels(houses.test$HeatingQC) %in% 
                                 c("Ex")] <- 5
levels(houses.test$HeatingQC)[levels(houses.test$HeatingQC) %in% 
                                 c("Gd")] <- 4
levels(houses.test$HeatingQC)[levels(houses.test$HeatingQC) %in% 
                                 c("TA")] <- 3
levels(houses.test$HeatingQC)[levels(houses.test$HeatingQC) %in% 
                                 c("Fa")] <- 2
levels(houses.test$HeatingQC)[levels(houses.test$HeatingQC) %in% 
                                 c("Po")] <- 1

levels(houses.train$HeatingQC)[levels(houses.train$HeatingQC) %in% 
                                   c("Ex")] <- 5
levels(houses.train$HeatingQC)[levels(houses.train$HeatingQC) %in% 
                                   c("Gd")] <- 4
levels(houses.train$HeatingQC)[levels(houses.train$HeatingQC) %in% 
                                   c("TA")] <- 3
levels(houses.train$HeatingQC)[levels(houses.train$HeatingQC) %in% 
                                   c("Fa")] <- 2
levels(houses.train$HeatingQC)[levels(houses.train$HeatingQC) %in% 
                                   c("Po")] <- 1

# SaleCondition
levels(houses.test$SaleCondition)[levels(houses.test$SaleCondition) %in% 
                                     c("AdjLand")] <- "Abnorml"

levels(houses.train$SaleCondition)[levels(houses.train$SaleCondition) %in% 
                                 c("AdjLand")] <- "Abnorml"

# Condition1
levels(houses.train$Condition1)[levels(houses.train$Condition1) %in%
                                c("RRNn")] <- "RRAn"
levels(houses.train$Condition1)[levels(houses.train$Condition1) %in%
                                c("RRNe")] <- "RRAe"
levels(houses.train$Condition1)[levels(houses.train$Condition1) %in%
                                  c("PosA")] <- "PosN"

levels(houses.test$Condition1)[levels(houses.test$Condition1) %in%
                                  c("RRNn")] <- "RRAn"
levels(houses.test$Condition1)[levels(houses.test$Condition1) %in%
                                  c("RRNe")] <- "RRAe"
levels(houses.test$Condition1)[levels(houses.test$Condition1) %in%
                                  c("PosA")] <- "PosN"

# Condition2
houses.train$Condition2 <- NULL # removing because too few levels
houses.test$Condition2 <- NULL

# HouseStyle
levels(houses.train$HouseStyle)[levels(houses.train$HouseStyle) %in%
                                  c("1.5Unf")] <- "1.5Fin"
levels(houses.train$HouseStyle)[levels(houses.train$HouseStyle) %in%
                                  c("2.5Unf")] <- "2.5Fin"
levels(houses.test$HouseStyle)[levels(houses.test$HouseStyle) %in%
                                  c("1.5Unf")] <- "1.5Fin"
levels(houses.test$HouseStyle)[levels(houses.test$HouseStyle) %in%
                                  c("2.5Unf")] <- "2.5Fin"

# Make ordinal categorical OverallQual numeric
houses.train$OverallQual <- as.numeric(houses.train$OverallQual)
houses.test$OverallQual <- as.numeric(houses.test$OverallQual)

# Make ordinal categorical OverallCond numeric
houses.train$OverallCond <- as.numeric(houses.train$OverallCond)
houses.test$OverallCond <- as.numeric(houses.test$OverallCond)

# PoolQC
levels(houses.train$PoolQC)[levels(houses.train$PoolQC) %in%
                                 c("Ex", "Gd", "TA", "Fa")] <- "Pool"
levels(houses.test$PoolQC)[levels(houses.test$PoolQC) %in%
                                 c("Ex", "Gd", "TA", "Fa")] <- "Pool"

# GarageType
levels(houses.train$GarageType)[levels(houses.train$GarageType) %in% 
                                  c("CarPort", "2Types")] <- "Other"
levels(houses.test$GarageType)[levels(houses.test$GarageType) %in% 
                                  c("CarPort", "2Types")] <- "Other"

# MiscFeature
levels(houses.train$MiscFeature)[levels(houses.train$MiscFeature) %in% 
                                   c("Gar2", "Othr", "TenC")] <- "NA"
levels(houses.test$MiscFeature)[levels(houses.test$MiscFeature) %in% 
                                   c("Gar2", "Othr", "TenC")] <- "NA"


#RoofStyle
levels(houses.train$RoofStyle)[levels(houses.train$RoofStyle) %in% c("Shed", "Mansard", "Gambrel")]= "Other"
levels(houses.test$RoofStyle)[levels(houses.test$RoofStyle) %in% c("Shed", "Mansard", "Gambrel")]= "Other"

#RoofMatl
levels(houses.train$RoofMatl)[levels(houses.train$RoofMatl) %in% c("ClyTile", "Membran", "Metal", "Roll")]= "Other"
levels(houses.train$RoofMatl)[levels(houses.train$RoofMatl) %in% c("WdShake", "WdShngl")]= "Wood"

levels(houses.test$RoofMatl)[levels(houses.test$RoofMatl) %in% c("ClyTile", "Membran", "Metal", "Roll")]= "Other"
levels(houses.test$RoofMatl)[levels(houses.test$RoofMatl) %in% c("WdShake", "WdShngl")]= "Wood"



houses.train[MSSubClass == 40]$MSSubClass <- "20"
houses.train[MSSubClass == 45]$MSSubClass <- "50"
houses.train[MSSubClass == 75]$MSSubClass <- "60"
houses.train[MSSubClass == 150]$MSSubClass <- "120"
houses.train[MSSubClass == 180]$MSSubClass <- "160"
houses.train$MSSubClass <- droplevels(houses.train$MSSubClass)

houses.test[MSSubClass == 40]$MSSubClass <- "20"
houses.test[MSSubClass == 45]$MSSubClass <- "50"
houses.test[MSSubClass == 75]$MSSubClass <- "60"
houses.test[MSSubClass == 150]$MSSubClass <- "120"
houses.test[MSSubClass == 180]$MSSubClass <- "160"
houses.test$MSSubClass <- droplevels(houses.test$MSSubClass)

houses.train$MSZoning <- droplevels(houses.train$MSZoning)

houses.test$MSZoning <- droplevels(houses.test$MSZoning)

houses.train$Utilities <- NULL

houses.test$Utilities <- NULL

houses.train[LotConfig == "FR3"]$LotConfig <- "FR2"
houses.train$LotConfig <- droplevels(houses.train$LotConfig)

houses.test[LotConfig == "FR3"]$LotConfig <- "FR2"
houses.test$LotConfig <- droplevels(houses.test$LotConfig)

houses.train$BsmtQual <- droplevels(houses.train$BsmtQual)

houses.test$BsmtQual <- droplevels(houses.test$BsmtQual)

houses.train[BsmtCond == "Po"]$BsmtCond <- "Fa"
houses.train$BsmtCond <- droplevels(houses.train$BsmtCond)

houses.test[BsmtCond == "Po"]$BsmtCond <- "Fa"
houses.test$BsmtCond <- droplevels(houses.test$BsmtCond)

# MasVnrType
houses.train$MasVnrType <- droplevels(houses.train$MasVnrType)
houses.test$MasVnrType <- droplevels(houses.test$MasVnrType)

houses.train <- droplevels(houses.train)
houses.test <- droplevels(houses.test)



# Save data
save(houses.train, houses.test, file = "./data/house_imputed.RData")