# Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(Amelia)

# Load Helper Functions
source("Helpers.R")

# Load Data
load("./data/house_cleaned.RData")

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


##################
# Features
##################

houses = rbind(houses.train, houses.test) # need to only do feature engineering that doesn't leak information
# Lot landscape-ablility
houses[, c("LotShape.LandContour.interaction") := list(with(houses, interaction(LotShape, LandContour)))]
levels(houses$LotShape.LandContour.interaction)[levels(houses$LotShape.LandContour.interaction) %in%
                                                  c("IR1.Bnk", "IR2.Bnk", "IR3.Bnk")] <- "IR.Bnk"
levels(houses$LotShape.LandContour.interaction)[levels(houses$LotShape.LandContour.interaction) %in%
                                                  c("IR1.HLS", "IR2.HLS", "IR3.HLS")] <- "IR.HLS"
levels(houses$LotShape.LandContour.interaction)[levels(houses$LotShape.LandContour.interaction) %in%
                                                  c("IR1.Low", "IR2.Low", "IR3.Low")] <- "IR.Low"


# Garage interaction (quality * number of cars)
houses[, c("Garage.interaction") := list(with(houses, interaction(GarageCars, GarageQual)))] # Very positive effect
# ignored warning message. Behaviour correct
levels(houses$Garage.interaction)[levels(houses$Garage.interaction) %in%
                                    c("3.ExGd", "4.ExGd", "5.ExGd")] <- "2.ExGd"
levels(houses$Garage.interaction)[levels(houses$Garage.interaction) %in%
                                    c("5.TA")] <- "4.TA"
levels(houses$Garage.interaction)[levels(houses$Garage.interaction) %in%
                                    c("3.FaPo", "4.FaPo", "5.FaPo")] <- "2.FaPo"
levels(houses$Garage.interaction)[levels(houses$Garage.interaction) %in%
                                    c("1.NA", "2.NA", "3.NA", "4.NA", "5.NA")] <- "0.NA"
levels(houses$Garage.interaction)[levels(houses$Garage.interaction) %in%
                                    c("0.ExGd", "0.TA", "0.FaPo")] <- "0.NA"
levels(houses$Garage.interaction)[levels(houses$Garage.interaction) %in%
                                    c("1.ExGd")] <- "2.FaPo"
houses[,Garage.interaction:=droplevels(Garage.interaction)] # drop unused levels


# Average (above ground) room size
houses[, c("Room.size") := round((GrLivArea/TotRmsAbvGrd))] # no effect?

# Number of bathrooms. Only count basement bathrooms if there's a 'finished' basement area 
houses[BsmtFinSF1>0, c("TotalBath") := list(FullBath + HalfBath + BsmtFullBath + BsmtHalfBath)] # Positive Effect
houses[BsmtFinSF1==00, TotalBath := list(FullBath + HalfBath)]


# Ratio of bathrooms to bedrooms (all above ground), compared to neighbourhood ratio # Negative
houses[BedroomAbvGr==0, c("BathToBed")] <- 0
houses[BedroomAbvGr > 0, c("BathToBed") := (FullBath + HalfBath)/BedroomAbvGr]

# Relative living area 
houses[, c("AvgHouseLivArea.ratio") := (GrLivArea)/ mean(GrLivArea)]

# Put SalePrice back in the last position
setcolorder(houses, c(setdiff(names(houses), "SalePrice"), "SalePrice"))

# Re-split data
houses.train <- houses[1:1460,]
houses.test <- houses[1461:2919,]
dim(houses.train)


#########################
# Label-Encode Ordinals
#########################

# ExterQual - Ordinal 
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

# ExterCond - Ordinal 
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

# KitchenQual - Ordinal 
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

# GarageQual - Ordinal
levels(houses.test$GarageQual)[levels(houses.test$GarageQual) %in% 
                                 c("Ex")] <- 5
levels(houses.test$GarageQual)[levels(houses.test$GarageQual) %in% 
                                 c("Gd")] <- 4
levels(houses.test$GarageQual)[levels(houses.test$GarageQual) %in% 
                                 c("TA")] <- 3
levels(houses.test$GarageQual)[levels(houses.test$GarageQual) %in% 
                                 c("Fa")] <- 2
levels(houses.test$GarageQual)[levels(houses.test$GarageQual) %in% 
                                 c("Po")] <- 1

levels(houses.train$GarageQual)[levels(houses.train$GarageQual) %in% 
                                  c("Ex")] <- 5
levels(houses.train$GarageQual)[levels(houses.train$GarageQual) %in% 
                                  c("Gd")] <- 4
levels(houses.train$GarageQual)[levels(houses.train$GarageQual) %in% 
                                  c("TA")] <- 3
levels(houses.train$GarageQual)[levels(houses.train$GarageQual) %in% 
                                  c("Fa")] <- 2
levels(houses.train$GarageQual)[levels(houses.train$GarageQual) %in% 
                                  c("Po")] <- 1

# GarageCond - Ordinal
levels(houses.test$GarageCond)[levels(houses.test$GarageCond) %in% 
                                 c("Ex")] <- 5
levels(houses.test$GarageCond)[levels(houses.test$GarageCond) %in% 
                                 c("Gd")] <- 4
levels(houses.test$GarageCond)[levels(houses.test$GarageCond) %in% 
                                 c("TA")] <- 3
levels(houses.test$GarageCond)[levels(houses.test$GarageCond) %in% 
                                 c("Fa")] <- 2
levels(houses.test$GarageCond)[levels(houses.test$GarageCond) %in% 
                                 c("Po")] <- 1

levels(houses.train$GarageCond)[levels(houses.train$GarageCond) %in% 
                                  c("Ex")] <- 5
levels(houses.train$GarageCond)[levels(houses.train$GarageCond) %in% 
                                  c("Gd")] <- 4
levels(houses.train$GarageCond)[levels(houses.train$GarageCond) %in% 
                                  c("TA")] <- 3
levels(houses.train$GarageCond)[levels(houses.train$GarageCond) %in% 
                                  c("Fa")] <- 2
levels(houses.train$GarageCond)[levels(houses.train$GarageCond) %in% 
                                  c("Po")] <- 1

# HeatingQC - Ordinal 
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

# BsmtQual - Ordinal
levels(houses.test$BsmtQual)[levels(houses.test$BsmtQual) %in% 
                               c("Ex")] <- 5
levels(houses.test$BsmtQual)[levels(houses.test$BsmtQual) %in% 
                               c("Gd")] <- 4
levels(houses.test$BsmtQual)[levels(houses.test$BsmtQual) %in% 
                               c("TA")] <- 3
levels(houses.test$BsmtQual)[levels(houses.test$BsmtQual) %in% 
                               c("Fa")] <- 2
levels(houses.test$BsmtQual)[levels(houses.test$BsmtQual) %in% 
                               c("Po")] <- 1

levels(houses.train$BsmtQual)[levels(houses.train$BsmtQual) %in% 
                                c("Ex")] <- 5
levels(houses.train$BsmtQual)[levels(houses.train$BsmtQual) %in% 
                                c("Gd")] <- 4
levels(houses.train$BsmtQual)[levels(houses.train$BsmtQual) %in% 
                                c("TA")] <- 3
levels(houses.train$BsmtQual)[levels(houses.train$BsmtQual) %in% 
                                c("Fa")] <- 2
levels(houses.train$BsmtQual)[levels(houses.train$BsmtQual) %in% 
                                c("Po")] <- 1

# BsmtCond - Ordinal
levels(houses.test$BsmtCond)[levels(houses.test$BsmtCond) %in% 
                               c("Ex")] <- 5
levels(houses.test$BsmtCond)[levels(houses.test$BsmtCond) %in% 
                               c("Gd")] <- 4
levels(houses.test$BsmtCond)[levels(houses.test$BsmtCond) %in% 
                               c("TA")] <- 3
levels(houses.test$BsmtCond)[levels(houses.test$BsmtCond) %in% 
                               c("Fa")] <- 2
levels(houses.test$BsmtCond)[levels(houses.test$BsmtCond) %in% 
                               c("Po")] <- 1

levels(houses.train$BsmtCond)[levels(houses.train$BsmtCond) %in% 
                                c("Ex")] <- 5
levels(houses.train$BsmtCond)[levels(houses.train$BsmtCond) %in% 
                                c("Gd")] <- 4
levels(houses.train$BsmtCond)[levels(houses.train$BsmtCond) %in% 
                                c("TA")] <- 3
levels(houses.train$BsmtCond)[levels(houses.train$BsmtCond) %in% 
                                c("Fa")] <- 2
levels(houses.train$BsmtCond)[levels(houses.train$BsmtCond) %in% 
                                c("Po")] <- 1

# Make ordinal categorical OverallQual numeric
houses.train$OverallQual <- as.numeric(houses.train$OverallQual)
houses.test$OverallQual <- as.numeric(houses.test$OverallQual)

# Make ordinal categorical OverallCond numeric
houses.train$OverallCond <- as.numeric(houses.train$OverallCond)
houses.test$OverallCond <- as.numeric(houses.test$OverallCond)

# Save data
save(houses.train, houses.test, file = "./data/house_imputed.RData")




