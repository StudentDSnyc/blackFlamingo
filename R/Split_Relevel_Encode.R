library(ggplot2)
library(caret)
library(vtreat)
library(car)

# Load Helper Functions
source("Helpers.R")

##################
# Load
##################

source('./Features.R')
load("./data/Xhouses.train.RData")
load("./data/Xhouses.test.RData")


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

# Street
houses.train$Street <- NULL
houses.test$Street <- NULL

houses.train <- droplevels(houses.train)
houses.test <- droplevels(houses.test)



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



