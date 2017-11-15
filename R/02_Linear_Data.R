library(vtreat)
library(car)
library(dplyr)

# Load Helper Functions
source("Helpers.R")

##################
# Load
##################
load("./data/house_imputed.RData")


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

# MSSubClass
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

# MSZoning
houses.train$MSZoning <- droplevels(houses.train$MSZoning)

houses.test$MSZoning <- droplevels(houses.test$MSZoning)

# Utilities
houses.train$Utilities <- NULL

houses.test$Utilities <- NULL

# LotConfig
houses.train[LotConfig == "FR3"]$LotConfig <- "FR2"
houses.train$LotConfig <- droplevels(houses.train$LotConfig)

houses.test[LotConfig == "FR3"]$LotConfig <- "FR2"
houses.test$LotConfig <- droplevels(houses.test$LotConfig)

# MasVnrType
houses.train$MasVnrType <- droplevels(houses.train$MasVnrType)
houses.test$MasVnrType <- droplevels(houses.test$MasVnrType)

# Street
houses.train$Street <- NULL
houses.test$Street <- NULL

houses.train <- droplevels(houses.train)
houses.test <- droplevels(houses.test)


#########################
# Create private set
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
encoded.private.train <- vtreat::prepare(encoder,
                                         private.train,
                                         pruneSig=1,
                                         scale = FALSE)

encoded.private.test <- vtreat::prepare(encoder,
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
encoded.houses.train <- vtreat::prepare(encoder,
                                        houses.train,
                                        pruneSig=1,
                                        scale = TRUE)

encoded.houses.test <- vtreat::prepare(encoder,
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
encoded.private.train <- scale(encoded.private.train, center = FALSE)
encoded.private.test <- scale(encoded.private.test, center = FALSE, attr(encoded.private.train, "scaled:scale"))
encoded.houses.train <- scale(encoded.houses.train, center = FALSE)
encoded.houses.test <- scale(encoded.houses.test, center = FALSE, attr(encoded.houses.train, "scaled:scale"))

# Convert to data.frame (from matrix) 
encoded.private.train <- as.data.frame.numeric(encoded.private.train)
encoded.private.test <- as.data.frame.numeric(encoded.private.test)
encoded.houses.train <- as.data.frame.numeric(encoded.houses.train)
encoded.houses.test <- as.data.frame.numeric(encoded.houses.test)

# Re-copy orignal y values (not scaled)
encoded.private.train[, c('SalePrice')] <- private.train$SalePrice
encoded.private.test[, c('SalePrice')] <- private.test$SalePrice
encoded.houses.train[, c('SalePrice')] <- houses.train$SalePrice
encoded.houses.test[, c('SalePrice')] <- 0

library(dplyr)

# Cut any linear combinations or duplicate columns generated by dummify
lincomb <- findLinearCombos(encoded.private.train)
#lapply(lincomb$linearCombos, function(x) colnames(encode.private.train)[x])
encoded.private.train <- encoded.private.train %>% select(-lincomb$remove)
encoded.private.test <- encoded.private.test %>% select(-lincomb$remove)

lincomb <- findLinearCombos(encoded.houses.train)
encoded.houses.train <- encoded.houses.train %>% select(-lincomb$remove)
encoded.houses.test <- encoded.houses.test %>% select(-lincomb$remove)



# Save encoded dataframes
save(encoded.private.train, file = "./data/privtrain_linear.RData")
save(encoded.private.test, file = "./data/privtest_linear.RData")
save(encoded.houses.train, file = "./data/housetrain_linear.RData")
save(encoded.houses.test, file = "./data/housetest_linear.RData")

#Save encoded dataframes as csv:
write.csv(encoded.private.train, "../Data/privtrain_linear.csv", row.names = FALSE)
write.csv(encoded.private.test, "../Data/privtest_linear.csv", row.names = FALSE)
write.csv(encoded.houses.train, "../Data/housetrain_linear.csv", row.names = FALSE)
write.csv(encoded.houses.test, "../Data/housetest_linear.csv", row.names = FALSE)
