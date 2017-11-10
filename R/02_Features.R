library(devtools)
#install_github("gathanei/xyz")
# library("xyz")


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


# Interactions # Prints interaction but not sure how to create the features we need or get model using them
# result <- xyz_regression(data.matrix(houses[1:1460, -which(names(houses) == "SalePrice"), with=FALSE]),
#                houses[1:1460, ][["SalePrice"]], # using list subsetting '[[' to access data.table column (fast)
#                lambdas = 10^seq(1, -3, length = 10), n_lambda = 10,
#                alpha = 0.1, L = 10,
#                standardize = TRUE,
#                standardize_response = TRUE)

# Interaction effect:
# Interaction effect: (4,8) coefficient: -0.1066311 # LotArea LandContour       # ok makes sense

# Interaction effect: (14,14) coefficient: -0.08428927
# Interaction effect: (30,72) coefficient: 0.07212801
# Interaction effect: (4,4) coefficient: -0.0693587
# Interaction effect: (3,26) coefficient: 0.06451609
# Interaction effect: (3,43) coefficient: -0.06385154


# Adding interaction variables
houses[, c("LotArea.LandContour.interaction") := list(with(houses, interaction(quantile(houses$LotArea, probs = seq(0, 1, 0.25)), LandContour)))]

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


# Basement interaction (quality of basement * number of (bath)rooms)
# houses[, c("Basement.interaction") := list(with(houses, interaction(BsmtQual, (BsmtFullBath+BsmtHalfBath))))]
# houses[,Basement.interaction:=droplevels(Basement.interaction)]
# houses[,Basement.interaction:=droplevels(Basement.interaction)]
# levels(houses$Garage.interaction)[levels(houses$Garage.interaction) %in% c("0.ExGd", "0.TA", "0.FaPo")]

# Additional Real Estate 'specialty' variables

# Kitchen interaction (quality * number of cars)
houses[, c("Kitchen.interaction") := list(with(houses, interaction(KitchenAbvGr, KitchenQual)))] # Negative
houses[,Kitchen.interaction:=droplevels(Kitchen.interaction)] # drop unused levels
levels(houses$Kitchen.interaction)[levels(houses$Kitchen.interaction) %in% c("2.4")] <- "1.4"
levels(houses$Kitchen.interaction)[levels(houses$Kitchen.interaction) %in% c("0.3")] <- "1.3"
levels(houses$Kitchen.interaction)[levels(houses$Kitchen.interaction) %in% c("3.3")] <- "2.3"


# When house was built/ remodelled compared to neighbourhood
houses[, c("new.old") := list((YearBuilt) - mean(YearBuilt)), by = .(Neighborhood, MSSubClass)] # Negative effect

# Average (above ground) room size (compared to Neighborhood)
houses[, c("Room.size") := round((GrLivArea/ TotRmsAbvGrd))] # no effect?

# Combine sold date year and month
houses[, c("full.YrSold") := as.numeric(sprintf("%d%02d", YrSold, MoSold))] # add 0 in front of month digit as needed

# Create quarter for seasonality
houses[, c("QuarterSold") := (MoSold)]
houses[MoSold %in% c(1, 2, 3), c("QuarterSold")] <- 1
houses[MoSold %in% c(4, 5, 6), c("QuarterSold")] <- 2
houses[MoSold %in% c(7, 8, 9), c("QuarterSold")] <- 3
houses[MoSold %in% c(10, 11, 12), c("QuarterSold")] <- 4
houses[, c("Year.Quarter") := paste0(YrSold, QuarterSold)] # only needed to merge HPI data in
houses[,YrSold:=NULL]
houses[, MoSold:=NULL]

# HPI <- fread(paste0("../Data/", "AmesHPI_Clean.csv"), 
#             header = TRUE,
#             sep = ",",
#             stringsAsFactors = FALSE,
#             na.strings = NULL) # don't coerce "NA" string to NA
# 
# houses <- merge(houses, HPI, by.x = "Year.Quarter", by.y = "Year.Quarter")

# Cleanup
# rm(HPI)
houses[, Year.Quarter:=NULL]

# Number of bathrooms. Only count basement bathrooms if there's a 'finished' basement area 
houses[BsmtFinSF1>0, c("TotalBath") := list(FullBath + .5*HalfBath + BsmtFullBath + .5*BsmtHalfBath)] # Positive Effect
houses[BsmtFinSF1==00, TotalBath := list(FullBath + .5*HalfBath)]

# Ratio of bathrooms to bedrooms (all above ground), compared to neighbourhood ratio # Negative
houses[BedroomAbvGr>0, c("BathToBed") := ((FullBath + .5*HalfBath)/ BedroomAbvGr)/ mean((FullBath + .5*HalfBath)/ BedroomAbvGr), by = .(Neighborhood)]
houses[BedroomAbvGr==0, c("BathToBed")] <- 0

# Ratio: House surface to median for neighbourhood/ type of dwelling (e.g. 1-STORY 1945 & OLDER)
houses[, c("AvgHouseLivArea.ratio") := (GrLivArea)/ mean(GrLivArea), by = .(Neighborhood, MSSubClass)] # Positive effect

# Put SalePrice back in the last position
setcolorder(houses, c(setdiff(names(houses), "SalePrice"), "SalePrice"))

# Split back
houses.train <- houses[1:1460,]
houses.test <- houses[1461:2919,]
dim(houses.train)

##################
# Save
##################

# to .RData
save(houses.train, file = "./data/houses.train.RData")
save(houses.test, file = "./data/houses.test.RData")

# to .csv
fwrite(houses.train, file = "../Data/features_houses_train.csv", quote=F, row.names=T)
fwrite(houses.test, file = "../Data/features_houses_test.csv", quote=F, row.names=T)


# Rejected features
# Living Area http://homeguides.sfgate.com/calculate-living-area-square-footage-33755.html 
# houses[, c("Adj.GrLivArea") := list(BsmtFinSF1+BsmtFinSF2+GrLivArea)] # Negative effect (!!)

# Lot to House Ratio
# houses[, c("LotToHouseRatio") := list(LotArea/ GrLivArea)] # Negative effect
# Same adjusted by type of Zoning (e.g. high density)
# houses[, c("LotToHouseSurfaceRatio") := list((LotArea/ GrLivArea)/ mean(LotArea/ GrLivArea)), by = .(MSZoning)] # Negative effect

