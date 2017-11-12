library(devtools)
library(data.table)

##################
# Load
##################
source('Impute.R')
load("./data/Xhouse_imputed.RData") 


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

# Split back
houses.train <- houses[1:1460,]
houses.test <- houses[1461:2919,]
dim(houses.train)

##################
# Save
##################

# to .RData
save(houses.train, file = "./data/Xhouses.train.RData")
save(houses.test, file = "./data/Xhouses.test.RData")

# to .csv
fwrite(houses.train, file = "../Data/Xhouses_train.csv", quote=F, row.names=T)
fwrite(houses.test, file = "../Data/Xhouses_test.csv", quote=F, row.names=T)


###################
# Unused features #
###################

# Basement interaction (quality of basement * number of (bath)rooms)
# houses[, c("Basement.interaction") := list(with(houses, interaction(BsmtQual, (BsmtFullBath+BsmtHalfBath))))]
# houses[,Basement.interaction:=droplevels(Basement.interaction)]
# houses[,Basement.interaction:=droplevels(Basement.interaction)]
# levels(houses$Garage.interaction)[levels(houses$Garage.interaction) %in% c("0.ExGd", "0.TA", "0.FaPo")]

# Additional Real Estate 'specialty' variables

# Kitchen interaction (quality * number of kitchens)
# houses[, c("Kitchen.interaction") := list(with(houses, interaction(KitchenAbvGr, KitchenQual)))] # Negative
# houses[,Kitchen.interaction:=droplevels(Kitchen.interaction)] # drop unused levels
# levels(houses$Kitchen.interaction)[levels(houses$Kitchen.interaction) %in% c("2.4")] <- "1.4"
# levels(houses$Kitchen.interaction)[levels(houses$Kitchen.interaction) %in% c("0.3")] <- "1.3"
# levels(houses$Kitchen.interaction)[levels(houses$Kitchen.interaction) %in% c("3.3")] <- "2.3"


# When house was built/ remodelled compared to neighbourhood
# houses[, c("new.old") := list((YearBuilt) - mean(YearBuilt)), by = .(Neighborhood, MSSubClass)] # Negative effect