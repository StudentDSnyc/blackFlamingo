library(dplyr)

# BsmtExposure
select(houses.train, BsmtExposure) %>%
  group_by(BsmtExposure) %>%
  summarise(n())

# 1           Gd   134
# 2           Av   221
# 3           Mn   114
# 4           No   953
# 5           NA    38

# BsmtFinType1
select(houses.train, BsmtFinType1) %>%
  group_by(BsmtFinType1) %>%
  summarise(n())

# 1          GLQ   418
# 2          ALQ   220
# 3          BLQ   148
# 4          Rec   133
# 5          LwQ    74
# 6          Unf   430
# 7           NA    37

# BsmtFinType2
select(houses.train, BsmtFinType2) %>%
  group_by(BsmtFinType2) %>%
  summarise(n())

# 1          GLQ    14
# 2          ALQ    19
# 3          BLQ    33
# 4          Rec    54
# 5          LwQ    46
# 6          Unf  1256
# 7           NA    38

# Heating
select(houses.train, Heating) %>%
  group_by(Heating) %>%
  summarise(n())

# 1   Floor     1
# 2    GasA  1428
# 3    GasW    18
# 4    Grav     7
# 5    OthW     2
# 6    Wall     4

levels(houses.train$Heating)[levels(houses.train$Heating) %in% 
                               c("Floor", "Grav", "OthW", "Wall")] <- "Other"

# I propose an "other" level that catches Floor, Grav, OthW, and Wall


# CentralAir
select(houses.train, CentralAir) %>%
  group_by(CentralAir) %>%
  summarise(n())

# 1          N    95
# 2          Y  1365


# Electrical 
select(houses.train, Electrical) %>%
  group_by(Electrical) %>%
  summarise(n())

# 1      SBrkr  1335
# 2      FuseA    94
# 3      FuseF    27
# 4      FuseP     3
# 5        Mix     1

levels(houses.train$Electrical)[levels(houses.train$Electrical) %in% 
                                  c("FuseF", "FuseP", "Mix")] <- "FuseFP"

# Based on the documentation file about the variables (given below),
# I suggest combiningFuseF, FuseP, and Mix into a new level 'FuseFP'.

# SBrkr	Standard Circuit Breakers & Romex
# FuseA	Fuse Box over 60 AMP and all Romex wiring (Average)	
# FuseF	60 AMP Fuse Box and mostly Romex wiring (Fair)
# FuseP	60 AMP Fuse Box and mostly knob & tube wiring (poor)
# Mix	Mixed


# Functional
select(houses.train, Functional) %>%
  group_by(Functional) %>%
  summarise(n())

# 1        Typ  1360
# 2       Min1    31
# 3       Min2    34
# 4        Mod    15
# 5       Maj1    14
# 6       Maj2     5
# 7        Sev     1

levels(houses.train$Functional)[levels(houses.train$Functional) %in% 
                               c("Maj1", "Maj2", "Sev", "Sal")] <- "Maj"

# I propose combining Maj1, Maj2, and Sev into new level 'MajSev'


# Paved Drive
select(houses.train, PavedDrive) %>%
  group_by(PavedDrive) %>%
  summarise(n())

# 1          Y  1340
# 2          P    30
# 3          N    90


# Fence
select(houses.train, Fence) %>%
  group_by(Fence) %>%
  summarise(n())

# 1  GdPrv    59
# 2  MnPrv   157
# 3   GdWo    54
# 4   MnWw    11
# 5     NA  1179


# MiscFeature
select(houses.train, MiscFeature) %>%
  group_by(MiscFeature) %>%
  summarise(n())

# 1        Gar2     2
# 2        Othr     2
# 3        Shed    49
# 4        TenC     1
# 5          NA  1406


# SaleType
select(houses.train, SaleType) %>%
  group_by(SaleType) %>%
  summarise(n())

# 1       WD  1267
# 2      CWD     4
# 3      New   122
# 4      COD    43
# 5      Con     2
# 6    ConLw     5
# 7    ConLI     5
# 8    ConLD     9
# 9      Oth     3

levels(houses.train$SaleType)[levels(houses.train$SaleType) %in% 
                                  c("ConLw", "ConLI", "ConLD", "Oth")] <- "Con"
levels(houses.train$SaleType)[levels(houses.train$SaleType) %in% 
                                c("CWD")] <- "WD"


# Foundation
select(houses.train, Foundation) %>%
  group_by(Foundation) %>%
  summarise(n())

# 1     BrkTil   146
# 2     CBlock   634
# 3      PConc   647
# 4       Slab    24
# 5      Stone     6
# 6       Wood     3

levels(houses.train$Foundation)[levels(houses.train$Foundation) %in% 
                                c("Stone", "Wood")] <- "Other"

# Exterior1st
select(houses.train, Exterior1st) %>%
  group_by(Exterior1st) %>%
  summarise(n())

# 1     AsbShng    20
# 2     AsphShn     1
# 3     BrkComm     2
# 4     BrkFace    50
# 5      CBlock     1
# 6     CemntBd    61
# 7     HdBoard   222
# 8     ImStucc     1
# 9     MetalSd   220
# 10     Plywood   108
# 11       Stone     2
# 12      Stucco    25
# 13     VinylSd   515
# 14     Wd Sdng   206
# 15     WdShing    26

levels(houses.train$Exterior1st)[levels(houses.train$Exterior1st) %in% 
                                  c("AsbShng", "AsphShn", "WdShing")] <- "Shingles"
levels(houses.train$Exterior1st)[levels(houses.train$Exterior1st) %in% 
                                   c("BrkComm", "BrkFace", "Brk Cmn", "Stone")] <- "Brick"
levels(houses.train$Exterior1st)[levels(houses.train$Exterior1st) %in% 
                                   c("CBlock", "CemntBd")] <- "Cement"
levels(houses.train$Exterior1st)[levels(houses.train$Exterior1st) %in% 
                                   c("ImStucc")] <- "Stucco"
levels(houses.train$Exterior1st)[levels(houses.train$Exterior1st) %in% 
                                   c("Other")] <- "VinylSd"


# Exterior2nd
select(houses.train, Exterior2nd) %>%
  group_by(Exterior2nd) %>%
  summarise(n())

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
