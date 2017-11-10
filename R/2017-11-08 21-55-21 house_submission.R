hpi = read.csv('AmesHPI.csv')

names(hpi) = c("DATE", "HPI")
names(hpi)
head(hpi)


library(tidyr)
hpi = separate(hpi, DATE, into=c("year","month", "day"), sep="-")
head(hpi)
hpi = hpi[, -which(names(hpi) %in% c("day"))]
head(hpi)
levels(hpi$month)
hpi$month = as.factor(hpi$month)
levels(hpi$month)
levels(hpi$month) = c("1", "2", "3", "4")
hpi$month
head(hpi)

hpi = unite(hpi, "Year.Quarter", year, month, sep="")
head(hpi)

write.csv(hpi, "AmesHPI_Clean.csv", row.names = FALSE)

test = read.csv('AmesHPI_Clean.csv')
head(test)




lapply(private)

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

# I propose an "other" level that catches Floor, Grav, OthW, and Wall

# select(houses.train, Heating, CentralAir) %>%
#   group_by(Heating, CentralAir) %>%
#   summarise(n())
# 
# # 1   Floor          N     1
# # 2    GasA          N    69
# # 3    GasA          Y  1359
# # 4    GasW          N    12
# # 5    GasW          Y     6
# # 6    Grav          N     7
# # 7    OthW          N     2
# # 8    Wall          N     4
# 
# select(houses.train, Heating, Utilities) %>%
#   group_by(Heating, Utilities) %>%
#   summarise(n())
# 
# select(houses.train, Heating, Electrical) %>%
#   group_by(Heating, Electrical) %>%
#   summarise(n())


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

# Based on the documentation file about the variables (given below),
# I suggest combiningFuseF, FuseP, and Mix into a new level 'FuseFP'.

# SBrkr	Standard Circuit Breakers & Romex
# FuseA	Fuse Box over 60 AMP and all Romex wiring (Average)	
# FuseF	60 AMP Fuse Box and mostly Romex wiring (Fair)
# FuseP	60 AMP Fuse Box and mostly knob & tube wiring (poor)
# Mix	Mixed











