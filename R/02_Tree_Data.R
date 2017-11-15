##################
# Load
##################
load("./data/house_imputed.RData")

# Save house data for trees

write.csv(houses.train, "../Data/housetrain_tree.csv", row.names = FALSE)
write.csv(houses.test, "../Data/housetest_tree.csv", row.names = FALSE)


#########################
# Create private test set
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

# Save private data for trees
save(private.train, private.test, file = "./data/private_imputed.RData")
write.csv(private.train, "../Data/privtrain_tree.csv", row.names = FALSE)
write.csv(private.test, "../Data/privtest_tree.csv", row.names = FALSE)