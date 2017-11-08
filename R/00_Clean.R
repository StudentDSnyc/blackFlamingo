# Load Libraries
library(data.table)

# Assumes data directory
fetch_data <- function(filename) {
  dt <- fread(paste0("../Data/", filename),  # faster option than read.csv
              header = TRUE,
              sep = ",",
              stringsAsFactors = FALSE, # load text variables as categorical
              na.strings = NULL) # don't coerce "NA" string to NA
  dt
}

# Load training and test data
houses.train <- fetch_data("train.csv")
houses.test <- fetch_data("test.csv")


## get levels of categorical features from data description
factorLevel <- list()
conn <- file("./data/data_description.txt", open="r")
f <-readLines(conn)
for (line in f){
  if(!grepl("^[[:blank:]]", line) & grepl(": ", line)) {
    col_name <<- trimws(gsub(":.*", "", line))
  } else {
    level <- trimws(gsub("\t.*", "", line))
    if (level != "") {
      factorLevel[[col_name]] <- c(factorLevel[[col_name]], level)
    }
  }
}
close(conn)

## Change column names starting with numbers 
which(colnames(houses.train) %in% c("1stFlrSF", "2ndFlrSF","3SsnPorch"))
#44, 45, 70
colnames(houses.train)[44] = "X1stFlrSF"
colnames(houses.train)[45] = "X2ndFlrSF"
colnames(houses.train)[70] = "X3SsnPorch"

colnames(houses.test)[44] = "X1stFlrSF"
colnames(houses.test)[45] = "X2ndFlrSF"
colnames(houses.test)[70] = "X3SsnPorch"

# Fixing level names
unique(houses.train$MSZoning)
factorLevel$MSZoning
factorLevel$MSZoning[2] <- "C (all)"

unique(houses.train$Neighborhood)
factorLevel$Neighborhood
factorLevel$Neighborhood[13] <- "NAmes"

unique(houses.train$BldgType)
factorLevel$BldgType
factorLevel$BldgType[c(2,3,5)] <- c("2fmCon","Duplex","Twnhs")

unique(houses.train$Exterior2nd)
factorLevel$Exterior2nd
factorLevel$Exterior2nd[c(17,6,3)] <- c("Wd Shng","CmentBd","Brk Cmn")

## convert column datatype to numeric / factor
## On training dataset
for (varname in names(houses.train)[-1]) {
  if (varname %in% names(factorLevel)) {
    houses.train[[varname]] <- factor(houses.train[[varname]], 
                                      levels = factorLevel[[varname]])
  } else {
    houses.train[[varname]] <- as.numeric(houses.train[[varname]])
  }
}

## On testing dataset
for (varname in names(houses.test)[-1]) {
  if (varname %in% names(factorLevel)) {
    houses.test[[varname]] <- factor(houses.test[[varname]], 
                                     levels = factorLevel[[varname]])
  } else {
    houses.test[[varname]] <- as.numeric(houses.test[[varname]])
  }
}


## Save data
houses.train$Id <- NULL
rownames(houses.test) <- houses.test$Id
houses.test$Id <- NULL
save(houses.train, houses.test, file = "./data/house_loaded.RData")

