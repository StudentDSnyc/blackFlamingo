#----------------------------------------- Functions ---------------------------

NA.analysis <- function(df){
  # prints the number of NA's and the % of overall values for all df columns containing at least one
  NA.count <- sapply(df, function(x) sum(is.na(x)))
  NA.percentage <- sapply(df, function(x) 100*sum(is.na(x))/length(x))
  
  #Create result matrix
  NA.summary = cbind(NA.count[NA.count>0], NA.percentage[NA.count>0])
  colnames(NA.summary) <- c("Count", "Percent")
  
  return(NA.summary[order(desc(NA.summary[,"Percent"])),])
}


## For Data Frames
replace.NA.most.frequent.value <- function(df, column){
  # Finds the most frequent value and replaces all NA's with it
  t <-table(as.factor(df[,column]), exclude=NULL)
  df[[column]][is.na(df[[column]])] <- names(which.max(t))
  return(df[[column]])
}

get.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# For Data Tables, non-mutating
impute.mode <- function(DT, column){
  if(anyNA(DT[[column]])){
    DT[is.na(DT[[column]])][[column]] <- get.mode(DT[[column]])
  }
  return(DT[[column]])
}

##########################
# Caret Summary helper functions
##########################

# untransform from box-cox given lambda
# doesn't account for lambda = 0
unbox <- function(data, lambda){
  ((data * lambda) + 1)^(1/lambda)
}

# measures RMSE of x %>% unbox %>% log, used in caret cross validation
RMSEbc <- function(data, lev = NULL, model = NULL){
  #print(data[,"pred"])
  fitted <- unbox(data[, "pred"], lambda)
  seen <- unbox(data[,"obs"], lambda)
  err <- sqrt(mean((log(seen) - log(fitted))^2, na.rm=FALSE))
  print(err)
  out <- c(err)
  names(out) <- c("RMSEbc")
  out
}

##########################
# Encoding functions
##########################

# encode.scale.df <- function(df){
#   # Creates dummy variables and scale the whole dataframe
# 
#   encoded.df <- stats::model.matrix(~., data=df)
#   cat("Encoded dataframe dimensions: ", dim(encoded.df))
#   
#   data.frame(lapply(encoded.df, scale))
# }

encode.scale.df <- function(df, outcomename = "SalePrice"){
  # Creates dummy variables and scale the whole dataframe
  
  set.seed(0)
  vtreat.encoder.df <- vtreat::designTreatmentsN(dframe = df,
                                                 varlist = colnames(df),
                                                 outcomename = outcomename,
                                                 rareCount=5,
                                                 rareSig=0.3,
                                                 verbose=TRUE) # creates a "treatmentplan"
  
  encoded.df <- as.data.frame(vtreat::prepare(vtreat.encoder.df,
                                              df,
                                              pruneSig=0.05,
                                              scale = TRUE))
                                              
  cat("Encoded dataframe dimensions: ", dim(encoded.df))
  # data.frame(lapply(encoded.df, scale))
  encoded.df
}


# Code to scale numeric features only
# indices <- sapply(houses, is.numeric)
# indices["SalePrice"] <- FALSE # don't scale the labels
# houses[indices]  <- data.frame(lapply(houses[indices], scale))

align.columns <- function(training.df, test.df) {
  # Add columns in training.df not in test.df to test.df, filled with 0s
  # Remove test.df columns not in training.df
  missing.cols <- setdiff(names(training.df), names(test.df))
  test.df[missing.cols] <- 0                   
  
  # Ensure same the order of columns in the training and test sets
  test.df[names(training.df)] 
}

label.count.encode.vector <- function(vector){
  # Replaces categorical labels with the number of occurences
  t <- table(vector, exclude=NULL)
  lapply(vector, function(x) t[names(t)==x])
}
# label.count.encoding(private.train$Condition1)

label.count.encode.df <- function(df){
  factor.columns <- sapply(df, is.factor)
  df[factor.columns] <- lapply(df[factor.columns], label.count.encode.vector)
  as.data.frame(sapply(df, as.numeric)) # convert created columns from 'list' to 'numeric'
}


