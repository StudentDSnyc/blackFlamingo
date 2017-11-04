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
# Encoding functions
##########################

encode.scale.df <- function(df){
  # Creates dummy variables and scale the whole dataframe
  
  set.seed(0)
  vtreat.encoder.df <- vtreat::designTreatmentsZ(df,
                                                            colnames(df),
                                                            minFraction= 0,
                                                            verbose=FALSE) # creates a "treatmentplan"
  
  # see vignette('vtreatVariableTypes', package = 'vtreat') for details
  sf <- vtreat.encoder.df$scoreFrame
  newvars <- sf$varName[sf$code %in% c("lev", "clean", "isBAD")] 
  encoded.df <- as.data.frame(vtreat::prepare(vtreat.encoder.df,
                                              df,
                                              scale = FALSE,  # doesn't work with TRUE
                                              varRestriction = newvars))
  cat("Encoded dataframe dimensions: ", dim(encoded.df))
  data.frame(lapply(encoded.df, scale))
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




