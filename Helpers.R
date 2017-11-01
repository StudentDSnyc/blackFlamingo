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




