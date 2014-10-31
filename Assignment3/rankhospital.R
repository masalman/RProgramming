#Mohamad Salman - R Programming Assignment 3 - 10/30/2014
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  oc.index <- 0
  ## Check that state and outcome are valid
  if(is.na(match(state, data[, 7]))) stop("invalid state")
  if (outcome == "heart attack") oc.index <- 11
  else if (outcome == "heart failure") oc.index <- 17
  else if (outcome == "pneumonia") oc.index <- 23
  else stop("invalid outcome")
  
  ## limit data to just the Hospital name, state and heart attack outcome
  data <- data[data$State == state,c(2,oc.index)]
  ## rename the column names for easier access
  colnames(data) <- c("Name","MR")
  ## remove "Not Available" entries
  data <- data[data$MR != "Not Available",]
  ## convert Name column to character vector
  data$Name <- as.character(data$Name)
  ## convert mortality rate column to numeric vector
  data$MR <- as.numeric(paste(data$MR))
  #data <- na.omit(data)
  data <- data[order(data$Name),]
  data <- data[order(data$MR),]
  ## Return hospital name in that state with given rank
  ## 30-day death rate
  if((is.numeric(num))&(num > nrow(data))) return(NA)
  else if(num == "best") return(data[1, 1])
  else if(num == "worst") return(data[nrow(data), 1])
  else return(data[num, 1])
     
}