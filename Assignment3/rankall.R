#Mohamad Salman - R Programming Assignment 3 - 10/30/2014
rankall<- function(outcome, num = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  hospital <- character()
  oc.index <- 0
  ## Check that state and outcome are valid
  state <- sort(unique(data[, 7]))
  
  if (outcome == "heart attack") oc.index <- 11
  else if (outcome == "heart failure") oc.index <- 17
  else if (outcome == "pneumonia") oc.index <- 23
  else stop("invalid outcome")
  
  ## limit data to just the Hospital name, state and heart attack outcome
  data <- data[,c(2,7,oc.index)]
  ## rename the column names for easier access
  colnames(data) <- c("Name","State","MR")
  ## remove "Not Available" entries
  data <- data[data$MR != "Not Available",]
  data$Name <- as.character(data$Name)
  data$State <- as.character(data$State)
  data$MR <- as.numeric(paste(data$MR))
  
  for (i in state) {
    st.data <- data[data$State == i,c(1,3)]
    st.data <- st.data[order(st.data$Name),]
    st.data <- st.data[order(st.data$MR),]
    if((is.numeric(num))&(num > nrow(st.data))) hospital <- c(hospital,NA)
    else if(num == "best") hospital <- c(hospital,st.data[1, 1])
    else if(num == "worst") hospital <- c(hospital,st.data[nrow(st.data), 1])
    else hospital <- c(hospital,st.data[num, 1])
  }
  return(data.frame(hospital=hospital,state=state))
}