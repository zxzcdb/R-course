##This function returns the best hospital with the lowest mortality rate by disease and state.

best <- function(state, outcome){
  ## Read outcome data
  complete.data <- read.csv("outcome-of-care-measures.csv")
  ##Check that state and outcome are valid
  if (!(state %in% complete.data$State)){
    stop("invalid state")
  }
  ##Select lowest 30-day death rate by diseases   
  disease.index <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% disease.index)){
    stop("invalid outcome")
  }      
  if(outcome == "heart attack"){
    small.table <- complete.data[,c(2,7,13)]   
  }else if(outcome == "heart failure"){
    small.table <- complete.data[,c(2,7,19)]    
  }else if(outcome == "pneumonia"){
    small.table <- complete.data[,c(2,7,25)]       
  }
  ##Select data by state
  ##Remove NA data
  compare <- small.table[small.table$State == state, c(1,3)]
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  compare[[2]] <- as.numeric.factor(compare[[2]])
  complete.compare <- compare[complete.cases(compare[[2]]),]
  ##Return hospital name in that state with lowest 30-day death
  ##Rate 
  result <- complete.compare[[1]][which(complete.compare[[2]] == min(complete.compare[[2]]), )]
  as.character(result)
  best <- function(state, outcome){
  ## Read outcome data
  complete.data <- read.csv("outcome-of-care-measures.csv")
  ##Check that state and outcome are valid
  if (!(state %in% complete.data$State)){
    stop("invalid state")
  }
  ##Select lowest 30-day death rate by diseases   
  disease.index <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% disease.index)){
    stop("invalid outcome")
  }      
  if(outcome == "heart attack"){
    small.table <- complete.data[,c(2,7,13)]   
  }else if(outcome == "heart failure"){
    small.table <- complete.data[,c(2,7,19)]    
  }else if(outcome == "pneumonia"){
    small.table <- complete.data[,c(2,7,25)]       
  }
  ##Select data by state
  ##Remove NA data
  compare <- small.table[small.table$State == state, c(1,3)]
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  compare[[2]] <- as.numeric.factor(compare[[2]])
  complete.compare <- compare[complete.cases(compare[[2]]),]
  ##Return hospital name in that state with lowest 30-day death
  ##Rate 
  result <- complete.compare[[1]][which(complete.compare[[2]] == min(complete.compare[[2]]), )]
  result <- sort(result)
  resultc <- as.character(result[1])
  resultc
}
