rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  complete.data <- read.csv("outcome-of-care-measures.csv")
  ##Check that state and outcome are valid
  if (!(state %in% complete.data$State)){
    stop("invalid state")
  } 
  disease.index <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% disease.index)){
    stop("invalid outcome")
  }    
  ##Select lowest 30-day death rate by diseases
  if(outcome == "heart attack"){
    small.table <- complete.data[,c(2,7,11)]   
  }else if(outcome == "heart failure"){
    small.table <- complete.data[,c(2,7,17)]    
  }else if(outcome == "pneumonia"){
    small.table <- complete.data[,c(2,7,23)]       
  }
  
  ##Select data by state
  ##Remove NA data
  compare <- small.table[small.table$State == state, c(1,3)]
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  compare[[2]] <- as.numeric.factor(compare[[2]])
  complete.compare <- compare[complete.cases(compare[[2]]),]
  
  ## Sort hospital 
  complete.compare <- 
    complete.compare[order(complete.compare[,2],complete.compare[,1]),]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if(num == "best") num <- 1
  if(num == "worst") num <- length(complete.compare[[1]])
  if(num > length(complete.compare[[1]])) return(NA)
  result <- complete.compare[num,1]
  resultc <- as.character(result[1])
  resultc
}
