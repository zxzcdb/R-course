## Incomplete Version

## To select the target hospital by state and mortality rank (lowest 30-day death rate by disease).
## Return the alpherbatical No.1 result.

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
  ## Sort hospital
  index <- as.numeric(names(table(complete.compare[,2])))
  index <- sort(index)
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if(num == "best") num <- 1
  if(num == "worst") num <- length(index)
  if(num > length(index)) return(NA)
  result <- complete.compare[[1]][which(complete.compare[[2]] == index[num], )]
  sort(result)
  resultc <- as.character(result[1])
  resultc
}
