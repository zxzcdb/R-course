rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  complete.data <- read.csv("outcome-of-care-measures.csv")
  
  ##Check if outcome is valid
  disease.index <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% disease.index)){
    stop("invalid outcome")
  }    
  
  ##Select 30-day mortality rate by diseases
  if(outcome == "heart attack"){
    small.table <- complete.data[,c(2,7,11)]   
  }else if(outcome == "heart failure"){
    small.table <- complete.data[,c(2,7,17)]    
  }else if(outcome == "pneumonia"){
    small.table <- complete.data[,c(2,7,23)]       
  }
  
  ## Remove NA data
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  small.table[[3]] <- as.numeric.factor(small.table[[3]])
  complete.compare <- small.table[complete.cases(small.table[[3]]),]
  
  ## Split the table by states
  cc.list <- split(complete.compare, complete.compare[[2]])
  
  ## Find the target hospital in each state
  ## Combine the result into a data frame
  output <- NULL  
  if(num == "best") num <- 1
  for(i in 1:length(cc.list)){
    tb <- cc.list[[i]]
    if(num == "worst"){
      tb <- tb[order(tb[,3],tb[,1]),]
      tb2 <- tail(tb,1)
      output <- rbind(output, tb2)
    }else if(num <= nrow(tb)){
      tb <- tb[order(tb[,3],tb[,1]),]
      tb2 <- tb[num,]
      output <- rbind(output, tb2)
    }else if(num > nrow(tb)){
      tb2 <- tb[1,]
      tb2[1,1] <- NA
      output <- rbind(output, tb2)
    }       
  } 
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    output <- output[,c(1,2)]
    colnames(output) <- c('hospital','state')
    output
  }
