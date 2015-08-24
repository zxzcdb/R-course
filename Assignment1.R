pollutantmean <- function(directory, pollutant, id = 1:332) {
  initialwd <- getwd()
  path <- file.path(getwd(),directory)
  setwd(path)
  x = numeric()
  for (i in id){    
    fn <- formatC(i, width = 3, flag = '0')
    fn <- paste(fn, ".csv", sep = "")
    monitor <- read.csv(fn)
    if (pollutant == "sulfate"){      
      v <- na.omit(monitor$sulfate)
      x <- append(x, v)
    } else if (pollutant == "nitrate"){    
      v <- na.omit(monitor$nitrate)
      x <- append(x, v)
    }
  }
  setwd(initialwd)
  mean(x)
}
complete <- function(directory, id = 1:332) {
  initialwd <- "C:/Users/te121877/Documents/rdpeng"
  setwd(initialwd)
  path <- file.path(getwd(),directory)
  setwd(path)
  df <- data.frame(x = numeric(), y = numeric())
  colnames(df) <- c("id","nobs")
  r <- 1
  for (i in id){ 
    fn <- formatC(i, width = 3, flag = '0')
    fn <- paste(fn, ".csv", sep = "")
    monitor <- read.csv(fn)
    cc <- sum(complete.cases(monitor$sulfate, monitor$nitrate))
    df <- rbind(df, data.frame(x = i, y = cc))
    r <- r + 1
  }
  setwd(initialwd) 
  df
}
corr <- function(directory, threshold = 0) {
  initialwd <- "C:/Users/te121877/Documents/rdpeng"
  setwd(initialwd)
  path <- file.path(getwd(),directory)
  setwd(path)
  v <- numeric()
  x <- numeric()
  for(i in 1:332){ 
    fn <- formatC(i, width = 3, flag = '0')
    fn <- paste(fn, ".csv", sep = "")
    monitor <- read.csv(fn)    
    cc <- complete.cases(monitor$sulfate, monitor$nitrate)
    ccsum <- sum(cc)          
    if(ccsum > threshold){     
      par <- monitor[complete.cases(monitor$sulfate, monitor$nitrate),]
      x <- cor(par[2],par[3])      
      v <- append(v,x[[1]])
    }    
  }  
  if(length(v) > 0){
    v    
  }else{      
    re <- numeric()
    re
  }   
}
