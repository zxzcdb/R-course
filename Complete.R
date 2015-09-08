## Write a function that reads a directory full of files and reports the number of 
## completely observed cases in each data file. 
## The function should return a data frame where the first column is the name of the file 
## and the second column is the number of complete cases.

complete <- function(directory, id = 1:332) {
  setwd("~/Documents/rviascott")
  path <- file.path(getwd(),directory)
  setwd(path)
  df <- data.frame(x = numeric(), y = numeric())  
  r <- 1
  for (i in id){ 
    fn <- formatC(i, width = 3, flag = '0')
    fn <- paste(fn, ".csv", sep = "")
    monitor <- read.csv(fn)
    cc <- sum(complete.cases(monitor$sulfate, monitor$nitrate))
    df <- rbind(df, c(i, cc))
    r <- r + 1
  }
  setwd("~/Documents/rviascott")
  colnames(df) <- c("id","nobs")
  df
}
