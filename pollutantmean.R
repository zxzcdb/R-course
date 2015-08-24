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
