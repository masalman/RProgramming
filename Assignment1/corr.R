corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  #create a file list
  f.list <- as.character(list.files(directory))
  #create a storage container
  storage <- numeric(0)
  hold <- 0
  #create file paths from file list
  f.paths <- paste(directory, "/", f.list, sep = "")
  #find the files
  subset.obs <- subset(complete(directory), nobs > threshold)[,1]
  
  for(i in subset.obs)
  {
     #read file
     index.file <- read.csv(f.paths[i])
     #isolate complete cases
     index.file <- index.file[complete.cases(index.file),]
     #run cor function
     hold <- cor(index.file$sulfate, index.file$nitrate, use = "pairwise.complete.obs")
     #store result
     storage <- c(storage, hold)
  }
  #return results
  return(storage)
}