complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  #create a directory file list
  f.list <- as.character(list.files(directory))
  #create a storage container
  storage <- c()
  #create file paths from file list
  f.paths <- paste( directory, "/", f.list, sep = "")
  #find the files
  for (i in id){
    index.file <- read.csv(f.paths[i])
    #save in storage
    storage <- c(storage, nrow(na.omit(index.file)))
  }
  #return in requested format
  data.frame(id = id, nobs = storage)
}