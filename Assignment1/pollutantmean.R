pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  #create a file list
  f.list <- as.character(list.files(directory))
  #create a storage container
  storage <- c()
  #create file paths from file list
  f.paths <- paste(directory, "/", f.list, sep = "")
  #find the files
  for (i in id){
     index.file <- read.csv(f.paths[i],header = T)
     head(index.file)
     pollutant
     #remove NA
     clear.na <- index.file[!is.na(index.file[, pollutant]), pollutant]
     #save in storage
     storage <- c(storage, clear.na)
  }
  #return in requested format
  round(mean(storage),3)
}