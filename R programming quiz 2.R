setwd("~/Documents/coursera data science")
unzip("rprog_data_specdata.zip")
setwd("~/Documents/coursera data science/specdata")
read.csv("200.csv")


# Part 1

# steps:
  # 1. read the files all the data files
  # 2. merge the data files in one data frame
  # 3. ignore the NAs
  # 4. subset the data frame by pollutant
  # 5. calculate the mean.

pollutantmean <- function(directory, pollutant, id = 1:332) {
  # directiory is a character vector of length 1 indicating the location of the 
  # CSV files
  filesfull <- list.files("~/Documents/coursera data science/specdata", 
                          full.names = TRUE)
  # list.files(): These functions produce a character vector of the names of files 
  # or directories in the named directory.
  
  dat <- data.frame()   # create an empty data frame
  
  for (i in id){      # loop through the list of files until id is found
    temp <- read.csv(filesfull[i], header = TRUE)   # read in the file
    dat <- rbind (dat, temp)   # add files to the main data frame
  }
  
  mean(dat[,pollutant], na.rm = TRUE)

}


# Part 2

# steps:

  # 1. read in the files
  # 2. remove the NAs from the set
  # 3. count the number of rows
  # 4. create a new data set  has two columns that contains the monitors id number and
     # the number of observations

complete <- function(directory,id=1:332){
  filesfull <- list.files("~/Documents/coursera data science/specdata", 
                          full.names = TRUE)
  dat <- data.frame()
  
  for (i in id){
    temp <- read.csv(filesfull[i], header = TRUE)
    temp <- na.omit(temp)   # delete rows that do not have complete cases
    Nobs <- nrow(temp)   #count all of the rows with complete cases
    tmp <- data.frame(i, Nobs)
    dat<-rbind(dat, tmp)

  }
  
  dat
  
}
    

# Part 3

# steps:

  # 1. read in the files
  # 2. remove the NAs from the set
  # 3. check to see if the number of complete cases are > then threshold
  # 4. find  the correlation between different types of pollutants.

corr<-function(directory,threshold=0){
  filesfull <- list.files("~/Documents/coursera data science/specdata", 
                          full.names = TRUE)
  dat <- vector(mode = "numeric", length = 0)   # #create empty vector
  
  for (i in 1:length(filesfull)){
    temp <- read.csv(filesfull[i], header = TRUE)
    temp <- na.omit(temp)
    sum <- nrow(temp) 
    
    if (sum  > threshold){
      dat<-c(dat,cor(temp$nitrate,temp$sulfate))
    }
  }
  
   dat
}






