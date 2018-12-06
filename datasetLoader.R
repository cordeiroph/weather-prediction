getDataSet <- function(){
  df <- read.csv("Assignment 2.csv")
  df$No <- NULL 
  return(df)
}

getNonNullDataSet <- function(){
  df <- read.csv("Assignment 2.csv")
  df$No <- NULL 
  return(na.omit(df))
}

getDataSet2 <- function(){
  df <- read.csv("Assignment 2.csv")
  df$No <- NULL 
  df$cbwd <- NULL
  return(na.omit(df))
}