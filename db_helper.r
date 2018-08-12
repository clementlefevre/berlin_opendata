library(RSQLite)
library(cycleRtools)
library(tidyverse)
library(lubridate)


DB.NAME <- 'zwift.sqlite'

FIT.FILES.TMP.DIR <- 'tmp/'


connect_to_db <- function(){
  tryCatch(
    {
      con <- dbConnect(RSQLite::SQLite(), DB.NAME)
      return(con)
    },
    error=function(cond) {
      message(paste("could open DB connection", filename))
      return(NULL)
    }
  )
}

read_df_from_db <- function(){
  con <- connect_to_db()
  
  tryCatch(
    {
      df <- (dbReadTable(con,'logs'))
      dbDisconnect(con)
      df$timestamp <- as.POSIXct(df$timestamp,  origin="1970-01-01", tz = 'CET')
      df$date <- as.Date(df$timestamp)
      return (df)
    },
    error=function(cond) {
      message(paste("Database Table log does not seem to exist:"))
     
      return(NULL)
    }
  ) 
  
 
}

get.data <- function(){
  
  df.fit.data <- read_df_from_db()
  
  print(max(df.fit.data$date))
  # Add column age of records in day
  df.fit.data$age.days <- as.Date(Sys.Date())-  as.Date(df.fit.data$timestamp)
  df.fit.data <- df.fit.data[order(df.fit.data$timestamp),]
  df.fit.data <- tibble::rowid_to_column(df.fit.data, "ID")
  df.fit.data$date <-ymd(as.Date(df.fit.data$timestamp))
  df.fit.data <- df.fit.data[order(df.fit.data$timestamp),]
  
  df.fit.data <- df.fit.data %>% arrange(date,timestamp)
  df.fit.data <- df.fit.data %>% group_by(activity.ID) %>% mutate(mean.power = mean(power.W,na.rm=TRUE))
  return (df.fit.data)
  
}

check_activity_exists<- function(activity.ID){
  con <- connect_to_db()
  tryCatch(
    { res <-dbGetQuery(con, paste0('SELECT count(1) FROM logs WHERE "activity.ID" = ',activity.ID))
    dbDisconnect(con)
    
    return(res>0)
     
    },
    error=function(cond) {
      message(paste("Database Table log does not seem to exist:"))
      
      return(FALSE)
    }
  ) 
 
}


insert_df_to_db <- function(df){
  con <- dbConnect(RSQLite::SQLite(), DB.NAME)
  dbWriteTable(con, "logs", df,append=TRUE)
  dbDisconnect(con)
}

read.ride.with.file.name<- function(filename){
  
  tryCatch(
    {
      print(paste0('try read fit file: ',filename))
      df <- read_ride(filename)
      return(df)
    },
    error=function(cond) {
      message(paste("File does not seem to exist:", filename))
      message("Here's the original error message:")
      return(NULL)
    }
  ) 
}


insert.new.fit.files <- function(activity.ID,df.activities){
  
  if (!check_activity_exists(activity.ID)){
    download.fit.file.and.save.to.db(activity.ID, df.activities)
  } 
}

download.fit.file.and.save.to.db <- function(activity.ID, df.activities){
  fit.file.url <- (df.activities[df.activities$id==activity.ID,])$download.fit.url
 
  
  ## create folder is not exists
  ifelse(!dir.exists(file.path(getwd(),FIT.FILES.TMP.DIR )), dir.create(file.path(getwd(), FIT.FILES.TMP.DIR)), FALSE)
  tmp.fit.file <- paste0(activity.ID,'.fit')
  
  download.file(url = fit.file.url,destfile = paste0(FIT.FILES.TMP.DIR,tmp.fit.file), method = 'libcurl')
  
  df <- read.ride.with.file.name(paste0(FIT.FILES.TMP.DIR,tmp.fit.file))
  df$activity.ID <- activity.ID
  df$timestamp <- as.POSIXct(df$timestamp,  origin="1970-01-01", tz = 'CET')
  df$date <- as.Date(df$timestamp)

  df <- df[order(df$timestamp),]

  insert_df_to_db(data.frame(df))
  file.remove( paste0(FIT.FILES.TMP.DIR,tmp.fit.file))

}

connect.to.zwift.and.update.db <- function(username,password){
  df.activities <- get.df.activities(username,password)
  print('hey----------------')
  lapply(df.activities[,'id'], insert.new.fit.files, df.activities=df.activities)
}




