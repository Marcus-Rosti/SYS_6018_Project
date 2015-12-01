#install.packages("jsonlite")
#install.packages("sqldf")
#install.packages("rjson")
#install.packages("plyr")

library(jsonlite)
library(sqldf)
library(rjson)
library(plyr)
library(parallel)

business <- data.frame()
checkin <- data.frame()
review <- data.frame()
tip <- data.frame()
user <- data.frame()

business_json_file <- "../data/json/yelp_academic_dataset_business.json"
checkin_json_file  <- "../data/json/yelp_academic_dataset_checkin.json"
review_json_file <- "../data/json/yelp_academic_dataset_reviews.json"
tip_json_file <- "../data/json/yelp_academic_dataset_tip.json"
user_json_file <- "../data/json/yelp_academic_dataset_user.json"


rbind.fill.parallel <- function(list, numCores){
  while(length(list) > 1) {
    idxlst <- seq(from=1, to=length(list), by=2)
    
    list <- mclapply(idxlst, function(i) {
      print(i)
      if(i==length(list)) { return(list[[i]]) }
      return(rbind.fill(list[[i]], list[[i+1]]))
    },mc.cores = getOption("mc.cores", numCores))
    gc()
  }
  return(list)
}

createFlatFiles <- function(){
  
  #businesses
  data <- paste(readLines(business_json_file),collapse=",")
  data <- fromJSON(paste("{\"dat\":[",data,"]}"))
  data <- lapply(data$dat, function(x){ unlist(x)}) 
  data <- (mclapply((1:length(data)), 
                    function(x) {print(x);do.call("data.frame", as.list(data[[x]]))}, mc.cores = getOption("mc.cores", 2L) ))
  gc()
  business <- rbind.fill.parallel(data,2L)[[1]]
  save(business,file="../data/rdata/business_raw_flatfile.RData")
  gc()
  
  
  #reviews 
  parts <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p')
  for(part in parts){
    print(paste("Part =",part))
    file <- paste0(review_json_file,"_part_a",part)
    data <- paste(readLines(file),collapse=",")
    data <- fromJSON(paste("{\"dat\":[",data,"]}"))
    data <- lapply(data$dat, function(x){ unlist(x)}) 
    data <- (mclapply((1:length(data)), 
                      function(x) {print(x);do.call("data.frame", as.list(data[[x]]))}, mc.cores = getOption("mc.cores", 2L) ))
    gc()
    reviews <- rbind.fill.parallel(data,2L)[[1]]
    save(reviews,file=paste0("../data/rdata/reviews_raw_flatfile_part_",part,".RData"))
    rm(data,file,reviews)
  }
  
  x <- parts[1]
  load(file=paste0("../data/rdata/reviews_raw_flatfile_part_",x,".RData"))
  review_data <-  reviews
  for(i in 2:length(parts)){
    x <- parts[i]
    load(file = paste0("../data/rdata/reviews_raw_flatfile_part_",x,".RData"))
    review_data <- rbind.fill(review_data,reviews)
  }
  reviews <- review_data
  save(reviews,file="../data/rdata/reviews_raw_flatfile.RData")
  
  
  #users
  data <- paste(readLines(user_json_file),collapse=",")
  data <- fromJSON(paste("{\"dat\":[",data,"]}"))
  data <- lapply(data$dat, function(x){ unlist(x)}) 
  data <- (mclapply((1:length(data)), 
                    function(x) {
                        print(x)
                        df <- do.call("data.frame", as.list(data[[x]]))
                        df <- df[,names(df)[!(grepl(x=names(df),pattern = ".*friends.*"))]]  
                        return(df)
                      }, mc.cores = getOption("mc.cores", 2L) ))
  gc()
  users1 <- rbind.fill.parallel(data[1:100000],2L)[[1]]
  save(users1,file="../data/rdata/users1_raw_flatfile.RData")
  gc()
  users2 <- rbind.fill.parallel(data[100001:200000],2L)[[1]]
  save(users2,file="../data/rdata/users2_raw_flatfile.RData")
  gc()
  users3 <- rbind.fill.parallel(data[200001:300000],2L)
  save(users3,file="../data/rdata/users3_raw_flatfile.RData")
  gc()
  users4 <- rbind.fill.parallel(data[300000:length(data)],2L)[[1]]
  save(users4,file="../data/rdata/users4_raw_flatfile.RData")
  gc()
  
  users <- rbind.fill(users1,users2,users3,users4)
  save(users,file="../data/rdata/users_raw_flatfile.RData")
  gc()

  #check-in
  data <- paste(readLines(checkin_json_file),collapse=",")
  data <- fromJSON(paste("{\"dat\":[",data,"]}"))
  data <- lapply(data$dat, function(x){ unlist(x)}) 
  data <- (mclapply((1:length(data)), 
                    function(x) {print(x);do.call("data.frame", as.list(data[[x]]))}, mc.cores = getOption("mc.cores", 2L) ))
  gc()
  checkin <- rbind.fill.parallel(data,2L)[[1]]
  save(checkin,file="../data/rdata/checkin_raw_flatfile.RData")
  gc()
  
  #tip
  parts <- c('a','b','c','d','e')
  for(part in parts){
    print(paste("Part =",part))
    file <- paste0(tip_json_file,"_part_a",part)
    data <- paste(readLines(file),collapse=",")
    data <- fromJSON(paste("{\"dat\":[",data,"]}"))
    data <- lapply(data$dat, function(x){ unlist(x)}) 
    data <- (mclapply((1:length(data)), 
                      function(x) {print(x);do.call("data.frame", as.list(data[[x]]))}, mc.cores = getOption("mc.cores", 2L) ))
    gc()
    tips <- rbind.fill.parallel(data,2L)[[1]]
    save(tips,file=paste0("../data/rdata/tips_raw_flatfile_part_",part,".RData"))
    rm(data,file,tips)
  }
  
  x <- parts[1]
  load(file=paste0("../data/rdata/tips_raw_flatfile_part_",x,".RData"))
  tip_data <-  tips
  for(i in 2:length(parts)){
    x <- parts[i]
    load(file = paste0("../data/rdata/tips_raw_flatfile_part_",x,".RData"))
    tip_data <- rbind.fill(tip_data,tips)
  }
  tips <- tip_data
  save(tips,file="../data/rdata/tips_raw_flatfile.RData")
}