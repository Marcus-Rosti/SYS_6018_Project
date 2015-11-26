#install.packages("jsonlite")
#install.packages("sqldf")
#install.packages("rjson")
#install.packages("plyr")

library(jsonlite)
library(sqldf)
library(rjson)
library(plyr)

business <- data.frame()
checkin <- data.frame()
review <- data.frame()
tip <- data.frame()
user <- data.frame()

business_json_file <- "../data/json/yelp_academic_dataset_business.json"
checkin_json_file  <- "../data/json/yelp_academic_dataset_checkin.json"
review_json_file <- "../data/json/yelp_academic_dataset_review.json"
tip_json_file <- "../data/json/yelp_academic_dataset_tip.json"
user_json_file <- "../data/json/yelp_academic_dataset_user.json"


convertJSONtoFlatStructure <- function(filepath){
  json <- paste(readLines(filepath),collapse=",")
  json <- fromJSON(paste("{\"dat\":[",json,"]}"))
  json.i <- lapply(json$dat, function(x){ unlist(x)}) 
  data <- (lapply((1:length(json.i)), 
               function(x) {print(x);do.call("data.frame", as.list(json.i[[x]]))} ))
  data2 <- data.frame()
  for(i in 1:length(data)){
    print(i)
    data2 <- rbind.fill(data2,data[[i]])
  }
  return(data2)
}

createFlatFiles <- function(){
  
  business <- convertJSONtoFlatStructure(business_json_file)
  save(business,file="../data/rdata/flatfile raw dump/business_raw_flatfile.R")
  
  #checkin <- convertJSONtoFlatStructure(checkin_json_file)
  #save(checkin,file="../data/rdata/flatfile raw dump/checkin_raw_flatfile.R")
  
  review <- convertJSONtoFlatStructure(review_json_file)
  save(review,file="../data/rdata/flatfile raw dump/review_raw_flatfile.R")
  
  
  #tip <- convertJSONtoFlatStructure(tip_json_file)
  #save(tip,file="../data/rdata/flatfile raw dump/tip_raw_flatfile.R")
  
  
  user <- convertJSONtoFlatStructure(user_json_file)
  save(user,file="../data/rdata/flatfile raw dump/user_raw_flatfile.R")

}