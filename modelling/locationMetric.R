load("../data/rdata/business_clean.RData")
load("../data/rdata/users_clean.RData")
load("../data/rdata/reviews_clean.RData")

require(sqldf)
require(Imap)
require(parallel)

users <- users_clean$user_id
rests <- business_clean$business_id

radius <- 0.5
active_restaurant <- "mwiNm868yAo8Xh8hO7Ke_Q"

lon.1 <- as.numeric(as.character(business_clean$longitude[business_clean$business_id=="mwiNm868yAo8Xh8hO7Ke_Q"]))
lat.1 <- as.numeric(as.character(business_clean$latitude[business_clean$business_id=="mwiNm868yAo8Xh8hO7Ke_Q"]))

near_by_restaurants <- c()   
for(rest in rests){
  lon.2 <- as.numeric(as.character(business_clean$longitude[business_clean$business_id==rest]))
  lat.2 <- as.numeric(as.character(business_clean$latitude[business_clean$business_id==rest]))
  dist <- gdist(lon.1, lat.1, lon.2, lat.2, units = "miles", a = 6378137.0, b = 6356752.3142, verbose = FALSE)
  if(dist<radius)
    near_by_restaurants <- c(near_by_restaurants,rest)
}
near_by_restaurants <- setdiff(near_by_restaurants,c("mwiNm868yAo8Xh8hO7Ke_Q"))
near_by_restaurants_sql <- paste(paste0("'",near_by_restaurants,"'"),collapse=",")
reviews_near_by <- sqldf(paste0("SELECT user_id FROM reviews_clean WHERE business_id IN (",near_by_restaurants_sql,")"))
users_near_by_count <- sqldf("SELECT user_id, count(*) as count FROM reviews_near_by GROUP BY user_id")

locationMetric <- matrix(,nrow=length(users),ncol=1)
dimnames(locationMetric)[[1]] <- users
dimnames(locationMetric)[[2]] <- "mwiNm868yAo8Xh8hO7Ke_Q"

calcLocationMetric <- function(i){
  print(i)
  u <- users[i]
  if(u %in% users_near_by_count$user_id){
    n <- users_near_by_count$count[users_near_by_count$user_id==u]
  }
  else{
    n <- 0
  }
  locationMetric[u,1] <- n/length(near_by_restaurants)
}
locMet <- sapply(1:length(users),calcLocationMetric)
locationMetric[,1] <- locMet
locationMetricHalfMile <- locationMetric[,1] 
save(locationMetricHalfMile,file="../data/rdata/locationMetricHalfMile.RData")

####################
radius <- 1
active_restaurant <- "mwiNm868yAo8Xh8hO7Ke_Q"

lon.1 <- as.numeric(as.character(business_clean$longitude[business_clean$business_id=="mwiNm868yAo8Xh8hO7Ke_Q"]))
lat.1 <- as.numeric(as.character(business_clean$latitude[business_clean$business_id=="mwiNm868yAo8Xh8hO7Ke_Q"]))

near_by_restaurants <- c()   
for(rest in rests){
  lon.2 <- as.numeric(as.character(business_clean$longitude[business_clean$business_id==rest]))
  lat.2 <- as.numeric(as.character(business_clean$latitude[business_clean$business_id==rest]))
  dist <- gdist(lon.1, lat.1, lon.2, lat.2, units = "miles", a = 6378137.0, b = 6356752.3142, verbose = FALSE)
  if(dist<radius)
    near_by_restaurants <- c(near_by_restaurants,rest)
}
near_by_restaurants <- setdiff(near_by_restaurants,c("mwiNm868yAo8Xh8hO7Ke_Q"))
near_by_restaurants_sql <- paste(paste0("'",near_by_restaurants,"'"),collapse=",")
reviews_near_by <- sqldf(paste0("SELECT user_id FROM reviews_clean WHERE business_id IN (",near_by_restaurants_sql,")"))
users_near_by_count <- sqldf("SELECT user_id, count(*) as count FROM reviews_near_by GROUP BY user_id")

locationMetric <- matrix(,nrow=length(users),ncol=1)
dimnames(locationMetric)[[1]] <- users
dimnames(locationMetric)[[2]] <- "mwiNm868yAo8Xh8hO7Ke_Q"

calcLocationMetric <- function(i){
  print(i)
  u <- users[i]
  if(u %in% users_near_by_count$user_id){
    n <- users_near_by_count$count[users_near_by_count$user_id==u]
  }
  else{
    n <- 0
  }
  locationMetric[u,1] <- n/length(near_by_restaurants)
}
locMet <- sapply(1:length(users),calcLocationMetric)
locationMetric[,1] <- locMet
locationMetricOneMile <- locationMetric[,1] 
save(locationMetricOneMile,file="../data/rdata/locationMetricOneMile.RData")

