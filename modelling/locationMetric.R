load("../data/rdata/business_clean.RData")
load("../data/rdata/users_clean.RData")
load("../data/rdata/reviews_clean.RData")
library(sqldf)

dist.prop <- function(points.1, thresh = 100){
  dist.prop <- vector(mode = "numeric", length = nrow(points.1))
  for (i in 1:nrow(points.1)){
    lon.1 <- points.1[i,1]
    lat.1 <- points.1[i,2]
    dist <- vector(mode = "numeric", length = nrow(points.1))
    for (j in 1:nrow(points.1)){
      lon.2 <- points.1[j,1]
      lat.2 <- points.1[j,2]
      dist[j] <- gdist(lon.1, lat.1, lon.2, lat.2, units = "miles", a = 6378137.0, b = 6356752.3142, verbose = FALSE)
    }
    dist.prop[i] <- length(which(dist <= thresh))/nrow(points.1)
  }
  return(dist.prop)
}

users <- users_clean$user_id
rests <- business_clean$business_id

locationMetric <- matrix(,nrow=length(users),ncol=1)
dimnames(topicDiff)[[1]] <- users
dimnames(topicDiff)[[2]] <- "mwiNm868yAo8Xh8hO7Ke_Q"

radius <- 1

active_restaurant <- "mwiNm868yAo8Xh8hO7Ke_Q"
for(i in 1:nrow())
dist.prop()


for(i in users){
    
}