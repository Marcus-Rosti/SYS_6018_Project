
load("../data/rdata/business_clean.RData")
load("../data/rdata/users_clean.RData")

require(Imap)

points.1 <- business_clean[complete.cases(business_clean[,c("longitude", "latitude")]),c("longitude", "latitude")]
points.1 <- points.1[1:10,]
points.1$longitude <- as.numeric(as.character(points.1$longitude))
points.1$latitude <- as.numeric(as.character(points.1$latitude))

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

test <- dist.prop(points.1, thresh = 0.5)
