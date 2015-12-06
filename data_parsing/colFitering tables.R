load(file="reviews_clean.RData")
library(plyr)

keep<-c("user_id","business_id")
reviews_clean2<-reviews_clean[,(names(reviews_clean) %in% keep)]
visits<-unique(reviews_clean2)

combi<- function (i) {
  as.data.frame(t(combn(i[[2]],2,simplify=T)))
} 

# delete all rows that corresponds to users who only went to one restaurant
test<-ddply(visits,.(user_id),nrow)
only<-test[which(test$V1<=1),]
com.predat<-visits[-which(visits$user_id %in% only$user_id),]

# create all combinations among all the restaurants one user went to
com.dat<-ddply(com.predat,.(user_id),combi) 

#### business * business table (does not include total user visits for each of the businesses)
com.dat2<-com.dat[,2:3]
length(unique(com.predat$business_id))#2640 businesses in the records
bus.mat<-matrix(NA,ncol=2640,nrow=2640,dimnames=list(unique(com.predat$business_id),unique(com.predat$business_id)))
#2640*2640---7038409 elements
com.mtx<-as.matrix(table(com.dat2)) # 6698850 elements
#fill in the values
cols <- colnames(bus.mat)[colnames(bus.mat) %in% colnames(com.mtx)]
rows <- rownames(bus.mat)[rownames(bus.mat) %in% rownames(com.mtx)]
bus.mat[rows, cols] <- com.mtx[rows, cols]

####total visits for each business 
bus.dat<-as.data.frame(table(visits$business_id))

#### Table showing whether the user has visited the restaurants (NOT DONE YET)

#size is too big; need to cut down the size
tt<-xtabs(~user_id+business_id,data=visits))

