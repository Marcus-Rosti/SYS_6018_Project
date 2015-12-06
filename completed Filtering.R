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

#### Table showing whether the user has visited the restaurants 
visits<-visits[order(visits$user_id,visits$business_id),]#135742
length(unique(visits$user_id))#45867
length(unique(visits$business_id)) #2653

# Subset 1
v1<-visits[1:50225,]
length(unique(v1$user_id))#5417
temp1<-matrix(0,ncol=2653,nrow=5417,dimnames=list(unique(v1$user_id),unique(visits$business_id)))
t1<-as.matrix(v1)
temp1[t1[,1:2]]<-1

# Subset 2
visits[75000:75199,]
v2<-visits[50226:75199,]
length(unique(v2$user_id))#5501
temp2<-matrix(0,ncol=2653,nrow=5501,dimnames=list(unique(v2$user_id),unique(visits$business_id)))
t2<-as.matrix(v2)
temp2[t2[,1:2]]<-1

# Subset 3
visits[90000:90048,]
v3<-visits[75200:90048,]
length(unique(v3$user_id))#5110
temp3<-matrix(0,ncol=2653,nrow=5110,dimnames=list(unique(v3$user_id),unique(visits$business_id)))
t3<-as.matrix(v3)
temp3[t3[,1:2]]<-1


# Subset 4
visits[100500:100544,]
v4<-visits[90049:100544,]
length(unique(v4$user_id))#5237
temp4<-matrix(0,ncol=2653,nrow=5237,dimnames=list(unique(v4$user_id),unique(visits$business_id)))
t4<-as.matrix(v4)
temp4[t4[,1:2]]<-1

# Subset 5
visits[110000:110042,]
v5<-visits[100545:110042,]
length(unique(v5$user_id))#5090
temp5<-matrix(0,ncol=2653,nrow=5090,dimnames=list(unique(v5$user_id),unique(visits$business_id)))
t5<-as.matrix(v5)
temp5[t5[,1:2]]<-1

# Subset 6
visits[120000:120044,]
v6<-visits[110043:120044,]
length(unique(v6$user_id))#6572
temp6<-matrix(0,ncol=2653,nrow=6572,dimnames=list(unique(v6$user_id),unique(visits$business_id)))
t6<-as.matrix(v6)
temp6[t6[,1:2]]<-1

# Subset 7
v7<-visits[120045:125742,]
length(unique(v7$user_id))#4187
temp7<-matrix(0,ncol=2653,nrow=4187,dimnames=list(unique(v7$user_id),unique(visits$business_id)))
t7<-as.matrix(v7)
temp7[t7[,1:2]]<-1

# Subset 8
v8<-visits[125743:135742,]
length(unique(v8$user_id))#8753
temp8<-matrix(0,ncol=2653,nrow=8753,dimnames=list(unique(v8$user_id),unique(visits$business_id)))
t8<-as.matrix(v8)
temp8[t8[,1:2]]<-1

