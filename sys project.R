#business

#Delete irrelevant or unnecessary variables
drops<-c('full_address',"state","city","attributes.Accepts.Insurance")
business_clean<-business_clean[,!(names(business_clean) %in% drops)]
business_clean<-business_clean[,-grep("attributes.Hair.Types.Specialized.In", names(business_clean))]
str(business_clean)
#Transform variable "categories" to factor 1/0

business_clean$categories[which(business_clean$categories=="Restaurants")]<-1
business_clean$categories[which(is.na(business_clean$categories)==T)]<-0
business_clean$categories<-as.factor(business_clean$categories)

#Change other categoreis to factors
business_clean$categories1<-as.factor(business_clean$categories1)
business_clean$categories2<-as.factor(business_clean$categories2)
business_clean$categories3<-as.factor(business_clean$categories3)
business_clean$categories4<-as.factor(business_clean$categories4)
business_clean$categories5<-as.factor(business_clean$categories5)
business_clean$categories6<-as.factor(business_clean$categories6)
business_clean$categories7<-as.factor(business_clean$categories7)
business_clean$categories8<-as.factor(business_clean$categories8)
business_clean$categories9<-as.factor(business_clean$categories9)
business_clean$categories10<-as.factor(business_clean$categories10)

##Normalize neighborhoods into 1/0 factors
####All neightborhoods variables only have NAs 

##Delete variables that have more than 2600 NAs (total observations:2653)
nrow(business_clean)#2653 in total

nafind<-function(x){sum(is.na(x))}
nacount<-apply(business_clean,2,"nafind")
nacount
varWna <- which(nacount>2600) #23 variables that have more than 2600 NAs
business_clean<-business_clean[,-varWna] #82 variables in the data set
