require(ROCR)
require(pROC)
require(sqldf)

load("../data/rdata/business_clean.RData")
load("../data/rdata/users_clean.RData")
load("../data/rdata/reviews_clean.RData")

users <- users_clean

users$topicDist <- rep(NA,nrow(users_clean))
users$locationMetricHalfMile <- rep(NA,nrow(users_clean))
users$locationMetricOneMile <- rep(NA,nrow(users_clean))
users$predictedStars <- rep(NA,nrow(users_clean))
users$visited <- rep(0,nrow(users_clean))

#drop unrequired columns
drop <- c("yelping_since","name","type")
users <- users[,setdiff(names(users),drop)]

#remove NAs from compliments columns
users$compliments.profile <- as.numeric(ifelse(is.na(users$compliments.profile),0,users$compliments.profile))
users$compliments.cute <- as.numeric(ifelse(is.na(users$compliments.cute),0,users$compliments.cute))
users$compliments.funny <- as.numeric(ifelse(is.na(users$compliments.funny),0,users$compliments.funny))
users$compliments.plain <- as.numeric(ifelse(is.na(users$compliments.plain),0,users$compliments.plain))
users$compliments.writer <- as.numeric(ifelse(is.na(users$compliments.writer),0,users$compliments.writer))
users$compliments.note <- as.numeric(ifelse(is.na(users$compliments.note),0,users$compliments.note))
users$compliments.photos <- as.numeric(ifelse(is.na(users$compliments.photos),0,users$compliments.photos))
users$compliments.hot <- as.numeric(ifelse(is.na(users$compliments.hot),0,users$compliments.hot))
users$compliments.cool <- as.numeric(ifelse(is.na(users$compliments.cool),0,users$compliments.cool))
users$compliments.more <- as.numeric(ifelse(is.na(users$compliments.more),0,users$compliments.more))
users$compliments.list <- as.numeric(ifelse(is.na(users$compliments.list),0,users$compliments.list))

## restaurant of interest: "Flavors of Louisiana" : mwiNm868yAo8Xh8hO7Ke_Q
## populate visited
users_who_visited <- sqldf("SELECT DISTINCT user_id 
                            FROM reviews_clean
                            WHERE business_id = 'mwiNm868yAo8Xh8hO7Ke_Q'")
for(u in users_who_visited$user_id){
  users$visited[users$user_id==u] = 1
}

## populate topicDist
load("../data/rdata/topic_distances.RData")
restTopicDiff <- topicDiff[,"mwiNm868yAo8Xh8hO7Ke_Q"]
users$topicDist <- ifelse(is.na(restTopicDiff[users$user_id]),1,restTopicDiff[users$user_id])


## populate locationMetricHalfMile
load("../data/rdata/locationMetricHalfMile.RData")
users$locationMetricHalfMile <- locationMetricHalfMile[users$user_id]

## populate locationMetricOneMile
load("../data/rdata/locationMetricOneMile.RData")
users$locationMetricOneMile <- locationMetricOneMile[users$user_id]

## populate predictedStars
source("../col_fitler/recommender.R")
x <- sapply(users$user_id,predict_rating,bid="mwiNm868yAo8Xh8hO7Ke_Q")
x <- ifelse(x>5,5,x)
x <- ifelse(x<0,0,x)
users$predictedStars <- x

# change variable type
users$votes.funny  <- as.numeric(users$votes.funny)
users$votes.useful  <- as.numeric(users$votes.useful)
users$votes.cool <- as.numeric(users$votes.cool)
users$review_count <- as.numeric(users$review_count)
users$fans <- as.numeric(users$fans)
users$average_stars <- as.numeric(users$average_stars)
users$topicDist <- as.numeric(users$topicDist)
users$locationMetricHalfMile <- as.numeric(users$locationMetricHalfMile)
users$locationMetricOneMile <- as.numeric(users$locationMetricOneMile)
users$predictedStars <- as.numeric(users$predictedStars)
users$visited <- as.factor(users$visited)


###
# Remove Outliers
#plot(users[,c(1:4,23)])
#plot(users[,c(6:10,23)])
#plot(users[,c(11:15,23)])
#plot(users[,c(16:22,23)])

###
# Split into train-test
set.seed(2313523)

indices <- sample(1:nrow(users),round(0.8*nrow(users)))
train.data <- users[indices,]
test.data <-  users[-indices,]

###
# Logistic Model
# Model 1
model1 <- glm(visited~. ,data=train.data[,c(1:4,6:22,23)],family="binomial")
summary(model1)

#ROC Curve
predictions <- predict(model1,newdata=test.data,type="response")
pred <- ROCR::prediction(predictions,test.data$visited)
perf <- ROCR::performance(pred,measure="tpr",x.measure="fpr")
perf.AUC <- ROCR::performance(pred,measure="auc")
perf.AUC
plot(perf)
title("ROC Curve for predictions, AUC =0.9458")

#evaluation
F1score <- function(model,test.data,response,threshold=0.5){
  predictions <- predict(model,newdata=test.data,type="response")
  predictions <- ifelse(predictions>=threshold,1,0)
  actuals <- test.data[,response]
  TN = sum(actuals==0 & predictions==0)
  FP = sum(actuals==0 & predictions==1)
  FN = sum(actuals==1 & predictions==0)
  TP = sum(actuals==1 & predictions==1)
  Pre = TP/(TP+FP)
  Rec = TP/(TP+FN)
  F1 = 2*((Pre*Rec)/(Pre+Rec))
  return(F1)
}

F1score(model1,test.data,"visited")
# 0.659

#Model 2
model2 <- glm(visited~topicDist+locationMetricOneMile+predictedStars,data=train.data,family="binomial")
summary(model2)
F1score(model2,test.data,"visited")
#0.6516

#ROC Curve
predictions <- predict(model2,newdata=test.data,type="response")
pred <- ROCR::prediction(predictions,test.data$visited)
perf <- ROCR::performance(pred,measure="tpr",x.measure="fpr")
perf.AUC <- ROCR::performance(pred,measure="auc")
perf.AUC
plot(perf,col="red")
title("ROC Curve for predictions, AUC =0.9469")

#ANOVA
anova(model1,model2, test='Chisq')

################

# Model 3
model3 <- glm(visited~topicDist+locationMetricOneMile+predictedStars+review_count+fans+average_stars,data=train.data,family="binomial")
summary(model3)
F1score(model3,test.data,"visited")
#0.64

#ROC Curve
predictions <- predict(model3,newdata=test.data,type="response")
pred <- ROCR::prediction(predictions,test.data$visited)
perf <- ROCR::performance(pred,measure="tpr",x.measure="fpr")
perf.AUC <- ROCR::performance(pred,measure="auc")
perf.AUC
plot(perf)
title("ROC Curve for predictions, AUC =0.9438")


#FINAL MODEL: Model 2 seems best
################################################

