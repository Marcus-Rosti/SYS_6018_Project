load("../data/rdata/business_clean.RData")
load("../data/rdata/users_clean.RData")
load("../data/rdata/reviews_clean.RData")

users <- users_clean

users$topicDist <- rep(NA,nrow(users_clean))
users$locationMetric <- rep(NA,nrow(users_clean))
users$predictedStars <- rep(NA,nrow(users_clean))
users$visited <- rep(0,nrow(users_clean))

drop <- c("yelping_since","name","type")
users <- users[,setdiff(names(users),drop)]

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


## populate locationMetric


## populate predictedStars


###
# Split into train-test
set.seed(2313523)

indices <- sample(1:nrow(users),round(0.8*nrow(users)))
train.data <- users[indices,]
test.data <-  users[-indices,]

###
# Logistic Model / or / SVM