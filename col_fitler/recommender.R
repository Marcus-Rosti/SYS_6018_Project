# Marcus Rosti
# Based on https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf
#setwd("~/Git/SYS_6018_Project/col_fitler/")
source("../data_parsing/preprocessing.R")
usb <- user_review_of_business_df()

predict_rating <- function(uid,bid) {
  require(recommenderlab)
  # all the reviews of the business
  try({
    business_reviews <- subset(usb,usb$business_id == bid)

    # all the target users reviews
    target_user_reviews <- subset(usb,usb$user_id == uid)

    # all reviews for reviewers of business reviewers
    all_reviews <-
      subset(usb,usb$user_id %in% business_reviews$user_id)

    rel_reviews = rbind(all_reviews,target_user_reviews)

    rel_reviews = subset(rel_reviews, rel_reviews$business_id != bid)

    correlated_reviews <-
      subset(rel_reviews,rel_reviews$business_id %in% target_user_reviews$business_id)


    user_ids <- unique(correlated_reviews$user_id)
    bus_ids <- c(unique(correlated_reviews$business_id),bid)

    rel.mat <-
      matrix(data = rep(NA,length(user_ids) * length(bus_ids)),
             ncol = length(bus_ids))

    dimnames(rel.mat) <- list(user = user_ids, business = bus_ids)

    # build ratings matrix
    for (i in 1:length(user_ids)) {
      for (j in 1:length(bus_ids)) {
        rel.mat[i,j] = round(mean(usb$stars[usb$user_id == user_ids[i] &
                                              usb$business_id == bus_ids[j]]))
      }
    }

    # build recommender
    r <- as(rel.mat, "realRatingMatrix")
    to.predict = r[which(row.names(rel.mat) == uid)]
    predictor = r[-which(row.names(rel.mat) == uid)]
    recommender <- Recommender(predictor, method = "UBCF")
    recom <- predict(recommender,to.predict, type = "ratings")
    final <- as(recom, "matrix")

    score = final[,which(colnames(rel.mat) == bid)][[1]]

    if (is.numeric(score)&&!is.na(score))
      return(score)
    return(mean(usb$stars[usb$business_id == bid]))
  })
  return(mean(usb$stars))
}

