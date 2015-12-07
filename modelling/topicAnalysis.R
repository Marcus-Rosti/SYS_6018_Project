load(file="../data/rdata/reviews_clean.RData")

library(sqldf)
library(stringr)
library(tm)

restaurant_documents <- list()
users_documents <- list()

for(i in 1:nrow(reviews_clean)){
  print(i)
  rest <- as.character(reviews_clean$business_id[i])
  user <- as.character(reviews_clean$user_id[i])
  text <- str_replace(as.character(reviews_clean$text[i]),"\\n","")
  if(rest %in% names(restaurant_documents)){
    restaurant_documents[[rest]] <- paste(restaurant_documents[[rest]],text)
  } else{
    restaurant_documents[[rest]] <- text
  }
  if(user %in% names(users_documents)){
    users_documents[[user]] <- paste(users_documents[[user]],text)
  } else{
    users_documents[[user]] <- text
  }
}

rest_vec <- unlist(restaurant_documents)
user_vec <- unlist(users_documents)
corpus = VCorpus(DataframeSource(data.frame(txt=c(rest_vec,user_vec))))
save(corpus,file="../data/rdata/review_corpus.RData")
