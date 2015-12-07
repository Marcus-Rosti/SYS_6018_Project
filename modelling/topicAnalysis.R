load(file="../data/rdata/reviews_clean.RData")

library(sqldf)
library(topicmodels)
library(stringr)
library(tm)
library(SnowballC)

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

# clean and compute tfidf
corpus.clean = tm_map(corpus, stripWhitespace)                          # remove extra whitespace
corpus.clean = tm_map(corpus.clean, removeNumbers)                      # remove numbers
corpus.clean = tm_map(corpus.clean, removePunctuation)                  # remove punctuation
corpus.clean = tm_map(corpus.clean, content_transformer(tolower))       # ignore case
corpus.clean = tm_map(corpus.clean, removeWords, stopwords("english"))
corpus.clean = tm_map(corpus.clean, stemDocument)                       # stem all words

corpus.clean.tf = DocumentTermMatrix(corpus.clean, control = list(weighting = weightTf))

save(corpus,file="../data/rdata/review_corpus.RData")
save(corpus.clean,file="../data/rdata/review_corpus_clean.RData")
save(corpus.clean.tf,file="../data/rdata/review_corpus_tf.RData")

# remove empty documents
row.sums = apply(corpus.clean.tf, 1, sum)
corpus = corpus[row.sums > 0]
corpus.clean.tf = corpus.clean.tf[row.sums > 0,]
hgvcbbn 

= finalDocumentDF[row.sums>0,]

set.seed(1)
topic.model = LDA(corpus.clean.tf, 5)
