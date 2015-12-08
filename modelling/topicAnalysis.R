load(file="../data/rdata/reviews_clean.RData")

require(sqldf)
require(topicmodels)
require(stringr)
require(tm)
require(SnowballC)
require(parallel)
require(ggplot2)

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
corpus.clean = tm_map(corpus.clean, removeWords, stopwords("english"))  # remove stop words english
corpus.clean = tm_map(corpus.clean, stemDocument)                       # stem all words
#some common words which kept appearing in all topics
corpus.clean = tm_map(corpus.clean, removeWords, c("food","place","good","like","time","just","get","great","tri","servic","order",
                                                   "restaur","love","will","back","one","realli"))

corpus.clean.tf = DocumentTermMatrix(corpus.clean, control = list(weighting = weightTf))


# remove empty documents
row.sums = apply(corpus.clean.tf, 1, sum)
corpus.clean = corpus.clean[row.sums > 0]
corpus.clean.tf = corpus.clean.tf[row.sums > 0,]


save(corpus.clean.tf,file="../data/rdata/review_corpus_tf.RData")


# keep track of which docs were removed in the original rest_vec and user_vec
row.sums.restvec <- row.sums[1:length(rest_vec)]
row.sums.uservec <- row.sums[length(rest_vec)+1:length(user_vec)]
rest_vec <- rest_vec[row.sums.restvec>0]
user_vec <- user_vec[row.sums.uservec>0]

restaurant_documents_sub <- restaurant_documents[row.sums.restvec>0] 
users_documents_sub <- users_documents[row.sums.uservec>0]

#rename documents in the tf matrix
corpus.clean.tf$dimnames[[1]] <- c(names(restaurant_documents_sub),names(users_documents_sub))

#Choose number of topics
#t1a <- Sys.time()
# Evaluate models with k from 5 to 10
#chooseK <- mclapply(seq(5,50, by=5), function(k){print(k);LDA(corpus.clean.tf, k)},mc.cores = getOption("mc.cores", 2L) )
#t2a <- Sys.time()
#timea <- t2a-t1a
#timea

# Extract log likelihood from each  model
#chooseK.logLik <- as.data.frame(as.matrix(lapply(chooseK, logLik)))
# And prepare to plot
#chooseK.logLik.df <- data.frame(topics=seq(5,50, by=5), LL=as.numeric(as.matrix(chooseK.logLik)))
# And plot
#ggplot(chooseK.logLik.df, aes(x=topics, y=LL)) + 
#  xlab("Number of topics") + ylab("Log likelihood of the model") + 
#  geom_line() 

#Train Topic model on 10 models
set.seed(1)
topic.model = LDA(corpus.clean.tf, 10)

save(topic.model,file="../data/rdata/topic_model_10.RData")


# look at the top 20 words within the topics
terms(topic.model, 20)
# look at the topics within the documents
topics(topic.model,10)[,1:117]
