###############################################################################################
#This was an exercise in text analytics and sentiment analyss. 2000 "#Columbine" tweets 
#were pulled from Twitter on the 19th anniversary of the Columbine High School Massacre. 

#Load the following libraries
library(twitteR)
library(qdap)
library(tm)
library(SnowballC)
library(stringr)
library(rtweet)
library(wordcloud)
library(RWeka)
library(tibble)
library(ggplot2)
library(tidytext)
library(gutenbergr)
library(dplyr)
library(tidyr)
library(radarchart)

setwd("C:/Users/Christie/Desktop/Kidambi/Text Analytics")

#Personalized keys are acquired by creating a Twitter account and going to https://apps.twitter.com/ 

consumer_key <- "Enter Key Here"
consumer_secret <- "Enter Key Here"
access_token <- "Enter Key Here"
access_secret <- "Enter Key Here"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#This will pull up to 2000 of the most recent tweets tagged with "#Columbine"
tw = twitteR::searchTwitter("#Columbine", n = 2000, retryOnRateLimit = 1e3)

#Converts the list of objects from a single twitteR class to a dataframe and then assigns that to a new variable called "mytext"
d = twitteR::twListToDF(tw)
mytext <- d

#Write to a csv, as Twitter limits the number of tweets you can pull within a certain time frame.
#This prevents new tweets from being scraped each time the code is run
#write.csv(mytext, file="columbine.csv")

#Converts the text type from UTF-8 to ASCII. This is necessary for data cleaning
mytext$text <- iconv(mytext$text, from = "UTF-8", to = "ASCII", sub = "")

#This creates a corpus containing only the text from the list of twitteR objects
columbine_corpus <- VCorpus(VectorSource(mytext$text))
print(columbine_corpus)

#This is a function that will clean all of the text within the corpus. Any URLs, 
#punctuation, numbers, stopwords (both built-in and customized), and whitespace
#will be  removed removed. All capital letters will be transformed to lower case, and 
#abbreviated words will be replaced with their proper names.
clean_corpus <- function(cleaned_corpus){
  removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
  cleaned_corpus <- tm_map(cleaned_corpus, removeURL)
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(replace_abbreviation))
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  custom_stop_words <- c("#Columbine", "columbine", "rt")
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  return(cleaned_corpus)
}

cleaned_col_corpus <- clean_corpus(columbine_corpus)

#This compares the tweets from the original corpus to the tweets in the cleaned one
print(columbine_corpus[[16]][1])
print(cleaned_col_corpus[[16]][1])  

#A term document matrix is then created with the the cleaned corpus, where each tweet is a column and each row is a word.
TDM_tweets <- TermDocumentMatrix(cleaned_col_corpus)
TDM_tweets_m <- as.matrix(TDM_tweets)
TDM_tweets_m[415:420, 11:16]

# The frequency of words across the entire corpus is calculated, and then three different
#word clouds are calculated: unigram, bigram, and trigram. 
term_frequency <- rowSums(TDM_tweets_m)
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,max.words=100,colors=brewer.pal(8, "Paired"))

#This tokenizer function identifies the n-gram used to create word cloud 
tokenizer <- function(x){
  NGramTokenizer(x,Weka_control(min=2,max=2))} #when "min" and "max" are set to "2", it creates a bigram
bigram_tdm <- TermDocumentMatrix(cleaned_col_corpus,control = list(tokenize=tokenizer))
bigram_tdm_m <- as.matrix(bigram_tdm)
term_frequency <- rowSums(bigram_tdm_m)
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)

#Creates a word cloud from most frequent bigrams amongst the 2000 tweets
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,max.words=100,colors=brewer.pal(8, "Paired"))


tokenizer <- function(x){
  NGramTokenizer(x,Weka_control(min=3,max=3))} #when "min" and "max" are set to "3", it creates a trigram
trigram_tdm <- TermDocumentMatrix(cleaned_col_corpus,control = list(tokenize=tokenizer))
trigram_tdm_m <- as.matrix(trigram_tdm)
term_frequency <- rowSums(trigram_tdm_m)
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)

#Creates a word cloud from most frequent trigrams amongst the 2000 tweets
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,max.words=50,colors=brewer.pal(8, "Paired"))

#The following lines create a TD-IDF matrix from the original cleaned corpus, which applies a weighting scheme to all words.
#A word cloud is then created using this matrix.
tfidf_tdm <- TermDocumentMatrix(cleaned_col_corpus,control=list(weighting=weightTfIdf))
tfidf_tdm_m <- as.matrix(tfidf_tdm)
term_frequency <- rowSums(tfidf_tdm_m)
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,max.words=100,colors=brewer.pal(8, "Paired"))

#The next objective is to do a sentiment analysis using polarity scoring. 

setwd("C:/Users/Christie/Desktop/Kidambi/Text Analytics/Final")

#The original tweets that were previously written to a CSV are read in. 
library(qdap)
library(tm)

twitter<-read.csv("columbine.csv", header=TRUE, sep=",")

#This function will apply polarity scores to desired input. Polarity
#identifies the following categories of words: positive, negative, 
#negation, amplification, and deamplification. 

functionPol<-function(x){
  Pol<-polarity(x)
  Result<-Pol$all$polarity
  return (Result)
}

#The tweets are then passed to this function and are
#each given polarity scores
Pol<-lapply(twitter$text, functionPol)
twitter$polarity<-sapply(Pol, paste0)

#The following two lines split the tweets into two categories- positive and 
#negative- based on their polarity scores. Any tweet with a polarity score
#greater than 0 is tagged as "Positive", and any tweet with a polarity 
#score of less than 0 is tagged as "Negative"
positive<-twitter[twitter$polarity>0,]
negative<-twitter[twitter$polarity<0,]

#These objects are then converted to vectors, then combined into one list,
#and finally converted to a corpus.
pos_text<-as.vector(positive$text)
neg_text<-as.vector(negative$text)
new_list<-list(pos_text, neg_text)
final_corpus<-VCorpus(VectorSource(new_list))

#The same cleaning function previously used is then applied to this new
#corpus containing positive and negative tweets
clean_corpus <- function(cleaned_corpus){
  removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
  cleaned_corpus <- tm_map(cleaned_corpus, removeURL)
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(replace_abbreviation))
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  custom_stop_words <- c("#Columbine", "columbine", "rt")
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  return(cleaned_corpus)
}

cleaned_final_corpus <- clean_corpus(final_corpus)

#A term document matrix is then created, where each word is a row
#and the column values represent the frequency of that word used in
#tweets tagged as either positive or negative
TDM_twitter <- TermDocumentMatrix(cleaned_final_corpus)
colnames(TDM_twitter) <- c("Positive", "Negative")
TDM_twitter_m <- as.matrix(TDM_twitter)

#A comparison cloud is then created, to compare the frequent usage of words 
#in the positve and negative reviews
comparison.cloud(TDM_twitter_m,colors=brewer.pal(8, "Dark2"),max.words = 200)
#A commonality cloud is created to identify the words that both types of reviews have in common.
commonality.cloud(TDM_twitter_m,colors=brewer.pal(8, "Dark2"),max.words = 200)

#Lastly, the NRC lexicon is used to perform an emotional analysis on the original 2000 tweet corpus
nrc_lex <- get_sentiments("nrc")
TDM_tweets <- TermDocumentMatrix(cleaned_col_corpus)
tweets_tidy <- tidy(TDM_tweets)
story_nrc <- inner_join(tweets_tidy, nrc_lex, by = c("term" = "word"))
story_nrc_noposneg <- story_nrc[!(story_nrc$sentiment %in% c("positive","negative")),]
aggdata <- aggregate(story_nrc_noposneg$count, list(index = story_nrc_noposneg$sentiment), sum)
chartJSRadar(aggdata)



