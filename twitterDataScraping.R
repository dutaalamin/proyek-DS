library(rtweet) #Untuk mencari tweet
library(twitteR) #untuk autentikasi ke twitter

#install.packages("devtools") untuk memperbaiki error pada library rtweet
#devtools::install_github("mkearney/rtweet") 

##------------------Pengambilan data------------------##

#mengirimkan parameter autentikasi yang dibutuhkan 
setup_twitter_oauth(consumer_key = "IM0B6X2N1rYCOP7PmjahJXY5m",
                    consumer_secret = "N9MpJxggVPfIi6CTh6lHwyAmh4aypkDESPEtsAn1d70W1fcUoo",
                    access_token = "1020303028769906688-k8qcHgifPgMPoPqwsRFWdNUoCqmPcp",
                    access_secret = "znCQLuvNZ3JZQuzar86b98tAPSh8A2Fu5Qa0LjOI1SPmQ") 

#Autentikasi ke halaman twitter secara default
auth_setup_default() 
travel_tweets <- search_tweets("pandemic+travelling -filter:retweets", n=500, lang="en")
write.csv(travel_tweets,file = 'tweet_df.csv')

##------------------Pembersihan data------------------##

#library yang digunakan untuk pembersihan data
library(dplyr)
library(tidyr)
library(tidytext)
library(textdata)
library(purrr)
library(tm)

#mengubah data menjadi corpus
travellingTweets <- travel_tweets$text
travel_corpus <- Corpus(VectorSource(travellingTweets))

#membersihkan data dari elemen yang tidak penting
tweet_clean <- tm_map(travel_corpus, removePunctuation) #menghilangkan tandabaca
tweet_clean <- tm_map(tweet_clean, content_transformer(tolower)) #menghilangkan kapital
tweet_clean <- tm_map(tweet_clean, removeNumbers) #menghilangkan angka
tweet_clean <- tm_map(tweet_clean, stripWhitespace) #menghilangkan white space
removeURL <- function(removeURL) gsub("http[^[:space:]]*", "", removeURL)
tweet_clean <- tm_map(tweet_clean, removeURL) #menghilangkan url
removeEmoticon <- function(removeEmoticon) gsub("[^\x01-\x7F]", "", removeEmoticon)
tweet_clean <- tm_map(tweet_clean, removeEmoticon) #menghilangkan emoticon
setwd('D:\\Praktikum Data Science\\Final Project')
stopwords = readLines("stopwords.txt")
tweet_clean <- tm_map(tweet_clean,removeWords,stopwords) #menghilangkan stopwords

#Menyimpan hasil pembersihan ke df baru
tweet_clean_df<-data.frame(text=unlist(sapply(tweet_clean, `[`)), stringsAsFactors=F)
#melihat hasil
View(tweet_clean_df)

#mengubah df menjadi file csv
write.csv(tweet_clean_df,file = 'tweetclean_df.csv')