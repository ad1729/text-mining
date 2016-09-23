library(tm)
library(magrittr)
library(qdap)
library(wordcloud)
library(plotrix)
library(RWeka)
library(RColorBrewer)
library(SnowballC)
library(stringi)
#https://cran.r-project.org/web/packages/stringi/stringi.pdf

source("twitter_oauth.R")

register_sqlite_backend("eating_insect_db")

dengue_raw = search_twitter_and_store("dengue+Dengue", table_name = "dengue_tweets_db", lang = "en")

# load data from the db
dengue_tweets_db = load_tweets_db(table_name = "dengue_tweets_db") %>% twListToDF()

dengue_tweets = dengue_tweets_db$text
head(dengue_tweets)

??stringi
