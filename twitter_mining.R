# https://cran.r-project.org/web/views/NaturalLanguageProcessing.html

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

# playing with twitter
location_name = "Delhi" 
location_info = availableTrendLocations()

# checking if location exists in the available locations
location_name %in% location_info$name
  
# get location index
location_index = which(location_info$name == location_name)
woeid = location_info[location_index,3] # woeid located in 3rd column

trends_delhi = getTrends(woeid)
str(trends_delhi)
head(trends_delhi$name, 25)

# doing the tweet analysis parts
# can expand the language range, specify spatial coordinates to restrict tweets to certain geographic regions, etc

# tells us the upper limit on pulling different types of data from twitter. Eg: /search/tweets is 180 which means 180 requests (1 request = 1 query of size n) per unit time (every 15 minutes). https://dev.twitter.com/rest/public/rate-limiting
twitteR::getCurRateLimitInfo()
twitteR::getCurRateLimitInfo("search")

# some useful functions
?searchTwitter
?load_tweets_db
?register_sqlite_backend
?store_tweets_db
?strip_retweets
?search_twitter_and_store

# getting the data
register_sqlite_backend("dengue_tweets_db")

# pull data from twitter and store into db
# https://dev.twitter.com/rest/public/search
#dengue_raw = search_twitter_and_store("dengue+Dengue", table_name = "dengue_tweets_db", lang = "en")

# load data from the db
dengue_tweets_db = load_tweets_db(table_name = "dengue_tweets_db") %>% twListToDF()

dengue_tweets = dengue_tweets_db$text
head(dengue_tweets)

# function below throws errors
# use check_text from qdap package
check_text(dengue_tweets[1:4])
# check encoding
?Encoding
table(Encoding(dengue_tweets)) # unknown
# https://stackoverflow.com/questions/8274972/official-encoding-used-by-twitter-streaming-api-is-it-utf-8

# prototype code + debugging
temp = dengue_tweets[1:2]; enc2utf8(temp)
# need to use apply to set encoding for each document (ie row of dengue_tweets)
Encoding(enc2utf8(temp))
clean_tweets(VCorpus(VectorSource(temp))) # assume clean_tweets is loaded first

# fixing encoding issues since it works for one tweet but not more
temp = dengue_tweets[1:5]
length(temp)
for (i in 1:length(temp)) {
  temp[i] = enc2utf8(temp[i])
  if (Encoding(temp[i]) == "unknown") { temp[i] = iconv(temp[i], to = "ASCII")}
}
Encoding(temp)

# nope, still now working. What am I doing with my life?
# http://www.textasdata.com/2015/02/encoding-headaches-emoticons-and-rs-handling-of-utf-816/
# talks about R encoding issues (voice in my ear telling me to use python)
# go with iconv for the time being
# using the solution listed on the blog above
Encoding(iconv(temp[2], from = "ASCII", to = "UTF-8", sub = ""))
# fuck, still doesn't work
# throw out the stupid tweets that don't get converted

dengue_tweets_temp = enc2utf8(dengue_tweets)
table(Encoding(dengue_tweets_temp)) 
# not promising, maybe will have to replicate this in python
# for the time being, stick to tweets with UTF-8 since this is proof of concept anyway

########
######## some curiosity led to me to use chardet
######## assuming python is installed on the linux system
######## pip install chardet
######## then run "chardet name_of_file"
# some stuff
foo = temp[2]; save(foo, file = "weird_file.RData")
getwd()
# so final command was: (in the terminal)
# chardet "/home/ad/Desktop/KUL Course Material/Text Based Information Retrieval/datacamp text mining slides/Text mining project/weird_file.RData"
# 
# with the output:
# 
#  KOI8-R (confidence: 0.39)
# that was an analysis of simply one tweet, we could do this for all
# for sanity's sake, this is not done

str(dengue_tweets[which(Encoding(dengue_tweets_temp) == "UTF-8")])

dengue_tweets_utf8 = dengue_tweets[which(Encoding(dengue_tweets_temp) == "UTF-8")]

## tried running again, still errors because emoji
## so must clean those damn emojis first
check_text(dengue_tweets_utf8)
dengue_tweets_utf8[32]
temp = dengue_tweets_utf8[32]

# find regex for emojis
# https://stackoverflow.com/questions/24672834/how-do-i-remove-emoji-from-string

removeEmoji = function(vec) {
  # define emoji unicode or unicode for other related symbols
  # https://stackoverflow.com/questions/24672834/how-do-i-remove-emoji-from-string
  # emoticons
  vec = gsub(pattern = "[\U0001F600-\U0001F64F]", "", x = vec)
  # symbols and pics
  vec = gsub(pattern = "[\U0001F300-\U0001F5FF]", "", x = vec)
  # dingbats
  vec = gsub(pattern = "[\U00027000-\U00027BFF]", "", x = vec)
  return(vec)
}

temp
removeEmoji(temp)

dengue_tweets_utf8[32]
foo = iconv(c(dengue_tweets_utf8[32], dengue_tweets_utf8[32]), "ASCII", "UTF-8", sub = "") # seems to accomplish what we need but encoding is no longer preserved

Encoding(foo) <- c("UTF-8", "UTF-8")
clean_tweets(VCorpus(VectorSource(foo))) # works without throwing errors

dengue_tweets_utf8 = iconv(dengue_tweets_utf8, "ASCII", "UTF-8", sub = "")

# making the corpus only on those with type "UTF-8"
dengue_corpus = VCorpus(VectorSource(dengue_tweets_utf8))

# on all the tweets
dengue_corpus = dengue_tweets %>% 
  iconv(., "ASCII", "UTF-8", sub = "") %>% 
  VectorSource() %>% VCorpus()

# processing the tweets
clean_tweets = function(corpus) {
  # removeEmoji has to be defined
  corpus = tm_map(corpus, content_transformer(removeEmoji))
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords, c("Dengue", "dengue"))
  # removing most common english stop words
  # stopwords("en")
  corpus = tm_map(corpus, removeWords, stopwords("en"))
  # stemming the document (processing the words to have the same root)
  corpus = tm_map(corpus, stemDocument)
  return(corpus)
}

# processing the tweets
dengue_corpus_clean = clean_tweets(dengue_corpus)

dengue_corpus_clean

# creating the matrix with tweets as rows and terms as columns
dengue_dtm = DocumentTermMatrix(dengue_corpus_clean)
dengue_dtm
inspect(dengue_dtm[1:5,1:10]) 
# seem to be lots of arbitrary numbers, so remove them and rerun

# top n words
freq = dengue_dtm %>% as.matrix() %>% 
  colSums() %>% sort(., decreasing = TRUE) 
freq %>% head(50)

# create word cloud
dengue_dtm_sparse = removeSparseTerms(dengue_dtm, 0.992)
dengue_dtm_sparse
freq = dengue_dtm_sparse %>% as.matrix() %>% 
  colSums() %>% sort(., decreasing = TRUE) 
freq %>% head(50)

wordcloud(names(freq), freq, max.words = 150)

# different colours
RColorBrewer::brewer.pal.info
RColorBrewer::display.brewer.all()
pal1 = brewer.pal(9, "GnBu")[-(1:3)]
wordcloud(names(freq), freq, max.words = 150, colors = pal1)

pal2 = brewer.pal(9, "OrRd")[-(1:3)]
wordcloud(names(freq), freq, max.words = 30, colors = pal2)

pal3 = brewer.pal(8, "Dark2")
wordcloud(names(freq), freq, max.words = 50, colors = pal3)

## trying word clouds with 2- and 3-grams
## use RWeka::NGramTokenizer
bigram_token = function(x) RWeka::NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram_token = function(x) RWeka::NGramTokenizer(x, Weka_control(min = 3, max = 3))
options(mc.cores = 1)
bigram_tdm = TermDocumentMatrix(dengue_corpus_clean, 
                                control = list(tokenize = bigram_token))
#save(bigram_tdm, file = "bigram_tdm.RData")

trigram_tdm = TermDocumentMatrix(dengue_corpus_clean, 
                                control = list(tokenize = trigram_token))
#save(trigram_tdm, file = "trigram_tdm.RData")

tdm_matrix = as.matrix(bigram_tdm)
bigram_freq = rowSums(tdm_matrix) %>% sort(., decreasing = TRUE)
bigram_freq %>% head(5)

findFreqTerms(bigram_tdm, lowfreq = 50)

wordcloud(names(bigram_freq), bigram_freq, max.words = 20)

# top n phrases
trigram_tdm %>% as.matrix() %>% rowSums() %>% sort(., decreasing = TRUE) %>% head(10)
# wordcloud
trigram_tdm %>% as.matrix() %>% rowSums() %>% sort(., decreasing = TRUE) %>%
  wordcloud(names(.), ., max.words = 5)

# weighting terms using TF-IDF and forming word cloud
uni_tdm_tfidf = TermDocumentMatrix(dengue_corpus_clean, control = list(weighting = weightTfIdf))

uni_tdm_tfidf_freq = uni_tdm_tfidf %>% as.matrix() %>% 
  rowSums() %>% sort(., decreasing = TRUE) 

# with TFIDF
uni_tdm_tfidf_freq %>% head(10)

# only TF
freq %>% head(10)

# bar chart
data.frame(name = names(uni_tdm_tfidf_freq), freq = uni_tdm_tfidf_freq) %>%
  dplyr::slice(1:n_words) %>%
  ggplot(data = ., aes(x = reorder(name, freq), y = freq)) + 
  geom_bar(stat = "identity") + theme_bw() + coord_flip() + 
  ylab("Word Count") + xlab("Term")

# unigram wordcloud with weighting
par(mfrow = c(1,2))
wordcloud(names(freq), freq, max.words = 50, col = pal3, scale = c(1,2))
title("Weight: TF")
uni_tdm_tfidf_freq %>% wordcloud(names(.), ., max.words = 50, col = pal3, scale = c(1,2))
title(main = "Weight: TFIDF")

# graphing tweets
library(graphTweets)
help("graphTweets")
str(dengue_tweets_db)

# cleaning the first column
to_utf8 = function(x) {iconv(x, "ASCII", "UTF-8", sub = "")}
dengue_tweets_db_clean = data.frame(dengue_tweets_db[-1], 
                                    lapply(dengue_tweets_db[1], to_utf8)) %>%
  dplyr::mutate(text = as.character(text))

str(dengue_tweets_db_clean)  

edges = getEdges(data = dengue_tweets_db_clean[1:50,], tweets = "text", source = "screenName")

graph1 = igraph::graph.data.frame(edges, directed = TRUE)
plot(graph1)

nodes = getNodes(edges, source = "source", target = "target", "retweetCount")
