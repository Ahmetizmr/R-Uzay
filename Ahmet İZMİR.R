install.packages("twitteR")
install.packages("ROAuth")
install.packages("tm")
install.packages("RCurl")
install.packages("magrittr")
install.packages("dplyr") 
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("funModeling")
install.packages("lubridate")
install.packages("stringi")
install.packages("stringr")
install.packages("readxl")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("xlsx")

library(csvread)
library(xlsx)
library(lubridate)
library(twitteR)
library(tm)
library(ROAuth)
library(RCurl)
library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(funModeling)
library(stringi)
library(stringr)
library(tm)
library(tidytext)
library(dplyr)
library(ggplot2)

api_key = ""
api_secret = ""
acces_token = ""
acces_token_secret = ""
setup_twitter_oauth(api_key,api_secret,acces_token,acces_token_secret)

tweets <- searchTwitteR("#uzay", n=1000,  locale = "tr_TR",lang="tr")
tweets.df <- twListToDF(tweets)
tweet_clean <- tweets.df
tweet_clean$text <- stri_enc_toutf8(tweet_clean$text)
view(tweet_clean$text)
tweet_clean$text <- ifelse(str_sub(tweet_clean$text,1,2) == "RT",
                           substring(tweet_clean$text,3),
                           tweet_clean$text)
view(tweet_clean$text)

tweet_clean$text <- ifelse(str_sub(tweet_clean$text,1,2) == "RT",
                           substring(tweet_clean$text,3),
                           tweet_clean$text)
view(tweet_clean$text)

tweet_clean$text <- str_replace_all(tweet_clean$text, "http[^[:space:]]*", "")



tweet_clean$text <- str_replace_all(tweet_clean$text, "#\\S+", "")
tweet_clean$text <- str_replace_all(tweet_clean$text, "@\\S+", "")


tweet_clean$text <- str_replace_all(tweet_clean$text, "[[:punct:][:blank:]]+", " ")


tweet_clean$text  <- str_to_lower(tweet_clean$text, "tr")


tweet_clean$text <- removeNumbers(tweet_clean$text)
#stopwordsler
liste=c(stopwords("en")"ve","da","az","de","ý","t","s","fetö","darbe","value","tag","it","rt","retweet","th","now","one","x","enter","can
        ","today","ýt","amp","ýts","race","shýbg","ready","early","survives","know","follow","get","h",
        "hec","mc","ýf","day","help","pool","us","go","big","date","end","hesita","sins","deadly","akýta",
        "soon","friends","read","will","referral","join","anm","community","project","prize","map",
        "entry","new","hard","don","global","erbakanyeniden","takipçimize","gereken","olmasý")


tweet_clean$text = removeWords(tweet_clean$text,liste)


tweet_clean$text <- str_replace_all(tweet_clean$text, "[<].*[>]", "")
tweet_clean$text <- gsub("\uFFFD", "", tweet_clean$text, fixed =  TRUE)
tweet_clean$text <- gsub("\n", "", tweet_clean$text, fixed =  TRUE)


tweet_clean$text <- str_replace_all(tweet_clean$text, "[^[:alnum:]]"," " )

library(tidytext)
library(dplyr)
library(ggplot2)

tidy_tweets <- tweet_clean %>% select(text) %>% 
  mutate(linenumber = row_number()) %>% unnest_tokens(word, text)



head(tidy_tweets)

tidy_tweets %>%
  count(word, sort = TRUE) %>%
  filter(n > 13) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + theme_minimal() +
  ggtitle("Tweetlerde en çok kullanýlan kelimeler")

library(xlsx)
library(wordcloud)




tidy_tweets %>% 
  
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

write.csv(tweet_clean$text,file='')
write.csv(tidy_tweets, file='')

doc.corpus <- Corpus(VectorSource(tweets.df$text))
doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus, content_transformer(removePunctuation))
doc.corpus <- tm_map(doc.corpus,content_transformer(removeNumbers))

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(doc.corpus, removeURL)
myCorpus <- tm_map(myCorpus, stripWhitespace)#bosluklarin
tdm <- TermDocumentMatrix(myCorpus)
findFreqTerms(tdm, lowfreq = 15)
