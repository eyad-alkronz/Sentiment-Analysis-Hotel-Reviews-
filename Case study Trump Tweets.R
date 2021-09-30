library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

library(dslabs)
data("trump_tweets")
head(trump_tweets)
names(trump_tweets)
trump_tweets %>% select(text) %>% head

#and the source variable tells us the device that was used to compose and upload each tweet:
trump_tweets %>% count(source) %>% arrange(desc(n))

#We can use extract to remove the Twitter for part of the source and filter out retweets.

trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source)



#We are interested in what happened during the campaign, 
#so for the analysis here we will focus on what was tweeted between the day Trump announced
#his campaign and election day. So we define the following table:


campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)



#We can now use data visualization to explore the possibility that two different groups were
#tweeting from these devices. For each tweet, we will extract the hour, in the east coast (EST), 
#it was tweeted then compute the proportion of tweets tweeted at each hour for each device.
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
 # scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")


##Text as data
#The tidytext package helps us convert free from text into a tidy table.
#Having the data in this format greatly facilitates data visualization 
#and applying statistical techniques.


library(tidytext)
#The main function needed to achieve this is unnest_tokens(). 
#A token refers to the units that we are considering to be a data point.
#The most common tokens will be words, but they can also be single characters


#The functions will take a vector of strings and extract the tokens so that each one gets a row in the new table. Here is a simple example:
example <- data_frame(line = c("A","B" , "C", "D" ),
                      text = c("Roses are red,", "Violets are blue,",
                               "Sugar is sweet,", "And so are you." 
                                ))
example
example %>% unnest_tokens(word, text)

#Now let's look at a quick example with a tweet number 3008:

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)


campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#Now we are ready to extract the words for all our tweets.
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 


#And we can now answer questions such as "what are the most commonly used words?"
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))


#It is not surprising that these are the top words. The top words are not informative.
#The tidytext package has database of these commonly used words, referred to as stop words,
#in text mining:
stop_words


#If we filter out rows representing stop words with filter(!word %in% stop_words$word):
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word )


#And we can now answer questions such as "what are the most commonly used words?"
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))  
  

#We end up with a much more informative set of top 10 tweeted words:

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

#final edits
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

#For each word we want to know if it is more likely to come from an Android tweet or an iPhone tweet.
#We previously introduced the odds ratio, a summary statistic useful for quantifying these differences


android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)




android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)



#Sentiment Analysis
#1_ the bing lexicon divides words into positive and negative.
get_sentiments("bing")


#2 The AFINN lexicon assigns a score between -5 and 5, with -5 the most negative and 5 the most positive.
get_sentiments("afinn")


#The loughran and nrc lexicons provide several different sentiments:

get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)



#For the analysis here we are interested in exploring the different sentiments of each tweet,
#so we will use the nrc lexicon:
nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)


#We can combine the words and sentiments using inner_join(), which will only keep words associated with a sentiment. 
#Here are 10 random words extracted from the tweets:
tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)



sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts


#Because more words were used on the Android than on the phone:
tweet_words %>% group_by(source) %>% summarize(n = n())

sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or  


log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip()


android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 






