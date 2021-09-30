
library(dslabs)
library(tidyverse)    # includes readr
library(readxl)
library(lubridate)
library(tidytext)
library(tidyverse)  

filename <- "tripadvisor_hotel_reviews.csv"
# check if the file exists
file.exists(filename)

# read file in CSV format
data <- read_csv(filename , col_names = TRUE)


temp<- data.frame(id = seq(1,count(data)%>%pull()))

data <- cbind(data, temp)  


#groub by rate
data %>%  
  ggplot(aes(x= Rating ))+
  geom_bar()  
 
Review_words <- data %>% 
 # mutate(Review = str_replace_all(data$Review, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, Review )  %>%  #  , token = "regex", pattern = pattern
filter(!word %in% stop_words$word   &
            !str_detect(word, "^\\d+$"))  #%>%
  #mutate(word = str_replace(word, "^'", ""))


#calculate world count for each statment

#plot rating vs wordCount
SentenceWithWordCount <- Review_words %>% group_by(id) %>%
  summarize( WordCount = n()) 

data <- data %>% left_join(SentenceWithWordCount  , by="id")

data%>% group_by(Rating) %>% 
  summarize(meanCount = mean(WordCount)) %>%
  ggplot(aes(x = Rating , y = meanCount   ))+
  geom_point() +
    geom_smooth(se = TRUE,
                na.rm = FALSE)
#Insight
#Higher Rated Reviews tend to have less words while, lower rated reviews have very high word count


 
#Sentiment Analysis
#In sentiment analysis we assign a word to one or more "sentiment". 

#to show words count by sentiments that offers by afinn
get_sentiments("afinn")%>%count(value)


#to convert value from [-5,5] range to [0,5] to be more suitable for out anlysis
afinn <- get_sentiments("afinn") %>%
  select(word, value)%>%
  mutate(value = ifelse(value == 5 , 5 , value) )   %>%
  mutate(value = ifelse(value == 4 , 5 , value) )  %>% 
  mutate(value = ifelse(value == 3 , 4 , value) )  %>% 
  mutate(value = ifelse(value == 2 , 4 , value) )  %>% 
  mutate(value = ifelse(value == 1 , 3 , value) )  %>% 
 
  mutate(value = ifelse(value == -5 , 0 , value) )  %>% 
  mutate(value = ifelse(value == -4 , 0 , value) )  %>% 
  mutate(value = ifelse(value == -3 , 1 , value) )  %>% 
  mutate(value = ifelse(value == -2 , 2 , value) )  %>% 
  mutate(value = ifelse(value == -1 , 2 , value) ) 

 
#New form of afinn
afinn%>%count(value)

# sentiment_counts_for_rating <- Review_words %>%
#   left_join(afinn, by = "word") %>%
#   count(Rating, value) %>%
#   spread(Rating, n) %>%
#   mutate(value = replace_na(value, replace = "none"))
# sentiment_counts_for_rating


#assign value for each words to its value that offered by afinn
Review_words <- Review_words %>%
  left_join(afinn, by = "word")  
 
#Now , at this moment there are alot of words without value 
#because afinn does not have value for all words 

 
#i want to replace all NA values to average of statment values for each statment 
Review_words <- Review_words %>% group_by(id) %>%
 mutate(value = ifelse(is.na(value), mean(value , na.rm = TRUE),value  ))


Review_words <-  Review_words %>%
  mutate(value = ifelse(is.nan(value), 2.5,value  ))
 
#Now we want to calculate value for each
# result = average of values for all words in this sentence
predicted_rating_per_sentences <-  Review_words%>%
  group_by(id) %>% 
  summarize(result = (mean(value)))


#plot histogram for predicted_rating_per_sentences
predicted_rating_per_sentences %>% ggplot(aes(result)) +
  geom_histogram()

#join predicted_rating_per_sentences with original Data 
# calculate differ for enhancement pupose 
# diff value must be near to zero 
data <- predicted_rating_per_sentences %>% left_join(data ,by = "id")%>%
  mutate(differ = ceiling(result) - Rating ) 

#plot histogram to show differ 
data %>% ggplot(aes(abs(differ))) +
  geom_histogram()


data %>% ggplot(aes(Rating)) +
  geom_histogram()

data %>% ggplot(aes(result)) +
  geom_histogram()



#calculate teh mean of error 
data %>% mutate(differ = abs(differ)) %>% 
   group_by(Rating)%>%
   summarize(meanDiffer = mean(differ , na.rm = TRUE))


#percent of 100% prediction 

data%>% group_by(differ) %>%
  summarize(percent = n()/count(data)) %>% filter(percent>0.01)


data%>% group_by(differ ) %>%
  summarize(percent = n()/count(data) ) %>% filter(percent>0.01)


 
 